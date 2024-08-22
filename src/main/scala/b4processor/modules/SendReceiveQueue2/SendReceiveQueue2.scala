package b4processor.modules.SendReceiveQueue2

import b4processor.Parameters
import b4processor.connections.{CollectedOutput, Decoder2SendReceiveQueue, MPQ2MPQ, OutputValue}
import b4processor.utils.operations.SendReceiveOperation
import b4processor.utils.{FormalTools, Tag}
import chisel3._
import chisel3.util._

class SendReceiveQueue(implicit params: Parameters)
  extends Module
    with FormalTools {
  val io = IO(new Bundle {
    val decoders = Vec( //命令をとってくる
      params.decoderPerThread, Flipped(Decoupled(new Decoder2SendReceiveQueue)),
    )
    val collectedOutput = Flipped(new CollectedOutput()) //オペランドバイパスに使用
    val recevedData = Irrevocable(new OutputValue()) //実行結果を出力
    val requester = Vec(params.threads, new MPQ2MPQ())
    val responser = Flipped(Vec(params.threads, new MPQ2MPQ()))

    val empty = Output(Bool())
    val full = Output(Bool())
  })

  val sendDefaultEntry = SendQueueEntry.default
  val receiveDefaultEntry = ReceiveQueueEntry.default

  val sendHead = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
  val sendTail = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
  val receiveHead = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
  val receiveTail = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))

    io.empty := sendHead === sendTail || receiveHead === receiveTail
    io.full := (sendHead + 1.U === sendTail) || (receiveHead + 1.U === receiveTail)

  val sendbuffer = RegInit(
    VecInit(
      Seq.fill(math.pow(2, params.sendReceiveQueueIndexWidth).toInt)(sendDefaultEntry),
    ),
  )
  val receivebuffer = RegInit(
    VecInit(
      Seq.fill(math.pow(2, params.sendReceiveQueueIndexWidth).toInt)(receiveDefaultEntry),
    ),
  )

  for (i <- 0 until params.threads) {
    io.responser(i).response.valid := false.B
    io.responser(i).response.SendData := 0.U

    io.requester(i).request.valid := false.B
    io.requester(i).request.ThreadId := 0.U
  }

  var sendInsertIndex = sendHead
  var receiveInsertIndex = receiveHead

  var SQisFull = sendTail === sendInsertIndex + 1.U
  var RQisFull = receiveTail === receiveInsertIndex + 1.U

  /** デコードした命令をSRQに加えるかどうか確認し，s or r 命令ならばエンキュー */
  for (i <- 0 until params.decoderPerThread) {
    val decoder = io.decoders(i)
    val OpIsSend = decoder.bits.operation === SendReceiveOperation.Send
    //decoder.ready := (OpIsSend && !SQisFull) || (!OpIsSend && !RQisFull)
    decoder.ready := !SQisFull && !RQisFull
    val entryValid = decoder.ready && decoder.valid

    when(entryValid) {
      when(OpIsSend){
        sendbuffer(sendInsertIndex) := SendQueueEntry.validEntry(
          destinationTag = decoder.bits.destinationTag,
          destinationTagValid = decoder.bits.destinationTagValid,
          sendDataTag = decoder.bits.sendDataTag,
          sendData = decoder.bits.sendData,
          sendDataValid = decoder.bits.sendDataValid,
        )
      }.otherwise{
        receivebuffer(receiveInsertIndex) := ReceiveQueueEntry.validEntry(
          destinationTag = decoder.bits.destinationTag,
          sendDataTag = decoder.bits.sendDataTag,
          sendDataTagValid = decoder.bits.sendDataTagValid,
        )
      }
    }
    sendInsertIndex = sendInsertIndex + (entryValid && OpIsSend).asUInt
    receiveInsertIndex = receiveInsertIndex + (entryValid && !OpIsSend).asUInt
  }
  sendHead := sendInsertIndex
  receiveHead := receiveInsertIndex

  /** オペランドバイパスのタグが対応していた場合は，ALUを読み込む */
  for (o <- io.collectedOutput.outputs) {
    when(o.valid) {
      for (buf <- sendbuffer) {
        when(buf.sendDataTag === o.bits.tag && !buf.sendDataValid) {
          buf.sendData := o.bits.value
          buf.sendDataValid := true.B
        }
        when(buf.destinationTag.id === o.bits.tag.id && buf.sendDataTag.threadId === o.bits.tag.threadId && !buf.destinationTagValid) {
          buf.destinationTag.threadId := o.bits.value
          buf.destinationTagValid := true.B
        }
      }
      for (buf <- receivebuffer) {
        when(buf.sendDataTag.id === o.bits.tag.id && buf.destinationTag.threadId === o.bits.tag.threadId && !buf.sendDataTagValid) {
          buf.sendDataTag.threadId := o.bits.value
          buf.sendDataTagValid := true.B
        }
      }
    }
  }

  /** ReceiveQueue request 送信 */
  var receiveReq = receivebuffer(receiveTail)

  for (requester <- io.requester) {
    requester.request.valid := receiveReq.valid && receiveReq.sendDataTagValid
    requester.request.ThreadId := receiveReq.sendDataTag.threadId
  }

  /** SendQueue request 受信 -> response 送信*/
  var sendReq = sendbuffer(sendTail)
  var sendReady = sendReq.destinationTagValid && sendReq.sendDataValid

  for(responser <- io.responser){
    when(responser.request.valid && sendReady && (responser.request.ThreadId === sendReq.sendDataTag.threadId)){
      responser.response.valid := true.B
      responser.response.SendData := sendReq.sendData

      sendReq.valid := false.B
      sendReq.destinationTagValid := false.B
      sendReq.sendDataValid := false.B
      sendTail := sendTail + 1.U
    }
  }

  /** ReceiveQueue response 受信 */

  for (requester <- io.requester) {
    io.recevedData.bits.value := requester.response.SendData
    io.recevedData.bits.tag := receiveReq.destinationTag
    io.recevedData.valid := requester.response.valid
    io.recevedData.bits.isError := false.B

    when(requester.response.valid){
      receiveReq.valid := false.B
      receiveReq.sendDataTagValid := false.B
      receiveTail := receiveTail + 1.U
    }
  }
}



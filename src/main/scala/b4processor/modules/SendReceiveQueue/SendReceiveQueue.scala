package b4processor.modules.SendReceiveQueue

import b4processor.Parameters
import b4processor.connections.{CollectedOutput, Decoder2SendReceiveQueue, OutputValue, SendReceiveQueue2ReorderBuffer}
import chisel3._
import chisel3.util._
import _root_.circt.stage.ChiselStage
import b4processor.utils.{FormalTools, RVRegister, Tag}
import b4processor.utils.operations.SendReceiveOperation

class SendReceiveQueue(implicit params: Parameters)
  extends Module
    with FormalTools {
  val io = IO(new Bundle {
    val decoders = Vec( //命令をとってくる
      params.threads,
      Vec(params.decoderPerThread, Flipped(Decoupled(new Decoder2SendReceiveQueue))),
    )
    val collectedOutput = Flipped(Vec(params.threads, new CollectedOutput())) //オペランドバイパスに使用
    val recevedData = Irrevocable(new OutputValue()) //実行結果を出力

    val reorderBuffer = Flipped( //Send命令に必要なデータを取得
      Vec(
        params.threads,
        Vec(
          params.maxRegisterFileCommitCount,
          Valid(new SendReceiveQueue2ReorderBuffer),
        ),
      ),
    )

    val empty = Output(Bool())
    val full = Output(Bool())

    // LSQのエントリ数はこのままでいいのか
  })

  val sendDefaultEntry = SendQueueEntry.default
  val receiveDefaultEntry = ReceiveQueueEntry.default

  val sendHead = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
  val sendTail = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
  val receiveHead = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
  val receiveTail = RegInit(0.U(params.sendReceiveQueueIndexWidth.W))
    
  io.empty := sendHead === sendTail || receiveHead === receiveTail
  io.full := (sendHead + 1.U === sendTail) || (receiveHead + 1.U === receiveTail)

  // 出力の初期化
  io.recevedData.valid := false.B
  io.recevedData.bits := DontCare
  // end

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

  var sendInsertIndex = sendHead
  var receiveInsertIndex = receiveHead

  /** デコードした命令をSRQに加えるかどうか確認し，s or r 命令ならばエンキュー */
  for (t <- 0 until params.threads) {
    for (i <- 0 until params.decoderPerThread) {
      val decoder = io.decoders(t)(i)
      decoder.ready := (sendTail =/= sendInsertIndex + 1.U) && (receiveTail =/= receiveInsertIndex + 1.U) //not full
      val entryValid = decoder.ready && decoder.valid

      when(entryValid) {
        when(decoder.bits.operation === SendReceiveOperation.Send){
          sendbuffer(sendInsertIndex) := SendQueueEntry.validEntry(
            destinationTag = decoder.bits.destinationTag,
            destinationTagValid = decoder.bits.destinationTagValid,
            channel = decoder.bits.channel,
            channelTag = decoder.bits.channelTag,
            channelValid = decoder.bits.channelValid,
            sendDataTag = decoder.bits.sendDataTag,
            sendDataTagValid = decoder.bits.sendDataTagValid,
            sendData = decoder.bits.sendData,
            sendDataValid = decoder.bits.sendDataValid,
            opIsDone = false.B,
          )
        }.elsewhen(decoder.bits.operation === SendReceiveOperation.Receive){
          receivebuffer(receiveInsertIndex) := ReceiveQueueEntry.validEntry(
            destinationTag = decoder.bits.destinationTag,
            destinationTagValid = decoder.bits.destinationTagValid,
            channel = decoder.bits.channel,
            channelTag = decoder.bits.channelTag,
            channelValid = decoder.bits.channelValid,
            sendDataTag = decoder.bits.sendDataTag,
            sendDataTagValid = decoder.bits.sendDataTagValid,
            opIsDone = false.B,
          )
        }
      }
      sendInsertIndex = sendInsertIndex + (entryValid && (decoder.bits.operation === SendReceiveOperation.Send)).asUInt
      receiveInsertIndex = receiveInsertIndex + (entryValid && (decoder.bits.operation === SendReceiveOperation.Receive)).asUInt
    }
  }
  sendHead := sendInsertIndex
  receiveHead := receiveInsertIndex


  /** オペランドバイパスのタグorPCが対応していた場合は，ALUを読み込む */
  for (t <- 0 until params.threads) {
    for (o <- io.collectedOutput(t).outputs) {
      when(o.valid) {
        for (buf <- sendbuffer) {
          when(buf.sendDataTag === o.bits.tag && !buf.sendDataValid) {
            buf.sendData := o.bits.value
            buf.sendDataValid := true.B
          }
          when(buf.channelTag === o.bits.tag && !buf.channelValid) {
            buf.channel := o.bits.value
            buf.channelValid := true.B
          }
          when(
            buf.destinationTag.id === o.bits.tag.id &&
            buf.sendDataTag.threadId === o.bits.tag.threadId &&
            !buf.destinationTagValid
            ) {
            buf.destinationTag.threadId := o.bits.value
            buf.destinationTagValid := true.B
          }
        }
        for (buf <- receivebuffer) {
          when(buf.channelTag === o.bits.tag && !buf.channelValid) {
            buf.channel := o.bits.value
            buf.channelValid := true.B
          }
          when(
            buf.sendDataTag.id === o.bits.tag.id &&
            buf.destinationTag.threadId === o.bits.tag.threadId &&
            !buf.sendDataTagValid
          ) {
            buf.sendDataTag.threadId := o.bits.value
            buf.sendDataTagValid := true.B
          }
        }
      }
    }
  }

  for (t <- 0 until params.threads) {
    for (rb <- io.reorderBuffer(t)) {
      when(rb.valid) {
        for (buf <- sendbuffer) {
          when(buf.valid && (rb.bits.destinationTag === buf.destinationTag)) {
            buf.readyReorderSign := true.B
          }
        }
      }
    }
  }

  /** send-receive動作 */
  for (sendBuf <- sendbuffer) {
    when(
      (sendHead =/= sendTail) && (receiveHead =/= receiveTail) &&
       sendBuf.valid && sendBuf.sendDataValid && sendBuf.channelValid &&
       sendBuf.destinationTagValid
    ) { //send命令が実行可能な時
      for (receiveBuf <- receivebuffer) {
        when (
          receiveBuf.valid && //receive命令があるとき
          sendBuf.destinationTag.threadId === receiveBuf.destinationTag.threadId &&
          sendBuf.sendDataTag.threadId === receiveBuf.sendDataTag.threadId &&
          sendBuf.channel === receiveBuf.channel &&
          receiveBuf.channelValid && receiveBuf.sendDataTagValid
        ) { //send命令とreceive命令が対応しているとき
          io.recevedData.valid := true.B
          io.recevedData.bits.value := sendBuf.sendData
          io.recevedData.bits.tag := receiveBuf.destinationTag
          io.recevedData.bits.isError := false.B
          sendBuf.opIsDone := true.B
          receiveBuf.opIsDone := true.B
          sendBuf.valid := false.B
          receiveBuf.valid := false.B
        }
      }
    }
  }
  when(sendbuffer(sendTail).opIsDone && sendHead =/= sendTail) {
    sendTail := sendTail + 1.U
  }

  when(receivebuffer(receiveTail).opIsDone && receiveHead =/= receiveTail) {
    receiveTail := receiveTail + 1.U
  }
}

object SendReceiveQueue extends App {
  implicit val params =
    Parameters(
      maxRegisterFileCommitCount = 2,
      tagWidth = 4,
      loadStoreQueueIndexWidth = 3,
    )

  //  println(ChiselStage.emitCHIRRTL(new LoadStoreQueue))
  //  println(ChiselStage.emitFIRRTLDialect(new LoadStoreQueue))
  //  println(ChiselStage.emitHWDialect(new LoadStoreQueue))

  ChiselStage.emitSystemVerilogFile(
    new SendReceiveQueue,
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb,verifLabels",
      //      "--emit-chisel-asserts-as-sva",
      "--dedup",
      "--mlir-pass-statistics",
    ),
  )

  var s = ChiselStage.emitSystemVerilog(
    new SendReceiveQueue,
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb,verifLabels",
      //      "--emit-chisel-asserts-as-sva",
      "--dedup",
    ),
  )
}

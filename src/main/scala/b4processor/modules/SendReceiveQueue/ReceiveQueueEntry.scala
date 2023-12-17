package b4processor.modules.SendReceiveQueue

import b4processor.Parameters
import b4processor.utils.operations.SendReceiveOperation
import b4processor.utils.{RVRegister, Tag}
import chisel3._

/** LSQのエントリ
 *
 * @param params
 *   パラメータ
 */
class ReceiveQueueEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  /** 命令がリオーダバッファでコミットされたか */
  val readyReorderSign = Bool()

  /** 命令自体を識別するためのタグ(Destination Tag) */
  val destinationTag = new Tag
  val destinationTagValid = Bool()

  /** channel */
  val channel = UInt(8.W)
  val channelTag = new Tag()
  val channelValid = Bool()

  /** Sendに使用するデータが格納されるタグ(SourceRegister2 Tag) */
  val sendDataTag = new Tag
  val sendDataTagValid = Bool()

  /** Send-Receiveを実行した */
  val opIsDone = Bool()
}

object ReceiveQueueEntry {
  def validEntry(
                  destinationTag: Tag,
                  destinationTagValid: Bool,
                  channel: UInt,
                  channelTag: Tag,
                  channelValid: Bool,
                  sendDataTag: Tag,
                  sendDataTagValid: Bool,
                  opIsDone: Bool,
                )(implicit params: Parameters): ReceiveQueueEntry = {
    val entry = ReceiveQueueEntry.default
    entry.valid := true.B

    entry.destinationTag := destinationTag
    entry.destinationTagValid := destinationTagValid

    entry.channel := channel
    entry.channelTag := channelTag
    entry.channelValid := channelValid

    entry.sendDataTag := sendDataTag
    entry.sendDataTagValid := sendDataTagValid

    entry.opIsDone := opIsDone

    entry
  }

  def default(implicit params: Parameters): ReceiveQueueEntry = {
    val entry = Wire(new ReceiveQueueEntry())
    entry.valid := false.B
    entry.readyReorderSign := false.B

    entry.destinationTag := Tag(0, 0)
    entry.destinationTagValid := false.B

    entry.channel := 0.U
    entry.channelTag := Tag(0, 0)
    entry.channelValid := false.B

    entry.sendDataTag := Tag(0, 0)
    entry.sendDataTagValid := false.B

    entry.opIsDone := false.B

    entry
  }
}
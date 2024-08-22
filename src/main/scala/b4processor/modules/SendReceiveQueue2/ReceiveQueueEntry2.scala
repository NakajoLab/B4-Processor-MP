package b4processor.modules.SendReceiveQueue2

import b4processor.Parameters
import b4processor.utils.Tag
import chisel3._

/** LSQのエントリ
 *
 * @param params
 *   パラメータ
 */
class ReceiveQueueEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  /** 命令自体を識別するためのタグ(Destination Tag) */
  val destinationTag = new Tag

  /** Sendに使用するデータが格納されるタグ(SourceRegister2 Tag) */
  val sendDataTag = new Tag
  val sendDataTagValid = Bool()
}

object ReceiveQueueEntry {
  def validEntry(
                  destinationTag: Tag,
                  sendDataTag: Tag,
                  sendDataTagValid: Bool,
                )(implicit params: Parameters): ReceiveQueueEntry = {
    val entry = ReceiveQueueEntry.default
    entry.valid := true.B

    entry.destinationTag := destinationTag

    entry.sendDataTag := sendDataTag
    entry.sendDataTagValid := sendDataTagValid

    entry
  }

  def default(implicit params: Parameters): ReceiveQueueEntry = {
    val entry = Wire(new ReceiveQueueEntry())
    entry.valid := false.B

    entry.destinationTag := Tag(0, 0)

    entry.sendDataTag := Tag(0, 0)
    entry.sendDataTagValid := false.B

    entry
  }
}
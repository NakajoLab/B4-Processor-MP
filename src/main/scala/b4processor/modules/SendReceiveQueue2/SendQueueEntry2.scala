package b4processor.modules.SendReceiveQueue2

import b4processor.Parameters
import b4processor.utils.Tag
import chisel3._

/** LSQのエントリ
 *
 * @param params
 *   パラメータ
 */
class SendQueueEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  /** 命令がリオーダバッファでコミットされたか */
  val readyReorderSign = Bool()

  /** 命令自体を識別するためのタグ(Destination Tag) */
  val destinationTag = new Tag
  val destinationTagValid = Bool()

  /** Sendに使用するデータが格納されるタグ(SourceRegister2 Tag) */
  val sendDataTag = new Tag
  /** Sendデータ */
  val sendData = UInt(64.W)
  val sendDataValid = Bool()
}

object SendQueueEntry {
  def validEntry(
                  destinationTag: Tag,
                  destinationTagValid: Bool,
                  sendDataTag: Tag,
                  sendData: UInt,
                  sendDataValid: Bool,
                )(implicit params: Parameters): SendQueueEntry = {
    val entry = SendQueueEntry.default
    entry.valid := true.B

    entry.destinationTag := destinationTag
    entry.destinationTagValid := destinationTagValid

    entry.sendDataTag := sendDataTag
    entry.sendData := sendData
    entry.sendDataValid := sendDataValid

    entry
  }

  def default(implicit params: Parameters): SendQueueEntry = {
    val entry = Wire(new SendQueueEntry())
    entry.valid := false.B
    entry.readyReorderSign := false.B

    entry.destinationTag := Tag(0, 0)
    entry.destinationTagValid := false.B

    entry.sendDataTag := Tag(0, 0)
    entry.sendData := 0.U
    entry.sendDataValid := false.B

    entry
  }
}

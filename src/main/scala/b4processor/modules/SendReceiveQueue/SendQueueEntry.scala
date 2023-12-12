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
class SendQueueEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  /** 命令がリオーダバッファでコミットされたか */
  val readyReorderSign = Bool()

  /** 命令自体を識別するためのタグ(Destination Tag) */
  val destinationTag = new Tag

  /** 送信先のレジスタ */
  val destinationRegister = new RVRegister

  /** Sendに使用するデータが格納されるタグ(SourceRegister2 Tag) */
  val sendDataTag = new Tag

  /** Sendデータ */
  val sendData = UInt(64.W)

  /** Sendデータが有効である */
  val sendDataValid = Bool()

  /** Send-Receiveを実行した */
  val opIsDone = Bool()
}

object SendQueueEntry {
  def validEntry(
                  destinationTag: Tag,
                  destinationRegister: RVRegister,
                  sendDataTag: Tag,
                  sendData: UInt,
                  sendDataValid: Bool,
                  opIsDone: Bool,
                )(implicit params: Parameters): SendQueueEntry = {
    val entry = SendQueueEntry.default
    entry.valid := true.B

    entry.destinationTag := destinationTag
    entry.destinationRegister := destinationRegister

    entry.sendDataTag := sendDataTag
    entry.sendData := sendData
    entry.sendDataValid := sendDataValid

    entry.opIsDone := opIsDone

    entry
  }

  def default(implicit params: Parameters): SendQueueEntry = {
    val entry = Wire(new SendQueueEntry())
    entry.valid := false.B
    entry.readyReorderSign := false.B

    entry.destinationTag := Tag(0, 0)
    entry.destinationRegister := DontCare

    entry.sendDataTag := Tag(0, 0)
    entry.sendData := 0.U
    entry.sendDataValid := false.B

    entry.opIsDone := false.B

    entry
  }
}

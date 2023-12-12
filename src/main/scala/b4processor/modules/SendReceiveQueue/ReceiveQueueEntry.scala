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

  /** 受信先のレジスタ */
  val destinationRegister = new RVRegister

  /** Sendに使用するデータが格納されるタグ(SourceRegister2 Tag) */
  val sendDataTag = new Tag

  /** Send-Receiveを実行した */
  val opIsDone = Bool()
}

object ReceiveQueueEntry {
  def validEntry(
                  destinationTag: Tag,
                  destinationRegister: RVRegister,
                  sendDataTag: Tag,
                  opIsDone: Bool,
                )(implicit params: Parameters): ReceiveQueueEntry = {
    val entry = ReceiveQueueEntry.default
    entry.valid := true.B

    entry.destinationTag := destinationTag
    entry.destinationRegister := destinationRegister

    entry.sendDataTag := sendDataTag

    entry.opIsDone := opIsDone

    entry
  }

  def default(implicit params: Parameters): ReceiveQueueEntry = {
    val entry = Wire(new ReceiveQueueEntry())
    entry.valid := false.B
    entry.readyReorderSign := false.B

    entry.destinationTag := Tag(0, 0)
    entry.destinationRegister := DontCare

    entry.sendDataTag := Tag(0, 0)

    entry.opIsDone := false.B

    entry
  }
}
package b4processor.modules.SendReceiveQueue

import b4processor.Parameters
import b4processor.utils.operations.SendReceiveOperation
import b4processor.utils.{RVRegister, Tag}
import chisel3._

/** SRQのエントリ
 *
 * @param params
 *   パラメータ
 */
class SendReceiveQueueEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  /** 命令がリオーダバッファでコミットされたか */
  val readyReorderSign = Bool()

  /** 命令の情報 */
  val operation = SendReceiveOperation()

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

object SendReceiveQueueEntry {
  def validEntry(
                  operation: SendReceiveOperation.Type,
                  destinationTag: Tag,
                  destinationRegister: RVRegister,
                  sendDataTag: Tag,
                  sendData: UInt,
                  sendDataValid: Bool,
                  opIsDone: Bool,
                )(implicit params: Parameters): SendReceiveQueueEntry = {
    val entry = SendReceiveQueueEntry.default
    entry.valid := true.B

    entry.operation := operation

    entry.destinationTag := destinationTag
    entry.destinationRegister := destinationRegister

    entry.sendDataTag := sendDataTag
    entry.sendData := sendData
    entry.sendDataValid := sendDataValid

    entry.opIsDone := opIsDone

    entry
  }

  def default(implicit params: Parameters): SendReceiveQueueEntry = {
    val entry = Wire(new SendReceiveQueueEntry())
    entry.valid := false.B
    entry.readyReorderSign := false.B

    entry.operation := DontCare

    entry.destinationTag := Tag(0, 0)
    entry.destinationRegister := DontCare

    entry.sendDataTag := Tag(0, 0)
    entry.sendData := 0.U
    entry.sendDataValid := false.B

    entry.opIsDone := false.B

    entry
  }
}

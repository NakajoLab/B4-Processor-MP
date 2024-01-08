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
class OutputBufferEntry(implicit params: Parameters) extends Bundle {

  /** エントリが有効である */
  val valid = Bool()

  val value = UInt(2.W) //本来は64bit, シミュレーション高速化のため小さい値にしている
  val destinationTag = new Tag

}

object OutputBufferEntry {
  def validEntry(
                  destinationTag: Tag,
                  value: UInt,
                )(implicit params: Parameters): OutputBufferEntry = {
    val entry = OutputBufferEntry.default
    entry.valid := true.B

    entry.destinationTag := destinationTag
    entry.value := value

    entry
  }

  def default(implicit params: Parameters): OutputBufferEntry = {
    val entry = Wire(new OutputBufferEntry())
    entry.valid := false.B

    entry.destinationTag := Tag(0, 0)
    entry.value := 0.U

    entry
  }
}
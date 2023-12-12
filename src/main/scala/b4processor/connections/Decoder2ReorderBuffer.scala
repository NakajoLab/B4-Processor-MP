package b4processor.connections

import b4processor.Parameters
import b4processor.utils.{ForPext, RVRegister, Tag}
import chisel3._
import chisel3.util._

/** デコーダとリオーダバッファをつなぐ
  *
  * @param params
  *   パラメータ
  */
class Decoder2ReorderBuffer(implicit params: Parameters) extends Bundle {
  val sources = Vec(3, new SourceRegister())
  val destination = new DestinationRegister()
  val programCounter = Output(UInt(64.W))
  val isDecodeError = Output(Bool())
  // 全体をDecoupledにするとsource1などがすべてOutputにってしまって、間違っているのでこちらに書く
  val ready = Input(Bool())
  val valid = Output(Bool())
  val operationSendReceive = Output(Bool())

  class SourceRegister extends Bundle {
    val sourceRegister = Output(new RVRegister())
    val matchingTag = Flipped(Valid(new Tag))
    val value = Flipped(Valid(UInt(64.W)))
  }

  class DestinationRegister extends Bundle {
    val destinationRegister = Output(new RVRegister())
    val destinationTag = Input(new Tag)
    val operationInorder = Output(Bool())
  }
}

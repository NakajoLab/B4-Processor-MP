package b4processor.connections

import b4processor.Parameters
import b4processor.utils.operations.SendReceiveOperation
import b4processor.utils.{Tag, RVRegister}
import chisel3._
import chisel3.util._

/** デコーダとSRQをつなぐ
 *
 * @param params
 *   パラメータ
 */
class Decoder2SendReceiveQueue(implicit params: Parameters) extends Bundle {

  /** Send命令とReceive命令を区別 */
  val operation = SendReceiveOperation()

  /** 送信先のレジスタ番号 & ThreadIDを送る */
  val destinationReg = new RVRegister //rs1: threadid rd: register num
  val destinationTag = new Tag

  /** sendデータ */
  val sendData = UInt(64.W)                       //rs2
  val sendDataTag = new Tag

  /** sendデータの値が有効か */
  val sendDataValid = Bool()


  /** 受信先のレジスタ番号とタグ、スレッドIDを送る */
  //val receiveDestination = new DestinationRegister() //rs1: threadId rd: register num

  /** receiveデータの値が有効か */
  //val receiveDataValid = Bool()

  class DestinationRegister extends Bundle {
    val destinationRegister = Output(new RVRegister())
    val destinationTag = Input(new Tag)
    val operationInorder = Output(Bool())
  }
}
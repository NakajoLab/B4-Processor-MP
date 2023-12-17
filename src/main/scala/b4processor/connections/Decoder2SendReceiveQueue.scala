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

  /** channel & ThreadIDを送る */
  val channel = UInt(8.W)
  val channelTag = new Tag
  val channelValid = Bool()

  val destinationTag = new Tag
  val destinationTagValid = Bool()

  /** sendデータ */
  val sendData = UInt(64.W)                       //rs2
  val sendDataTag = new Tag

  /** sendデータの値が有効か */
  val sendDataValid = Bool()
  val sendDataTagValid = Bool()


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
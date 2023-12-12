package b4processor.modules.reorderbuffer

import b4processor.utils.RVRegister
import b4processor.utils.RVRegister.AddRegConstructor
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor

class ReorderBufferEntry extends Bundle {

  /** デスティネーションレジスタ */
  val destinationRegister = new RVRegister()

  /** 命令の処理が完了した（コミットできる） */
  val valueReady = Bool()

  /** 通常時：実行結果の値 例外時：エラーの種類 */
  val value = UInt(64.W)

  /** プログラムカウンタ */
  val programCounter = UInt(64.W)

  val operationInorder = Bool()

  /** isError */
  val isError = Bool()

  /** isSendReceiveOp */
  val isSendReceiveOp = Bool() //added by akamatsu
}

object ReorderBufferEntry {
  def default: ReorderBufferEntry =
    (new ReorderBufferEntry).Lit(
      _.valueReady -> false.B,
      _.destinationRegister -> 0.reg,
      _.isError -> false.B,
      _.value -> 0.U,
      _.programCounter -> 0.U,
      _.operationInorder -> false.B,
      _.isSendReceiveOp -> false.B, //added by akamatsu
    )
}

package b4processor.modules.cache

import b4processor.Parameters
import b4processor.structures.memoryAccess.MemoryAccessInfo
import chisel3._

class DataMemoryBufferEntry(implicit params: Parameters) extends Bundle {

  /** アドレス値 */
  val address = SInt(64.W)

  /** 命令を識別するためのタグ(Destination Tag) */
  val tag = UInt(params.tagWidth.W)

  /** ストアデータ */
  val data = UInt(64.W)

  /** メモリアクセスの情報 */
  val accessInfo = new MemoryAccessInfo()
}

object DataMemoryBufferEntry {
  def validEntry(
    address: SInt,
    tag: UInt,
    data: UInt,
    accessInfo: MemoryAccessInfo
  )(implicit params: Parameters): DataMemoryBufferEntry = {
    val entry = DataMemoryBufferEntry.default
    entry.address := address
    entry.tag := tag
    entry.data := data
    entry.accessInfo := accessInfo

    entry
  }

  def default(implicit params: Parameters): DataMemoryBufferEntry = {
    val entry = Wire(new DataMemoryBufferEntry)
    entry := DontCare

    entry
  }
}

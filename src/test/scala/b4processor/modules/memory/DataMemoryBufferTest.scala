package b4processor.modules.memory

import b4processor.Parameters
import b4processor.modules.cache.DataMemoryBuffer
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import b4processor.structures.memoryAccess.MemoryAccessType._
import b4processor.structures.memoryAccess.MemoryAccessWidth
import b4processor.utils.operations.{LoadStoreOperation, LoadStoreWidth}
import b4processor.utils.Tag

class DataMemoryBufferTestWrapper(implicit params: Parameters)
    extends DataMemoryBuffer {}

class DataMemoryBufferTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Data Memory Buffer"

  implicit val params = Parameters(maxRegisterFileCommitCount = 2, tagWidth = 5)

  it should "enqueue and dequeue" in {
    test(new DataMemoryBufferTestWrapper)
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        c.io.dataIn(0).valid.poke(true)
        c.io.dataIn(0).bits.address.poke(4)
        c.io.dataIn(0).bits.tag.poke(Tag(0, 10))
        c.io.dataIn(0).bits.data.poke(123)
        c.io.dataIn(0).bits.operation.poke(LoadStoreOperation.Load)
        c.io.dataIn(0).bits.operationWidth.poke(LoadStoreWidth.Byte)
        c.io.memory.read.request.ready.poke(true)
        c.io.memory.write.request.ready.poke(true)

        c.io.dataIn(1).valid.poke(true)
        c.io.dataIn(1).bits.address.poke(8)
        c.io.dataIn(1).bits.tag.poke(Tag(0, 11))
        c.io.dataIn(1).bits.data.poke(1234)
        c.io.dataIn(1).bits.operation.poke(LoadStoreOperation.Store)
        c.io.dataIn(1).bits.operationWidth.poke(LoadStoreWidth.Word)

        c.io.memory.read.request.ready.poke(true.B)
        c.io.memory.write.request.ready.poke(true.B)

        if (c.io.dataIn(0).ready.peekBoolean()) {
          c.clock.step(1)
          c.io.dataIn(0).valid.poke(false)
        } else if (c.io.dataIn(1).ready.peekBoolean()) {
          c.clock.step(1)
          c.io.dataIn(1).valid.poke(false)
        }

        // RRArbterで順序が入れ替わってしまっている。
        c.io.memory.read.request.valid.expect(false)
        c.io.memory.write.request.valid.expect(true)
        c.io.memory.write.request.bits.address.expect(8)
        c.io.memory.write.request.bits.outputTag.id.expect(11)
        c.io.memory.write.requestData.bits.data.expect(1234)
        c.clock.step(1)

        c.io.dataIn(0).valid.poke(false)
        c.io.dataIn(1).valid.poke(false)

        c.io.memory.read.request.valid.expect(true)
        c.io.memory.write.request.valid.expect(false)
        c.io.memory.read.request.bits.address.expect(4)
        c.io.memory.read.request.bits.outputTag.id.expect(10)
        c.clock.step(1)
      }
  }
}

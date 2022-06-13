package b4processor.modules.memory

import b4processor.Parameters
import b4processor.utils.DataMemoryValue
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DataMemoryTestWrapper(implicit params: Parameters) extends DataMemory {}

class DataMemoryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Data Memory"

  implicit val params = Parameters()

  it should "store and load a value" in {
    test(new DataMemoryTestWrapper) { c =>
      // 0アドレスへのストア
      c.io.dataIn.bits.address.poke(0)
      c.io.dataIn.bits.data.poke(123)
      c.io.dataIn.bits.tag.poke(10)
      c.io.dataIn.bits.function3.poke("b011".U)
      c.io.dataIn.bits.opcode.poke("b0100011".U)
      c.io.dataIn.valid.poke(true)

      c.clock.step(1)
      // 別アドレスへのストア
      c.io.dataIn.bits.address.poke(40)
      c.io.dataIn.bits.data.poke(1000)
      c.io.dataIn.bits.tag.poke(30)
      c.io.dataIn.bits.function3.poke("b011".U)
      c.io.dataIn.bits.opcode.poke("b0100011".U)
      c.io.dataIn.valid.poke(true)

      c.clock.step(2)
      // 0アドレスからのロード
      c.io.dataIn.bits.address.poke(0)
      c.io.dataIn.bits.data.poke(0)
      c.io.dataIn.bits.tag.poke(20)
      c.io.dataIn.bits.function3.poke("b011".U)
      c.io.dataIn.bits.opcode.poke("b0000011".U)
      c.io.dataOut.valid.expect(true)
      c.io.dataOut.bits.data.expect(123)
      c.io.dataOut.bits.tag.expect(20)
    }
  }

  it should "load a byte value" in {
    test(new DataMemoryTestWrapper) { c =>
      // 0アドレスへのストア
      c.io.dataIn.bits.address.poke(0)
      c.io.dataIn.bits.data.poke("b10000000011".U)
      c.io.dataIn.bits.tag.poke(10)
      c.io.dataIn.bits.function3.poke("b011".U)
      c.io.dataIn.bits.opcode.poke("b0100011".U)
      c.io.dataIn.valid.poke(true)

      c.clock.step(2)
      // 0アドレスからのロード
      c.io.dataIn.bits.address.poke(0)
      c.io.dataIn.bits.data.poke(0)
      c.io.dataIn.bits.tag.poke(20)
      c.io.dataIn.bits.function3.poke("b000".U)
      c.io.dataIn.bits.opcode.poke("b0000011".U)
      c.io.dataOut.valid.expect(true)
      c.io.dataOut.bits.data.expect(3)
      c.io.dataOut.bits.tag.expect(20)
    }
  }
}
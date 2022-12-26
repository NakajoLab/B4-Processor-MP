package b4processor.modules.decoder

import b4processor.Parameters
import b4processor.common.OpcodeFormat
import b4processor.common.OpcodeFormat._
import b4processor.utils.Tag
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValueSelector1Wrapper(implicit params: Parameters)
    extends ValueSelector1 {

  /** 初期化
    *
    * @param sourceTag
    *   ソースタグ
    * @param registerFileValue
    *   レジスタファイルから渡される値
    * @param reorderBufferValue
    *   リオーダバッファからの値
    * @param aluBypassValue
    *   ALUからバイパスされてきた値。タプルの1つめの値がdestination tag、2つめがvalue。
    */
  def initialize(
    sourceTag: Option[Int] = None,
    registerFileValue: Int = 0,
    reorderBufferValue: Option[Int] = None,
    aluBypassValue: Option[(Int, Int)] = None,
    opcodeFormat: OpcodeFormat.Type = R,
    immediate: Int = 0
  ): Unit = {
    this.io.outputCollector.outputs.valid
      .poke(aluBypassValue.isDefined.B)
    this.io.outputCollector.outputs.bits.tag
      .poke(Tag(0, aluBypassValue.getOrElse((0, 0))._1))
    this.io.outputCollector.outputs.bits.value
      .poke(aluBypassValue.getOrElse((0, 0))._2.U)

    this.io.reorderBufferValue.valid.poke(reorderBufferValue.isDefined)
    this.io.reorderBufferValue.bits.poke(reorderBufferValue.getOrElse(0))
    this.io.registerFileValue.poke(registerFileValue)
    this.io.sourceTag.valid.poke(sourceTag.isDefined)
    this.io.sourceTag.tag.poke(Tag(0, sourceTag.getOrElse(0)))
    this.io.opcodeFormat.poke(opcodeFormat)
    this.io.immediateValue.poke(immediate)
  }

  def expectValue(value: Option[Int]): Unit = {
    this.io.value.valid.expect(value.isDefined.B, "valueがありませんです")
    if (value.isDefined) {
      this.io.value.bits.expect(value.get.U, "valueの値が間違っています")
    }
  }
}

class ValueSelector1Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ValueSelector1"

  implicit val defaultParams = Parameters(threads = 1, decoderPerThread = 1)

  it should "use the register file" in {
    test(new ValueSelector1Wrapper) { c =>
      c.initialize(registerFileValue = 5)
      c.expectValue(Some(5))
    }
  }

  it should "use the reorder buffer" in {
    test(new ValueSelector1Wrapper) { c =>
      c.initialize(
        sourceTag = Some(3),
        registerFileValue = 5,
        reorderBufferValue = Some(6)
      )
      c.expectValue(Some(6))
    }
  }

  it should "use the alu bypass" in {
    test(
      new ValueSelector1Wrapper()(defaultParams.copy(decoderPerThread = 1))
    ) { c =>
      c.initialize(
        sourceTag = Some(3),
        registerFileValue = 5,
        aluBypassValue = Some((3, 12))
      )
      c.expectValue(Some(12))
    }
  }

  it should "use multiple alu bypasses" in {
    test(
      new ValueSelector1Wrapper()(defaultParams.copy(decoderPerThread = 4))
    ) { c =>
      c.initialize(
        sourceTag = Some(3),
        registerFileValue = 5,
        aluBypassValue = Some(3, 12)
      )
      c.expectValue(Some(12))
    }
  }

  it should "use immediate" in {
    test(new ValueSelector1Wrapper) { c =>
      c.initialize(immediate = 9, opcodeFormat = U)
      c.expectValue(Some(9))
    }
  }

  it should "not have any value" in {
    test(new ValueSelector1Wrapper) { c =>
      c.initialize(sourceTag = Some(3), registerFileValue = 5)
      c.expectValue(None)
    }
  }
}

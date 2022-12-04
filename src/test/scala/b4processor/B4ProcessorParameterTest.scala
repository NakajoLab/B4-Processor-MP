package b4processor

import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class B4ProcessorParameterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "B4Processor with many parameters"
  implicit val defaultParams = Parameters(debug = true)

  for (threads <- Seq(1, 2))
    for (decoderPerThread <- Seq(1, 3))
      for (maxCommitCount <- Seq(1, 3))
        for (tagWidth <- Seq(2, 5))
          for (lsqWidth <- Seq(2, 5))
            it should s"run fibonacci_c threads=${threads} decoders=${decoderPerThread} maxCommitCount=${maxCommitCount} tagWidth=${tagWidth} lsqWidth=${lsqWidth}" in {
              test(
                new B4ProcessorWithMemory()(
                  defaultParams.copy(
                    threads = threads,
                    decoderPerThread = decoderPerThread,
                    tagWidth = tagWidth,
                    maxRegisterFileCommitCount = maxCommitCount,
                    loadStoreQueueIndexWidth = lsqWidth
                  )
                )
              )
                .withAnnotations(
                  Seq(
                    WriteVcdAnnotation,
                    VerilatorBackendAnnotation,
                    CachingAnnotation
                  )
                ) { c =>
                  c.initialize("riscv-sample-programs/fibonacci_c/fibonacci_c")
                  c.checkForRegister(3, 21, 1500)
                }
            }
}

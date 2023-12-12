package b4processor.z_B4ProcessorTests

import b4processor.Parameters
import b4processor.utils.B4ProcessorWithMemory
import chiseltest._
import chiseltest.internal.CachingAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class z10_B4ProcessorProgramTest
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior of "B4Processor test programs"
  // デバッグに時間がかかりすぎるのでパラメータを少し下げる。
  implicit val defaultParams =
    Parameters(
      debug = true,
      tagWidth = 4,
      threads = 1,
      decoderPerThread = 1,
//      enablePExt = true
    )
  val backendAnnotation = IcarusBackendAnnotation
  val WriteWaveformAnnotation = WriteFstAnnotation

  // sendプログラムが実行できる
  it should "execute send with 2 parallel thread" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 2, decoderPerThread = 1),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/send")
        c.checkForRegister(13, 20, 200)
      }
  }
  // branchプログラムが実行できる
  it should "execute branch with no parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 1),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/branch")
        c.checkForRegister(13, 20, 200)
      }
  }
  // branchプログラムが実行できる
  it should "execute branch with 2 parallel thread" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 2, decoderPerThread = 1),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/branch")
        c.checkForRegister(13, 20, 200, 0)
        c.checkForRegister(13, 20, 200, 1)
      }
  }

  // フィボナッチ数列の計算が同時発行数1でできる
  it should "execute fibonacci with no parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 1),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/fibonacci")
        c.checkForRegister(6, 55, 400)
      }
  }

  // フィボナッチ数列の計算が同時発行数2でできる
  it should "execute fibonacci with 2 parallel" in {
    test(new B4ProcessorWithMemory())
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/fibonacci")
        c.checkForRegister(6, 55, 400)
      }
  }

  // フィボナッチ数列の計算が同時発行数4でできる
  it should "execute fibonacci with 4 parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 4),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/fibonacci")
        c.checkForRegister(6, 55, 200)
      }
  }

  // call(JALRを使った関数呼び出し)とret(JALRを使った関数からのリターン)がうまく実行できる
  it should "execute call_ret with 2 parallel" in {
    test(new B4ProcessorWithMemory())
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/call_ret")
        c.checkForRegister(5, 1, 100)
        c.checkForRegister(6, 2, 20)
        c.checkForRegister(7, 3, 20)
      }
  }

  // 並列実行できそうな大量のadd命令を同時発行数1で試す
  it should "execute many_add with no parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 1),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add")
        c.checkForRegister(1, 8, 100)
      }
  }

  // 並列実行できそうな大量のadd命令を同時発行数1で試す
  it should "execute many_add with no parallel 2 thread" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 2, decoderPerThread = 1),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add")
        c.checkForRegister(1, 8, 200, 0)
        c.checkForRegister(1, 8, 70, 1)
      }
  }

  // 並列実行できそうな大量のadd命令を同時発行数2で試す
  it should "execute many_add with 2 parallel" in {
    test(new B4ProcessorWithMemory()(defaultParams.copy(fetchWidth = 8)))
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add")
        c.checkForRegister(1, 8, 100)
      }
  }

  // 並列実行できそうな大量のadd命令を同時発行数4で試す
  it should "execute many_add with 4 parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 4, fetchWidth = 8),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add")
        c.checkForRegister(1, 8, 70)
      }
  }

  // タグ幅をとても小さくする（すべてのデコーダが使えない）ような状況でもうまく動作する
  it should "execute many_add with 4 parallel with very low tag width" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams
          .copy(threads = 1, decoderPerThread = 4, fetchWidth = 8, tagWidth = 2),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add")
        c.checkForRegister(1, 8, 70)
      }
  }

  // 並列実行できそうな大量のadd命令を同時発行数8で試す
  it should "execute many_add with 8 parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 8,
          fetchWidth = 8,
          executors = 4,
          maxRegisterFileCommitCount = 10,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add")
        c.checkForRegister(1, 8, 70)
      }
  }

  // アウトオブオーダでできそうな命令を同時発行数4で試す
  it should "execute out_of_order with 4 parallel" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams
          .copy(
            threads = 1,
            decoderPerThread = 4,
            fetchWidth = 8,
            maxRegisterFileCommitCount = 8,
          ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_add_out_of_order")
        c.clock.step(60)
        c.io.registerFileContents.get(0)(1).expect(1)
        c.io.registerFileContents.get(0)(2).expect(2)
        c.io.registerFileContents.get(0)(3).expect(3)
        c.io.registerFileContents.get(0)(4).expect(4)
        c.io.registerFileContents.get(0)(5).expect(1)
        c.io.registerFileContents.get(0)(6).expect(2)
        c.io.registerFileContents.get(0)(7).expect(3)
        c.io.registerFileContents.get(0)(8).expect(4)
        c.io.registerFileContents.get(0)(9).expect(1)
        c.io.registerFileContents.get(0)(10).expect(2)
        c.io.registerFileContents.get(0)(11).expect(3)
        c.io.registerFileContents.get(0)(12).expect(4)
      }
  }

//  // 単純な値をストアしてロードするプログラム
//  it should "run load_store" in {
//    test(
//      new B4ProcessorWithMemory()(
//        defaultParams.copy(
//          threads = 1,
//          decoderPerThread = 1,
//          maxRegisterFileCommitCount = 1
//        )
//      )
//    )
//      .withAnnotations(
//        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation)
//      ) { c => }
//  }

  // 単純な値をストアしてロードするプログラム同時発行数2
  it should "run load_store with 2 parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(threads = 1, decoderPerThread = 2),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/load_store")
        c.checkForRegister(3, 10, 200)
        c.io.registerFileContents
          .get(0)(1)
          .expect(defaultParams.instructionStart + 0x58 + 16)
        c.io.registerFileContents.get(0)(2).expect(10)
        c.io.registerFileContents.get(0)(3).expect(10)
      }
  }

  it should "run fibonacci_c" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 1,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/fibonacci_c")
        c.clock.step(500)
        c.checkForRegisterChange(3, 1298777728820984005L, 10000)
      }
  }

  it should "run fibonacci_c with 2 parallel" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 2,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 3,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/fibonacci_c")
        c.checkForRegisterChange(3, 1298777728820984005L, 10000)
      }
  }

  it should "run load_plus_arithmetic" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/load_plus_arithmetic")
        c.checkForRegister(2, 20, 50)
        c.checkForRegister(3, 1, 50)
      }
  }

  it should "run load_after_store" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/load_after_store")
        c.checkForRegister(3, 10, 100)
      }
  }

  it should "run enter_c" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/enter_c")
        c.checkForRegister(3, 5, 100)

      }
  }

  it should "run calculation_c" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/calculation_c")
        c.checkForRegister(3, 18, 400)

      }
  }

  it should "run loop_c" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 1,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/loop_c")
        c.checkForRegister(3, 30, 1000)

      }
  }

  it should "run loop_c with 4 parallel" in {
    test(
      new B4ProcessorWithMemory()(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/loop_c")
        c.checkForRegister(3, 30, 2000)

      }
  }

  it should "run many_load_store" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 1,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_load_store")
        c.checkForRegister(2, 36)

      }
  }

  it should "run many_load_store with 4 parallel" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/many_load_store")
        c.checkForRegister(2, 36, 100)

      }
  }

  it should "run load_store_cross" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 1,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/load_store_cross")
        c.checkForRegister(2, 101, 100)
        c.checkForRegister(3, 201, 100)

      }
  }

  it should "run load_store_cross with 4 parallel" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 4,
          maxRegisterFileCommitCount = 4,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/load_store_cross")
        c.checkForRegister(2, 101, 100)
        c.checkForRegister(3, 201, 100)
      }
  }

  it should "run csrtest" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 2,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/csrtest")
        c.checkForRegister(17, 10)
        c.checkForRegister(17, 20)
        c.checkForRegister(17, 30)
        c.clock.step(20)
      }
  }

  it should "run illegal_inst" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 2,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/illegal_inst")
        c.checkForRegister(10, 10)
        c.checkForRegister(10, 40)
        c.clock.step(20)
      }
  }

  it should "run io_test" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 2,
          loadStoreQueueIndexWidth = 2,
        ),
      ),
    )
      .withAnnotations(
        Seq(WriteWaveformAnnotation, backendAnnotation, CachingAnnotation),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/io_test")
        for (p <- "ABCDEFGHIJK")
          c.checkForOutput(p)
        c.checkForOutput('O')
      }
  }

  it should "run bench" in {
    val p = defaultParams.copy(
      threads = 1,
      decoderPerThread = 2,
      maxRegisterFileCommitCount = 2,
      loadStoreQueueIndexWidth = 3,
      tagWidth = 5,
      executors = 4,
      instructionStart = 0x8000_0000L,
    )
    test(
      new B4ProcessorWithMemory(
      )(p),
    )
      .withAnnotations(
        Seq(
          WriteWaveformAnnotation,
          VerilatorBackendAnnotation,
          CachingAnnotation,
        ),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/bench")
        c.io.simulationIO.output.ready.poke(true)
        for (_ <- 0 until p.threads)
          c.checkForOutputAny(2000, print_value = true)
        for (_ <- "OK!\n")
          c.checkForOutputAny(2000, print_value = true)
        for (p <- "took ")
          c.checkForOutput(p, 20000, print_value = true)
        var end = false
        while (!end) {
          val p = c.getOutput(2000)
          print(p)
          if (p == ' ') {
            end = true
          }
        }
        for (p <- "cycles\n")
          c.checkForOutput(p, 1000, print_value = true)

        for (n <- Seq(1, 2, 3, 5, 8, 13, 21, 34, 55))
          for (p <- n.toString + "\n")
            c.checkForOutput(p, 2000, print_value = true)
      }
  }

  it should "run pext_test" in {
    test(
      new B4ProcessorWithMemory(
      )(
        defaultParams.copy(
          threads = 1,
          decoderPerThread = 1,
          maxRegisterFileCommitCount = 1,
          loadStoreQueueIndexWidth = 2,
          enablePExt = true,
        ),
      ),
    )
      .withAnnotations(
        Seq(
          WriteWaveformAnnotation,
          VerilatorBackendAnnotation,
          CachingAnnotation,
        ),
      ) { c =>
        c.initialize("programs/riscv-sample-programs/pext_test")
        c.clock.step(300)
      }
  }
}

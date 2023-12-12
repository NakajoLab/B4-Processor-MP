package b4processor.utils

import chisel3._
import chiseltest.ChiselScalatestTester
import chiseltest.experimental.sanitizeFileName
import circt.stage.ChiselStage

import java.io.PrintWriter
import java.util.regex.Matcher
import scala.reflect.io.Directory
import scala.sys.process._

trait SymbiYosysFormal {
  this: ChiselScalatestTester =>

  def symbiYosysCheck(
    gen: => RawModule,
    depth: Int = 20,
    engine: String = "",
  ) = {
    var s = ChiselStage.emitSystemVerilog(
      gen,
      firtoolOpts = Array(
        "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb,verifLabels",
        //      "--emit-chisel-asserts-as-sva",
        "--dedup",
      ),
    )

    val rrr =
      """if \(`ASSERT_VERBOSE_COND_\).*\n *\$error\((.*)\).*\n.*\n.*""".r
    val rrr2 = """\$fwrite\(.*\n(.*)\).*""".r
    val rrr3 = """\(`PRINTF_COND_\) & """.r
    val rrr4 = """endmodule""".r
    val rrr5 = """([^ ]+): (cover\(.*\);)....(.*)""".r
    //  s = rrr.replaceAllIn(s, "assert(0);")
    s = rrr.replaceAllIn(
      s,
      m => {
        val comment = m.group(1)
        val label = comment
          .replaceAll("[^a-zA-Z0-9 ]", "")
          .replaceAll("[ :]+", "_")
          .stripMargin('_')
        Matcher.quoteReplacement(s"$label: assert(0); // ${comment}")
      },
    )
    s = rrr2.replaceAllIn(
      s,
      m => {
        val label = m
          .group(1)
          .replaceAll("[^a-zA-Z0-9 :]", "")
          .replaceAll("[ :]+", "_")
          .stripMargin('_')
        Matcher.quoteReplacement(s"$label: assume(0); // ${m.group(1)}")
      },
    )
    s = rrr3.replaceAllIn(s, "")
    s = rrr4.replaceAllIn(
      s,
      "    reg f_valid;\n" +
        "    initial f_valid = 1;\n" +
        "    always @(posedge clock) begin\n" +
        "      f_valid <= 0;\n" +
        "      assume (reset == f_valid);\n" +
        "    end\n" +
        "endmodule",
    )
    s = rrr5.replaceAllIn(
      s,
      m => {
        val normalized_comment = m
          .group(3)
          .replaceAll("[^a-zA-Z0-9 :]", "")
          .replaceAll("[ :]+", "_")
          .stripMargin('_')
        s"${m.group(1)}__${normalized_comment}: ${m.group(2)} // ${m.group(3)}"
      },
    )
    val name = sanitizeFileName(getTestName)
    Directory("formal").createDirectory()
    Directory(s"formal/${name}").createDirectory()
    val file = new PrintWriter(s"formal/${name}/out.sv")
    file.write(s)
    file.close()

    val p = """module ([a-zA-Z0-9_]+)\(.*""".r
    val module_line = s
      .split("\n")
      .toSeq
      .findLast(ss => p.matches(ss))
      .get
    val module_name = module_line match {
      case p(n) => n
      case _    => throw new RuntimeException("module name not found?")
    }

    val conf_file_content =
      s"""[tasks]
         |bmc
         |prove
         |cover
         |
         |[options]
         |depth ${depth}
         |cover:
         |mode cover
         |--
         |prove:
         |mode prove
         |--
         |bmc:
         |mode bmc
         |--
         |
         |[engines]
         |smtbmc ${engine}
         |
         |[script]
         |read -formal out.sv
         |prep -top ${module_name}
         |opt_merge -share_all
         |
         |[files]
         |out.sv
         |""".stripMargin
    val file2 = new PrintWriter(s"formal/${name}/check.sby")
    file2.write(conf_file_content)
    file2.close()

    val result = s"sh -c 'cd formal/${name}; sby -f check.sby'".!
    if (result != 0) {
      throw new RuntimeException(s"formal check failed! exeit code ${result}")
    }
  }
}

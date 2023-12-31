package b4processor.utils.operations

import circt.stage.ChiselStage
import b4processor.Parameters
import b4processor.modules.PExt.PExtensionOperation
import b4processor.riscv.Instructions
import b4processor.riscv.Instructions.{M64Type, MType}
import b4processor.utils.BundleInitialize.AddBundleInitializeConstructor
import b4processor.utils.RVRegister
import b4processor.utils.RVRegister.{AddRegConstructor, AddUIntRegConstructor}
import b4processor.utils.operations.OptionalBundle.{invalid, valid}
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}

import scala.+:
import scala.language.implicitConversions

class Operations extends Bundle {
  val valid = Bool()
  val sources = Vec(3, new SourceDef())
  val rd = new RVRegister
  val useRs2AsStoreSrc = Bool()
  val branchOffset = SInt(12.W)
  val aluOp = OptionalBundle(ALUOperation())
  val loadStoreOp = OptionalBundle(LoadStoreOperation())
  val loadStoreWidth = LoadStoreWidth()
  val fence = Bool()
  val ecall = Bool()
  val ebreak = Bool()
  val fence_i = Bool()
  val compressed = Bool()
  val csrOp = OptionalBundle(CSROperation())
  val sendReceiveOp = OptionalBundle(SendReceiveOperation()) //added by akamatsu
//  val csrAddress = UInt(12.W)
  val amoOp = OptionalBundle(AMOOperation.Type())
  val amoWidth = AMOOperationWidth.Type()
  val amoOrdering = new AMOOrdering
  val pextOp = OptionalBundle(new PExtensionOperation.Type())

  class SourceDef extends Bundle {
    val reg = new RVRegister
    val value = UInt(64.W)
    val valid = Bool()
  }
}

object Operations {
  implicit def Int2UInt(n: Int): UInt = n.U
  implicit def functionReduceSecondArgument(
    f: Operations => (Data, Data),
  ): (Operations, UInt) => (Data, Data) = (op, _) => f(op)

  private def createOperation(
    elems: ((Operations, UInt) => (Data, Data))*,
  ): (UInt, UInt) => Operations = (instruction, pc) => {
    createOperationWithPC(elems.map {
      f => (op: Operations, inst: UInt, _pc: UInt) => f(op, inst)
    }: _*)(instruction, pc)
  }

  private def createOperationWithPC(
    elems: ((Operations, UInt, UInt) => (Data, Data))*,
  ): (UInt, UInt) => Operations = (instruction, pc) => {
    val w = Wire(new Operations())
    w := DontCare
    w.valid := true.B
    for (e <- elems) {
      val a = e(w, instruction, pc)
      a._1 := a._2
    }
    w
  }

  def default: Operations = new Operations().initialize(
    _.valid -> false.B,
    _.sources(0).reg -> 0.reg,
    _.sources(1).reg -> 0.reg,
    _.sources(2).reg -> 0.reg,
    _.rd -> 0.reg,
    _.useRs2AsStoreSrc -> false.B,
    _.sources(0).value -> 0.U,
    _.sources(1).value -> 0.U,
    _.sources(2).value -> 0.U,
    _.sources(0).valid -> false.B,
    _.sources(1).valid -> false.B,
    _.sources(2).valid -> false.B,
    _.aluOp -> invalid(ALUOperation.Type()),
    _.loadStoreOp -> invalid(LoadStoreOperation.Type()),
    _.loadStoreWidth -> LoadStoreWidth.Byte,
    _.branchOffset -> 0.S,
    _.fence -> false.B,
    _.fence_i -> false.B,
    _.ecall -> false.B,
    _.ebreak -> false.B,
    _.compressed -> false.B,
    _.csrOp -> invalid(CSROperation.Type()),
//    _.csrAddress -> 0.U,
    _.amoOp -> invalid(AMOOperation.Type()),
    _.amoWidth -> AMOOperationWidth.Word,
    _.amoOrdering -> AMOOrdering(false.B, false.B),
    _.pextOp -> invalid(PExtensionOperation.Type()),
    _.sendReceiveOp -> invalid(SendReceiveOperation.Type()), //added by akamatsu
  )

  implicit class UIntAccess(u: UInt) {
    def catAccess(t: (Int, Int)*): UInt =
      t.map(a => u(a._1, a._2)).reduce(_ ## _)
  }

  implicit def Int2IntTuple(n: Int): (Int, Int) = (n, n)

  def btypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> OptionalBundle.valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      _.branchOffset -> _.catAccess(31, 7, (30, 25), (11, 8)).asSInt,
    )
  def rtypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      _.rd -> _(11, 7).reg,
    )

  def itypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      (u, _) => u.sources(1).reg -> 0.reg,
      (a, b) => a.sources(1).value -> b(31, 20).asSInt.pad(64).asUInt,
      (u, _) => u.sources(1).valid -> true.B,
      _.rd -> _(11, 7).reg,
    )

  def itype64ShiftOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).value -> _(25, 20),
      (u, _) => u.sources(1).valid -> true.B,
      _.rd -> _(11, 7).reg,
    )

  def itype64ShiftWOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      (u, _) => u.sources(1).reg -> 0.reg,
      _.sources(1).value -> _(24, 20),
      (u, _) => u.sources(1).valid -> true.B,
      _.rd -> _(11, 7).reg,
    )

  def utypeOp(op: ALUOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.aluOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      (u, _) => u.sources(1).reg -> 0.reg,
      _.sources(1).value -> _(31, 20),
      (u, _) => u.sources(1).valid -> true.B,
      _.rd -> _(11, 7).reg,
    )

  def loadOp(
    op: LoadStoreOperation.Type,
    width: LoadStoreWidth.Type,
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.loadStoreOp -> valid(op),
      (u, _) => u.loadStoreWidth -> width,
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).value -> _(31, 20).asSInt.pad(64).asUInt,
      (u, _) => u.sources(1).valid -> true.B,
      _.rd -> _(11, 7).reg,
    )

  def storeOp(
    op: LoadStoreOperation.Type,
    width: LoadStoreWidth.Type,
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.loadStoreOp -> valid(op),
      (u, _) => u.loadStoreWidth -> width,
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).value -> _.catAccess((31, 25), (11, 7)).asSInt
        .pad(64)
        .asUInt,
      (u, _) => u.sources(1).valid -> true.B,
      (u, _) => u.rd -> 0.reg,
      _.sources(1).reg -> _(24, 20).reg,
      (u, _) => u.useRs2AsStoreSrc -> true.B,
    )

  def receiveOp(op: SendReceiveOperation.Type): (UInt, UInt) => Operations = //added by akamatsu
    createOperation(
      (u, _) => u.sendReceiveOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      _.rd -> _(11, 7).reg,
    )

  def sendOp(op: SendReceiveOperation.Type): (UInt, UInt) => Operations = //added by akamatsu
    createOperation(
      (u, _) => u.sendReceiveOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      _.sources(2).reg -> _(31, 27).reg,
      (u, _) => u.rd -> 0.reg,
      //_.rd -> _(11, 7).reg,
    )


  def csrOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11, 7).reg,
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).value -> _(31, 20),
      (u, _) => u.sources(1).valid -> true.B,
      (u, _) => u.csrOp -> valid(op),
    )

  def csrImmOp(op: CSROperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (operations, int) => operations.rd -> int(11, 7).reg,
      _.sources(0).value -> _(19, 15),
      (u, _) => u.sources(0).valid -> true.B,
      _.sources(1).value -> _(31, 20),
      (u, _) => u.sources(1).valid -> true.B,
      (u, _) => u.csrOp -> valid(op),
    )

  def BaseDecodingList = {
    import Instructions.{IType, I64Type, ZICSRType, ZIFENCEIType}
    Seq(
      IType("BEQ") -> btypeOp(ALUOperation.BranchEqual),
      IType("BNE") -> btypeOp(ALUOperation.BranchNotEqual),
      IType("BLT") -> btypeOp(ALUOperation.BranchLessThan),
      IType("BGE") -> btypeOp(ALUOperation.BranchGreaterThanOrEqual),
      IType("BLTU") -> btypeOp(ALUOperation.BranchLessThanUnsigned),
      IType("BGEU") -> btypeOp(ALUOperation.BranchGreaterThanOrEqualUnsigned),
      IType("ADD") -> rtypeOp(ALUOperation.Add),
      IType("SLT") -> rtypeOp(ALUOperation.Slt),
      IType("SLTU") -> rtypeOp(ALUOperation.Sltu),
      IType("AND") -> rtypeOp(ALUOperation.And),
      IType("OR") -> rtypeOp(ALUOperation.Or),
      IType("XOR") -> rtypeOp(ALUOperation.Xor),
      IType("SLL") -> rtypeOp(ALUOperation.Sll),
      IType("SRL") -> rtypeOp(ALUOperation.Srl),
      IType("SUB") -> rtypeOp(ALUOperation.Sub),
      IType("SRA") -> rtypeOp(ALUOperation.Sra),
      IType("ADDI") -> itypeOp(ALUOperation.Add),
      IType("SLTI") -> itypeOp(ALUOperation.Slt),
      IType("SLTIU") -> itypeOp(ALUOperation.Sltu),
      IType("ANDI") -> itypeOp(ALUOperation.And),
      IType("ORI") -> itypeOp(ALUOperation.Or),
      IType("XORI") -> itypeOp(ALUOperation.Xor),
      IType("SND") -> sendOp(SendReceiveOperation.Send), //added by akamatsu
      IType("RCV") -> receiveOp(SendReceiveOperation.Receive), //added by akamatsu
      MType("DIV") -> rtypeOp(ALUOperation.Div),
      MType("MUL") -> rtypeOp(ALUOperation.Mul),
      MType("REM") -> rtypeOp(ALUOperation.Rem),
      M64Type("DIVW") -> rtypeOp(ALUOperation.Div),
      M64Type("MULW") -> rtypeOp(ALUOperation.Mul),
      M64Type("REMW") -> rtypeOp(ALUOperation.Rem),
//      I64Type("SLLI") -> itypeOp(ALUOperation.Sll),
//      I64Type("SRLI") -> itypeOp(ALUOperation.Srl),
//      I64Type("SRAI") -> itypeOp(ALUOperation.Sra),
      IType("LUI") -> createOperation(
        _.rd -> _(11, 7).reg,
        (u, _) => u.sources(0).reg -> 0.reg,
        (u, inst) =>
          u.sources(0).value ->
            (inst(31, 12) ## 0.U(12.W)).asSInt
              .pad(64)
              .asUInt,
        (u, _) => u.sources(0).valid -> true.B,
        (u, _) => u.sources(1).valid -> true.B,
        (u, _) => u.sources(1).value -> 0.U,
        (u, _) => u.aluOp -> valid(ALUOperation.Add),
      ),
      IType("AUIPC") -> createOperationWithPC(
        (u, inst, _) => u.rd -> inst(11, 7).reg,
        (u, inst, _) =>
          u.sources(0).value ->
            (inst(31, 12) ## 0.U(12.W)).asSInt
              .pad(64)
              .asUInt,
        (u, _, _) => u.sources(0).valid -> true.B,
        (u, _, _) => u.sources(1).valid -> true.B,
        (u, _, pc) => u.sources(1).value -> pc,
        (u, _, _) => u.aluOp -> valid(ALUOperation.Add),
      ),
      IType("JAL") -> createOperationWithPC(
        (u, inst, _) => u.rd -> inst(11, 7).reg,
        (u, _, pc) => u.sources(1).value -> pc,
        (u, inst, _) =>
          u.sources(0).value ->
            (inst.catAccess(31, (19, 12), 20, (30, 21)) ## 0.U(1.W)).asSInt
              .pad(64)
              .asUInt,
        (u, _, _) => u.sources(0).valid -> true.B,
        (u, _, _) => u.sources(1).valid -> true.B,
        (u, _, _) => u.aluOp -> valid(ALUOperation.AddJAL),
      ),
      IType("JALR") -> createOperationWithPC(
        (u, inst, _) => u.rd -> inst(11, 7).reg,
        (u, _, pc) => u.sources(1).value -> pc,
        (u, inst, _) => u.sources(0).reg -> inst(19, 15).reg,
        (u, _, _) => u.sources(1).valid -> true.B,
        (u, inst, _) => u.branchOffset -> inst(31, 20).asSInt,
        (u, _, _) => u.aluOp -> valid(ALUOperation.AddJALR),
      ),
      IType("LB") -> loadOp(LoadStoreOperation.Load, LoadStoreWidth.Byte),
      IType("LBU") -> loadOp(
        LoadStoreOperation.LoadUnsigned,
        LoadStoreWidth.Byte,
      ),
      IType("LH") -> loadOp(LoadStoreOperation.Load, LoadStoreWidth.HalfWord),
      IType("LHU") -> loadOp(
        LoadStoreOperation.LoadUnsigned,
        LoadStoreWidth.HalfWord,
      ),
      IType("LW") -> loadOp(LoadStoreOperation.Load, LoadStoreWidth.Word),
      I64Type("LWU") -> loadOp(
        LoadStoreOperation.LoadUnsigned,
        LoadStoreWidth.Word,
      ),
      I64Type("LD") -> loadOp(
        LoadStoreOperation.Load,
        LoadStoreWidth.DoubleWord,
      ),
      IType("SB") -> storeOp(LoadStoreOperation.Store, LoadStoreWidth.Byte),
      IType("SH") -> storeOp(LoadStoreOperation.Store, LoadStoreWidth.HalfWord),
      IType("SW") -> storeOp(LoadStoreOperation.Store, LoadStoreWidth.Word),
      I64Type("SD") -> storeOp(
        LoadStoreOperation.Store,
        LoadStoreWidth.DoubleWord,
      ),
      IType("FENCE") -> createOperation((u, _) => u.fence -> true.B),
      IType("ECALL") -> createOperation((u, _) => u.ecall -> true.B),
      IType("EBREAK") -> createOperation((u, _) => u.ebreak -> true.B),
      ZIFENCEIType("FENCE_I") -> createOperation((u, _) => u.fence_i -> true.B),
      I64Type("ADDIW") -> itypeOp(ALUOperation.AddW),
      I64Type("SLLI") -> itype64ShiftOp(ALUOperation.Sll),
      I64Type("SRLI") -> itype64ShiftOp(ALUOperation.Srl),
      I64Type("SRAI") -> itype64ShiftOp(ALUOperation.Sra),
      I64Type("SLLIW") -> itype64ShiftWOp(ALUOperation.SllW),
      I64Type("SRLIW") -> itype64ShiftWOp(ALUOperation.SrlW),
      I64Type("SRAIW") -> itype64ShiftWOp(ALUOperation.SraW),
      I64Type("ADDW") -> rtypeOp(ALUOperation.AddW),
      I64Type("SLLW") -> rtypeOp(ALUOperation.SllW),
      I64Type("SRLW") -> rtypeOp(ALUOperation.SrlW),
      I64Type("SUBW") -> rtypeOp(ALUOperation.SubW),
      I64Type("SRAW") -> rtypeOp(ALUOperation.SraW),
      ZICSRType("CSRRC") -> csrOp(CSROperation.ReadClear),
      ZICSRType("CSRRS") -> csrOp(CSROperation.ReadSet),
      ZICSRType("CSRRW") -> csrOp(CSROperation.ReadWrite),
      ZICSRType("CSRRCI") -> csrImmOp(CSROperation.ReadClear),
      ZICSRType("CSRRSI") -> csrImmOp(CSROperation.ReadSet),
      ZICSRType("CSRRWI") -> csrImmOp(CSROperation.ReadWrite),
    )
  }

  def amoOp(
    op: AMOOperation.Type,
    width: AMOOperationWidth.Type = AMOOperationWidth.Word,
  ): (UInt, UInt) => Operations =
    createOperation(
      _.rd -> _(11, 7).reg,
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      (u, i) => u.amoOrdering -> AMOOrdering(i(26), i(25)),
      (u, _) => u.amoOp -> valid(op),
      (u, _) => u.amoWidth -> width,
    )

  def AextDecodingList = {
    import Instructions.{AType, A64Type}
    import AMOOperation._
    import AMOOperationWidth._

    Seq(
      // 32
      AType("AMOADD_W") -> amoOp(Add),
      AType("AMOAND_W") -> amoOp(And),
      AType("AMOMAX_W") -> amoOp(Max),
      AType("AMOMAXU_W") -> amoOp(MaxU),
      AType("AMOMIN_W") -> amoOp(Min),
      AType("AMOMINU_W") -> amoOp(MinU),
      AType("AMOOR_W") -> amoOp(Or),
      AType("AMOSWAP_W") -> amoOp(Swap),
      AType("AMOXOR_W") -> amoOp(Xor),
      AType("LR_W") -> amoOp(Lr),
      AType("SC_W") -> amoOp(Sc),
      // 64
      A64Type("AMOADD_D") -> amoOp(Add, DoubleWord),
      A64Type("AMOAND_D") -> amoOp(And, DoubleWord),
      A64Type("AMOMAX_D") -> amoOp(Max, DoubleWord),
      A64Type("AMOMAXU_D") -> amoOp(MaxU, DoubleWord),
      A64Type("AMOMIN_D") -> amoOp(Min, DoubleWord),
      A64Type("AMOMINU_D") -> amoOp(MinU, DoubleWord),
      A64Type("AMOOR_D") -> amoOp(Or, DoubleWord),
      A64Type("AMOSWAP_D") -> amoOp(Swap, DoubleWord),
      A64Type("AMOXOR_D") -> amoOp(Xor, DoubleWord),
      A64Type("LR_D") -> amoOp(Lr, DoubleWord),
      A64Type("SC_D") -> amoOp(Sc, DoubleWord),
    )
  }

//  def CextDecodingList = {
//    import Instructions.CType
//
//    Seq(
//      // 32
//      CType("C_ADD") -> createOperation(),
//      CType("C_ADDI") -> createOperation(),
//      CType("C_ADDI16SP") -> createOperation(
//        _.aluOp -> ALUOperation.Add,
//        _.sources(0).reg -> 2.reg,
//        _.rd -> 1.U ## _(4, 2),
//        (u, i) =>
//          u.sources(1).value -> (i.catAccess((6, 5), (12, 10)) ## 0.U(3.W)),
//        _.sources(1).valid -> true.B
//      ),
//      CType("C_ADDI4SPN") -> createOperation(
//        _.aluOp -> ALUOperation.Add,
//        _.sources(0).reg -> 2.reg,
//        _.rd -> 1.U ## _(4, 2),
//        (u, i) =>
//          u.sources(1).value -> (i.catAccess((10, 7), (12, 11), 5, 6) ## 0.U(2.W)),
//        _.sources(1).valid -> true.B
//      ),
//      CType("C_AND") -> createOperation(),
//      CType("C_ANDI") -> createOperation(),
//      CType("C_BEQZ") -> createOperation(),
//      CType("C_BNEZ") -> createOperation(),
//      CType("C_EBREAK") -> createOperation(),
//      CType("C_J") -> createOperation(),
//      CType("C_JALR") -> createOperation(),
//      CType("C_JR") -> createOperation(),
//      CType("C_LI") -> createOperation(),
//      CType("C_LUI") -> createOperation(),
//      CType("C_LW") -> createOperation(
//        (u, _) => u.loadStoreOp -> LoadStoreOperation.Load,
//        (u, _) => u.loadStoreWidth -> LoadStoreWidth.Word,
//        (u, i) => u.sources(0).reg -> ("b01".U(2.W) ## i(9, 7)).reg,
//        (u, i) => u.sources(1).value -> (i.catAccess(5, (12, 10), 6) ## 0.U(2.W)),
//        (u, _) => u.sources(1).valid -> true.B,
//        (u, i) => u.rd -> ("b01".U(2.W) ## i(4, 2)).reg
//      ),
//      CType("C_LWSP") -> createOperation(),
//      CType("C_MV") -> createOperation(),
//      CType("C_NOP") -> createOperation(),
//      CType("C_OR") -> createOperation(),
//      CType("C_SUB") -> createOperation(),
//      CType("C_SW") -> createOperation(),
//      CType("C_SWSP") -> createOperation(),
//      CType("C_XOR") -> createOperation(),
//      C64Type("C_ADDIW") -> createOperation(),
//      C64Type("C_ADDW") -> createOperation(),
//      C64Type("C_LD") -> createOperation(),
//      C64Type("C_LDSP") -> createOperation(),
//      C64Type("C_SD") -> createOperation(),
//      C64Type("C_SDSP") -> createOperation(),
//      C64Type("C_SLLI") -> createOperation(),
//      C64Type("C_SRAI") -> createOperation(),
//      C64Type("C_SRLI") -> createOperation(),
//      C64Type("C_SUBW") -> createOperation()
//    )
//  }

  def zpnRtypeOpWithRd(
    op: PExtensionOperation.Type,
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.pextOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      _.sources(2).reg -> _(11, 7).reg,
      _.rd -> _(11, 7).reg,
    )

  def rtypeWithR3(op: PExtensionOperation.Type): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.pextOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.sources(1).reg -> _(24, 20).reg,
      _.sources(2).reg -> _(31, 27).reg,
      _.rd -> _(11, 7).reg,
    )

  def zpnSingleOperandWithImm(
    op: PExtensionOperation.Type,
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.pextOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      (u, _) => u.sources(1).reg -> 0.reg,
      (a, b) => a.sources(1).value -> b(24, 20),
      (u, _) => u.sources(1).valid -> true.B,
      _.rd -> _(11, 7).reg,
    )

  def zpnSingleOperand(
    op: PExtensionOperation.Type,
  ): (UInt, UInt) => Operations =
    createOperation(
      (u, _) => u.pextOp -> valid(op),
      _.sources(0).reg -> _(19, 15).reg,
      _.rd -> _(11, 7).reg,
    )

  def ZPN32ExtDecodingList = {
    import Instructions.ZPNType
    import PExtensionOperation._
    Seq(
      ZPNType("ADD16") -> zpnRtypeOpWithRd(ADD16),
      ZPNType("ADD8") -> zpnRtypeOpWithRd(ADD8),
      ZPNType("AVE") -> zpnRtypeOpWithRd(AVE),
//      ZPNType("CLROV") -> , // covered in csr
      ZPNType("CLRS16") -> zpnSingleOperand(CLRS16),
      ZPNType("CLRS32") -> zpnSingleOperand(CLRS32),
      ZPNType("CLRS8") -> zpnSingleOperand(CLRS8),
      ZPNType("CLZ16") -> zpnSingleOperand(CLZ16),
      ZPNType("CLZ32") -> zpnSingleOperand(CLZ32),
      ZPNType("CLZ8") -> zpnSingleOperand(CLZ8),
      ZPNType("CMPEQ16") -> zpnRtypeOpWithRd(CMPEQ16),
      ZPNType("CMPEQ8") -> zpnRtypeOpWithRd(CMPEQ8),
      ZPNType("CRAS16") -> zpnRtypeOpWithRd(CRAS16),
      ZPNType("CRSA16") -> zpnRtypeOpWithRd(CRSA16),
      ZPNType("KABS16") -> zpnRtypeOpWithRd(KABS16),
      ZPNType("KABS8") -> zpnSingleOperand(KABS8),
      ZPNType("KABSW") -> zpnSingleOperand(KABSW),
      ZPNType("KADD16") -> zpnRtypeOpWithRd(KADD16),
      ZPNType("KADD8") -> zpnRtypeOpWithRd(KADD8),
      ZPNType("KADDH") -> zpnRtypeOpWithRd(KADDH),
      ZPNType("KADDW") -> zpnRtypeOpWithRd(KADDW),
      ZPNType("KCRAS16") -> zpnRtypeOpWithRd(KCRAS16),
      ZPNType("KCRSA16") -> zpnRtypeOpWithRd(KCRSA16),
      ZPNType("KDMABB") -> zpnRtypeOpWithRd(KDMABB),
      ZPNType("KDMABT") -> zpnRtypeOpWithRd(KDMABT),
      ZPNType("KDMATT") -> zpnRtypeOpWithRd(KDMATT),
      ZPNType("KDMBB") -> zpnRtypeOpWithRd(KDMBB),
      ZPNType("KDMBT") -> zpnRtypeOpWithRd(KDMBT),
      ZPNType("KDMTT") -> zpnRtypeOpWithRd(KDMTT),
      ZPNType("KHM16") -> zpnRtypeOpWithRd(KHM16),
      ZPNType("KHM8") -> zpnRtypeOpWithRd(KHM8),
      ZPNType("KHMBB") -> zpnRtypeOpWithRd(KHMBB),
      ZPNType("KHMBT") -> zpnRtypeOpWithRd(KHMBT),
      ZPNType("KHMTT") -> zpnRtypeOpWithRd(KHMTT),
      ZPNType("KHMX16") -> zpnRtypeOpWithRd(KHMX16),
      ZPNType("KHMX8") -> zpnRtypeOpWithRd(KHMX8),
      ZPNType("KMABB") -> zpnRtypeOpWithRd(KMABB),
      ZPNType("KMABT") -> zpnRtypeOpWithRd(KMABT),
      ZPNType("KMADA") -> zpnRtypeOpWithRd(KMADA),
      ZPNType("KMADRS") -> zpnRtypeOpWithRd(KMADRS),
      ZPNType("KMADS") -> zpnRtypeOpWithRd(KMADS),
      ZPNType("KMATT") -> zpnRtypeOpWithRd(KMATT),
      ZPNType("KMAXDA") -> zpnRtypeOpWithRd(KMAXDA),
      ZPNType("KMAXDS") -> zpnRtypeOpWithRd(KMAXDS),
      ZPNType("KMDA") -> zpnRtypeOpWithRd(KMDA),
      ZPNType("KMMAC") -> zpnRtypeOpWithRd(KMMAC),
      ZPNType("KMMAC_U") -> zpnRtypeOpWithRd(KMMAC_U),
      ZPNType("KMMAWB") -> zpnRtypeOpWithRd(KMMAWB),
      ZPNType("KMMAWB2") -> zpnRtypeOpWithRd(KMMAWB2),
      ZPNType("KMMAWB2_U") -> zpnRtypeOpWithRd(KMMAWB2_U),
      ZPNType("KMMAWB_U") -> zpnRtypeOpWithRd(KMMAWB_U),
      ZPNType("KMMAWT") -> zpnRtypeOpWithRd(KMMAWT),
      ZPNType("KMMAWT2") -> zpnRtypeOpWithRd(KMMAWT2),
      ZPNType("KMMAWT2_U") -> zpnRtypeOpWithRd(KMMAWT2_U),
      ZPNType("KMMAWT_U") -> zpnRtypeOpWithRd(KMMAWT_U),
      ZPNType("KMMSB") -> zpnRtypeOpWithRd(KMMSB),
      ZPNType("KMMSB_U") -> zpnRtypeOpWithRd(KMMSB_U),
      ZPNType("KMMWB2") -> zpnRtypeOpWithRd(KMMWB2),
      ZPNType("KMMWB2_U") -> zpnRtypeOpWithRd(KMMWB2_U),
      ZPNType("KMMWT2") -> zpnRtypeOpWithRd(KMMWT2),
      ZPNType("KMMWT2_U") -> zpnRtypeOpWithRd(KMMWT2_U),
      ZPNType("KMSDA") -> zpnRtypeOpWithRd(KMSDA),
      ZPNType("KMSXDA") -> zpnRtypeOpWithRd(KMSXDA),
      ZPNType("KMXDA") -> zpnRtypeOpWithRd(KMXDA),
      ZPNType("KSLL16") -> zpnRtypeOpWithRd(KSLL16),
      ZPNType("KSLL8") -> zpnRtypeOpWithRd(KSLL8),
      ZPNType("KSLLI16") -> zpnRtypeOpWithRd(KSLLI16),
      ZPNType("KSLLI8") -> zpnRtypeOpWithRd(KSLLI8),
      ZPNType("KSLLIW") -> zpnRtypeOpWithRd(KSLLIW),
      ZPNType("KSLLW") -> zpnRtypeOpWithRd(KSLLW),
      ZPNType("KSLRA16") -> zpnRtypeOpWithRd(KSLRA16),
      ZPNType("KSLRA16_U") -> zpnRtypeOpWithRd(KSLRA16_U),
      ZPNType("KSLRA8") -> zpnRtypeOpWithRd(KSLRA8),
      ZPNType("KSLRA8_U") -> zpnRtypeOpWithRd(KSLRA8_U),
      ZPNType("KSLRAW") -> zpnRtypeOpWithRd(KSLRAW),
      ZPNType("KSLRAW_U") -> zpnRtypeOpWithRd(KSLRAW_U),
      ZPNType("KSTAS16") -> zpnRtypeOpWithRd(KSTAS16),
      ZPNType("KSTSA16") -> zpnRtypeOpWithRd(KSTSA16),
      ZPNType("KSUB16") -> zpnRtypeOpWithRd(KSUB16),
      ZPNType("KSUB8") -> zpnRtypeOpWithRd(KSUB8),
      ZPNType("KSUBH") -> zpnRtypeOpWithRd(KSUBH),
      ZPNType("KSUBW") -> zpnRtypeOpWithRd(KSUBW),
      ZPNType("KWMMUL") -> zpnRtypeOpWithRd(KWMMUL),
      ZPNType("KWMMUL_U") -> zpnRtypeOpWithRd(KWMMUL_U),
      ZPNType("MADDR32") -> zpnRtypeOpWithRd(MADDR32),
      ZPNType("MSUBR32") -> zpnRtypeOpWithRd(MSUBR32),
      ZPNType("PBSAD") -> zpnRtypeOpWithRd(PBSAD),
      ZPNType("PBSADA") -> zpnRtypeOpWithRd(PBSADA),
      ZPNType("PKBT16") -> zpnRtypeOpWithRd(PKBT16),
      ZPNType("PKTB16") -> zpnRtypeOpWithRd(PKTB16),
      ZPNType("RADD16") -> zpnRtypeOpWithRd(RADD16),
      ZPNType("RADD8") -> zpnRtypeOpWithRd(RADD8),
      ZPNType("RADDW") -> zpnRtypeOpWithRd(RADDW),
      ZPNType("RCRAS16") -> zpnRtypeOpWithRd(RCRAS16),
      ZPNType("RCRSA16") -> zpnRtypeOpWithRd(RCRSA16),
//      ZPNType("RDOV") ->, //covered by csr
      ZPNType("RSTAS16") -> zpnRtypeOpWithRd(RSTAS16),
      ZPNType("RSTSA16") -> zpnRtypeOpWithRd(RSTSA16),
      ZPNType("RSUB16") -> zpnRtypeOpWithRd(RSUB16),
      ZPNType("RSUB8") -> zpnRtypeOpWithRd(RSUB8),
      ZPNType("RSUBW") -> zpnRtypeOpWithRd(RSUBW),
      ZPNType("SCLIP16") -> zpnRtypeOpWithRd(SCLIP16),
      ZPNType("SCLIP32") -> zpnRtypeOpWithRd(SCLIP32),
      ZPNType("SCLIP8") -> zpnRtypeOpWithRd(SCLIP8),
      ZPNType("SCMPLE16") -> zpnRtypeOpWithRd(SCMPLE16),
      ZPNType("SCMPLE8") -> zpnRtypeOpWithRd(SCMPLE8),
      ZPNType("SCMPLT16") -> zpnRtypeOpWithRd(SCMPLT16),
      ZPNType("SCMPLT8") -> zpnRtypeOpWithRd(SCMPLT8),
      ZPNType("SLL16") -> zpnRtypeOpWithRd(SLL16),
      ZPNType("SLL8") -> zpnRtypeOpWithRd(SLL8),
      ZPNType("SLLI16") -> zpnRtypeOpWithRd(SLLI16),
      ZPNType("SLLI8") -> zpnRtypeOpWithRd(SLLI8),
      ZPNType("SMAQA") -> zpnRtypeOpWithRd(SMAQA),
      ZPNType("SMAQA_SU") -> zpnRtypeOpWithRd(SMAQASU),
      ZPNType("SMAX16") -> zpnRtypeOpWithRd(SMAX16),
      ZPNType("SMAX8") -> zpnRtypeOpWithRd(SMAX8),
      ZPNType("SMBB16") -> zpnRtypeOpWithRd(SMBB16),
      ZPNType("SMBT16") -> zpnRtypeOpWithRd(SMBT16),
      ZPNType("SMDRS") -> zpnRtypeOpWithRd(SMDRS),
      ZPNType("SMDS") -> zpnRtypeOpWithRd(SMDS),
      ZPNType("SMIN16") -> zpnRtypeOpWithRd(SMIN16),
      ZPNType("SMIN8") -> zpnRtypeOpWithRd(SMIN8),
      ZPNType("SMMUL_U") -> zpnRtypeOpWithRd(SMMUL_U),
      ZPNType("SMMWB") -> zpnRtypeOpWithRd(SMMWB),
      ZPNType("SMMWB_U") -> zpnRtypeOpWithRd(SMMWB_U),
      ZPNType("SMMWT") -> zpnRtypeOpWithRd(SMMWT),
      ZPNType("SMMWT_U") -> zpnRtypeOpWithRd(SMMWT_U),
      ZPNType("SMTT16") -> zpnRtypeOpWithRd(SMTT16),
      ZPNType("SMXDS") -> zpnRtypeOpWithRd(SMXDS),
      ZPNType("SRA16") -> zpnRtypeOpWithRd(SRA16),
      ZPNType("SRA16_U") -> zpnRtypeOpWithRd(SRA16_U),
      ZPNType("SRA8") -> zpnRtypeOpWithRd(SRA8),
      ZPNType("SRA8_U") -> zpnRtypeOpWithRd(SRA8_U),
      ZPNType("SRA_U") -> zpnRtypeOpWithRd(SRA_U),
      ZPNType("SRAI16") -> zpnRtypeOpWithRd(SRAI16),
      ZPNType("SRAI16_U") -> zpnRtypeOpWithRd(SRAI16_U),
      ZPNType("SRAI8") -> zpnRtypeOpWithRd(SRAI8),
      ZPNType("SRAI8_U") -> zpnRtypeOpWithRd(SRAI8_U),
      ZPNType("SRL16") -> zpnRtypeOpWithRd(SRL16),
      ZPNType("SRL16_U") -> zpnRtypeOpWithRd(SRL16_U),
      ZPNType("SRL8") -> zpnRtypeOpWithRd(SRL8),
      ZPNType("SRL8_U") -> zpnRtypeOpWithRd(SRL8_U),
      ZPNType("SRLI16") -> zpnRtypeOpWithRd(SRLI16),
      ZPNType("SRLI16_U") -> zpnRtypeOpWithRd(SRLI16_U),
      ZPNType("SRLI8") -> zpnRtypeOpWithRd(SRLI8),
      ZPNType("SRLI8_U") -> zpnRtypeOpWithRd(SRLI8_U),
      ZPNType("STAS16") -> zpnRtypeOpWithRd(STAS16),
      ZPNType("STSA16") -> zpnRtypeOpWithRd(STSA16),
      ZPNType("SUB16") -> zpnRtypeOpWithRd(SUB16),
      ZPNType("SUB8") -> zpnRtypeOpWithRd(SUB8),
      ZPNType("SUNPKD810") -> zpnRtypeOpWithRd(SUNPKD810),
      ZPNType("SUNPKD820") -> zpnRtypeOpWithRd(SUNPKD820),
      ZPNType("SUNPKD830") -> zpnRtypeOpWithRd(SUNPKD830),
      ZPNType("SUNPKD831") -> zpnRtypeOpWithRd(SUNPKD831),
      ZPNType("SUNPKD832") -> zpnRtypeOpWithRd(SUNPKD832),
      ZPNType("UCLIP16") -> zpnRtypeOpWithRd(UCLIP16),
      ZPNType("UCLIP32") -> zpnRtypeOpWithRd(UCLIP32),
      ZPNType("UCLIP8") -> zpnRtypeOpWithRd(UCLIP8),
      ZPNType("UCMPLE16") -> zpnRtypeOpWithRd(UCMPLE16),
      ZPNType("UCMPLE8") -> zpnRtypeOpWithRd(UCMPLE8),
      ZPNType("UCMPLT16") -> zpnRtypeOpWithRd(UCMPLT16),
      ZPNType("UCMPLT8") -> zpnRtypeOpWithRd(UCMPLT8),
      ZPNType("UKADD16") -> zpnRtypeOpWithRd(UKADD16),
      ZPNType("UKADD8") -> zpnRtypeOpWithRd(UKADD8),
      ZPNType("UKADDH") -> zpnRtypeOpWithRd(UKADDH),
      ZPNType("UKADDW") -> zpnRtypeOpWithRd(UKADDW),
      ZPNType("UKCRAS16") -> zpnRtypeOpWithRd(UKCRAS16),
      ZPNType("UKCRSA16") -> zpnRtypeOpWithRd(UKCRSA16),
      ZPNType("UKSTAS16") -> zpnRtypeOpWithRd(UKSTAS16),
      ZPNType("UKSTSA16") -> zpnRtypeOpWithRd(UKSTSA16),
      ZPNType("UKSUB16") -> zpnRtypeOpWithRd(UKSUB16),
      ZPNType("UKSUB8") -> zpnRtypeOpWithRd(UKSUB8),
      ZPNType("UKSUBH") -> zpnRtypeOpWithRd(UKSUBH),
      ZPNType("UKSUBW") -> zpnRtypeOpWithRd(UKSUBW),
      ZPNType("UMAQA") -> zpnRtypeOpWithRd(UMAQA),
      ZPNType("UMAX16") -> zpnRtypeOpWithRd(UMAX16),
      ZPNType("UMAX8") -> zpnRtypeOpWithRd(UMAX8),
      ZPNType("UMIN16") -> zpnRtypeOpWithRd(UMIN16),
      ZPNType("UMIN8") -> zpnRtypeOpWithRd(UMIN8),
      ZPNType("URADD16") -> zpnRtypeOpWithRd(URADD16),
      ZPNType("URADD8") -> zpnRtypeOpWithRd(URADD8),
      ZPNType("URADDW") -> zpnRtypeOpWithRd(URADDW),
      ZPNType("URCRAS16") -> zpnRtypeOpWithRd(URCRAS16),
      ZPNType("URCRSA16") -> zpnRtypeOpWithRd(URCRSA16),
      ZPNType("URSTAS16") -> zpnRtypeOpWithRd(URSTAS16),
      ZPNType("URSTSA16") -> zpnRtypeOpWithRd(URSTSA16),
      ZPNType("URSUB16") -> zpnRtypeOpWithRd(URSUB16),
      ZPNType("URSUB8") -> zpnRtypeOpWithRd(URSUB8),
      ZPNType("URSUBW") -> zpnRtypeOpWithRd(URSUBW),
      ZPNType("ZUNPKD810") -> zpnRtypeOpWithRd(ZUNPKD810),
      ZPNType("ZUNPKD820") -> zpnRtypeOpWithRd(ZUNPKD820),
      ZPNType("ZUNPKD830") -> zpnRtypeOpWithRd(ZUNPKD830),
      ZPNType("ZUNPKD831") -> zpnRtypeOpWithRd(ZUNPKD831),
      ZPNType("ZUNPKD832") -> zpnRtypeOpWithRd(ZUNPKD832),
    )
  }

  def ZPN64ExtDecodingList = {
    import Instructions.ZPN64Type
    import PExtensionOperation._
    Seq(
      ZPN64Type("ADD32") -> zpnRtypeOpWithRd(ADD32),
      ZPN64Type("CRAS32") -> zpnRtypeOpWithRd(CRAS32),
      ZPN64Type("CRSA32") -> zpnRtypeOpWithRd(CRSA32),
      ZPN64Type("INSB") -> zpnRtypeOpWithRd(INSB),
      ZPN64Type("KABS32") -> zpnRtypeOpWithRd(KABS32),
      ZPN64Type("KADD32") -> zpnRtypeOpWithRd(KADD32),
      ZPN64Type("KCRAS32") -> zpnRtypeOpWithRd(KCRAS32),
      ZPN64Type("KCRSA32") -> zpnRtypeOpWithRd(KCRSA32),
      ZPN64Type("KDMABB16") -> zpnRtypeOpWithRd(KDMABB16),
      ZPN64Type("KDMABT16") -> zpnRtypeOpWithRd(KDMABT16),
      ZPN64Type("KDMATT16") -> zpnRtypeOpWithRd(KDMATT16),
      ZPN64Type("KDMBB16") -> zpnRtypeOpWithRd(KDMBB16),
      ZPN64Type("KDMBT16") -> zpnRtypeOpWithRd(KDMBT16),
      ZPN64Type("KDMTT16") -> zpnRtypeOpWithRd(KDMTT16),
      ZPN64Type("KHMBB16") -> zpnRtypeOpWithRd(KHMBB16),
      ZPN64Type("KHMBT16") -> zpnRtypeOpWithRd(KHMBT16),
      ZPN64Type("KHMTT16") -> zpnRtypeOpWithRd(KHMTT16),
      ZPN64Type("KMABB32") -> zpnRtypeOpWithRd(KMABB32),
      ZPN64Type("KMABT32") -> zpnRtypeOpWithRd(KMABT32),
      ZPN64Type("KMADRS32") -> zpnRtypeOpWithRd(KMADRS32),
      ZPN64Type("KMADS32") -> zpnRtypeOpWithRd(KMADS32),
      ZPN64Type("KMATT32") -> zpnRtypeOpWithRd(KMATT32),
      ZPN64Type("KMAXDA32") -> zpnRtypeOpWithRd(KMAXDA32),
      ZPN64Type("KMAXDS32") -> zpnRtypeOpWithRd(KMAXDS32),
      ZPN64Type("KMDA32") -> zpnRtypeOpWithRd(KMDA32),
      ZPN64Type("KMSDA32") -> zpnRtypeOpWithRd(KMSDA32),
      ZPN64Type("KMSXDA32") -> zpnRtypeOpWithRd(KMSXDA32),
      ZPN64Type("KMXDA32") -> zpnRtypeOpWithRd(KMXDA32),
      ZPN64Type("KSLL32") -> zpnRtypeOpWithRd(KSLL32),
      ZPN64Type("KSLLI32") -> zpnRtypeOpWithRd(KSLLI32),
      ZPN64Type("KSLRA32") -> zpnRtypeOpWithRd(KSLRA32),
      ZPN64Type("KSLRA32_U") -> zpnRtypeOpWithRd(KSLRA32_U),
      ZPN64Type("KSTAS32") -> zpnRtypeOpWithRd(KSTAS32),
      ZPN64Type("KSTSA32") -> zpnRtypeOpWithRd(KSTSA32),
      ZPN64Type("KSUB32") -> zpnRtypeOpWithRd(KSUB32),
      ZPN64Type("PKBB16") -> zpnRtypeOpWithRd(PKBB16),
//      ZPN64Type("PKBT32") -> zpnRtypeOpWithRd(PKBT32),
//      ZPN64Type("PKTB32") -> zpnRtypeOpWithRd(PKTB32),
      ZPN64Type("PKTT16") -> zpnRtypeOpWithRd(PKTT16),
      ZPN64Type("RADD32") -> zpnRtypeOpWithRd(RADD32),
      ZPN64Type("RCRAS32") -> zpnRtypeOpWithRd(RCRAS32),
      ZPN64Type("RCRSA32") -> zpnRtypeOpWithRd(RCRSA32),
      ZPN64Type("RSTAS32") -> zpnRtypeOpWithRd(RSTAS32),
      ZPN64Type("RSTSA32") -> zpnRtypeOpWithRd(RSTSA32),
      ZPN64Type("RSUB32") -> zpnRtypeOpWithRd(RSUB32),
      ZPN64Type("SLL32") -> zpnRtypeOpWithRd(SLL32),
      ZPN64Type("SLLI32") -> zpnRtypeOpWithRd(SLLI32),
      ZPN64Type("SMAX32") -> zpnRtypeOpWithRd(SMAX32),
//      ZPN64Type("SMBT32") -> zpnRtypeOpWithRd(SMBT32),
      ZPN64Type("SMDRS32") -> zpnRtypeOpWithRd(SMDRS32),
      ZPN64Type("SMDS32") -> zpnRtypeOpWithRd(SMDS32),
      ZPN64Type("SMIN32") -> zpnRtypeOpWithRd(SMIN32),
      ZPN64Type("SMMUL") -> zpnRtypeOpWithRd(SMMUL),
//      ZPN64Type("SMTT32") -> zpnRtypeOpWithRd(SMTT32),
      ZPN64Type("SMXDS32") -> zpnRtypeOpWithRd(SMXDS32),
      ZPN64Type("SRA32") -> zpnRtypeOpWithRd(SRA32),
      ZPN64Type("SRA32_U") -> zpnRtypeOpWithRd(SRA32_U),
      ZPN64Type("SRAI32") -> zpnRtypeOpWithRd(SRAI32),
      ZPN64Type("SRAI32_U") -> zpnRtypeOpWithRd(SRAI32_U),
      ZPN64Type("SRAI_U") -> zpnRtypeOpWithRd(SRAI_U),
      ZPN64Type("SRAIW_U") -> zpnRtypeOpWithRd(SRAIW_U),
      ZPN64Type("SRL32") -> zpnRtypeOpWithRd(SRL32),
      ZPN64Type("SRL32_U") -> zpnRtypeOpWithRd(SRL32_U),
      ZPN64Type("SRLI32") -> zpnRtypeOpWithRd(SRLI32),
      ZPN64Type("SRLI32_U") -> zpnRtypeOpWithRd(SRLI32_U),
      ZPN64Type("STAS32") -> zpnRtypeOpWithRd(STAS32),
      ZPN64Type("STSA32") -> zpnRtypeOpWithRd(STSA32),
      ZPN64Type("SUB32") -> zpnRtypeOpWithRd(SUB32),
      ZPN64Type("UKADD32") -> zpnRtypeOpWithRd(UKADD32),
      ZPN64Type("UKCRAS32") -> zpnRtypeOpWithRd(UKCRAS32),
      ZPN64Type("UKCRSA32") -> zpnRtypeOpWithRd(UKCRSA32),
      ZPN64Type("UKSTAS32") -> zpnRtypeOpWithRd(UKSTAS32),
      ZPN64Type("UKSTSA32") -> zpnRtypeOpWithRd(UKSTSA32),
      ZPN64Type("UKSUB32") -> zpnRtypeOpWithRd(UKSUB32),
      ZPN64Type("UMAX32") -> zpnRtypeOpWithRd(UMAX32),
      ZPN64Type("UMIN32") -> zpnRtypeOpWithRd(UMIN32),
      ZPN64Type("URADD32") -> zpnRtypeOpWithRd(URADD32),
      ZPN64Type("URCRAS32") -> zpnRtypeOpWithRd(URCRAS32),
      ZPN64Type("URCRSA32") -> zpnRtypeOpWithRd(URCRSA32),
      ZPN64Type("URSTAS32") -> zpnRtypeOpWithRd(URSTAS32),
      ZPN64Type("URSTSA32") -> zpnRtypeOpWithRd(URSTSA32),
      ZPN64Type("URSUB32") -> zpnRtypeOpWithRd(URSUB32),
    )
  }

  def ZBBExtDecodingList = {
    import Instructions.ZBBType
    import PExtensionOperation._
    Seq(
//      ZBBType("ANDN") -> zpnRtypeOpWithRd(ANDN),
//      ZBBType("CLZ") -> zpnRtypeOpWithRd(CLZ),
//      ZBBType("CPOP") -> zpnRtypeOpWithRd(CPOP),
//      ZBBType("CTZ") -> zpnRtypeOpWithRd(CTZ),
      ZBBType("MAX") -> zpnRtypeOpWithRd(MAX),
//      ZBBType("MAXU") -> zpnRtypeOpWithRd(MAXU),
      ZBBType("MIN") -> zpnRtypeOpWithRd(MIN),
//      ZBBType("MINU") -> zpnRtypeOpWithRd(MINU),
//      ZBBType("ORC_B") -> zpnRtypeOpWithRd(ORC_B),
//      ZBBType("ORN") -> zpnRtypeOpWithRd(ORN),
//      ZBBType("ROL") -> zpnRtypeOpWithRd(ROL),
//      ZBBType("ROR") -> zpnRtypeOpWithRd(ROR),
//      ZBBType("SEXT_B") -> zpnRtypeOpWithRd(SEXT_B),
//      ZBBType("SEXT_H") -> zpnRtypeOpWithRd(SEXT_H),
//      ZBBType("XNOR") -> zpnRtypeOpWithRd(XNOR)
    )
  }

  def ZBTExtDecodingList = {
    import Instructions.ZBTType
    import PExtensionOperation._
    Seq(
      ZBTType("CMIX") -> rtypeWithR3(CMIX),
      ZBTType("CMOV") -> createOperation(),
      ZBTType("FSL") -> createOperation(),
      ZBTType("FSR") -> createOperation(),
    )
  }

  def ZPSFExtDecodingList = {
    import Instructions.ZPSFType
    import PExtensionOperation._
    Seq(
      ZPSFType("KADD64") -> zpnRtypeOpWithRd(KADD64),
      ZPSFType("KMAR64") -> zpnRtypeOpWithRd(KMAR64),
      ZPSFType("KMSR64") -> zpnRtypeOpWithRd(KMSR64),
      ZPSFType("KSUB64") -> zpnRtypeOpWithRd(KSUB64),
      ZPSFType("MULR64") -> zpnRtypeOpWithRd(MULR64),
      ZPSFType("MULSR64") -> zpnRtypeOpWithRd(MULSR64),
      ZPSFType("RADD64") -> zpnRtypeOpWithRd(RADD64),
      ZPSFType("RSUB64") -> zpnRtypeOpWithRd(RSUB64),
      ZPSFType("SMAL") -> zpnRtypeOpWithRd(SMAL),
      ZPSFType("SMALBB") -> zpnRtypeOpWithRd(SMALBB),
      ZPSFType("SMALBT") -> zpnRtypeOpWithRd(SMALBT),
      ZPSFType("SMALDA") -> zpnRtypeOpWithRd(SMALDA),
      ZPSFType("SMALDRS") -> zpnRtypeOpWithRd(SMALDRS),
      ZPSFType("SMALDS") -> zpnRtypeOpWithRd(SMALDS),
      ZPSFType("SMALTT") -> zpnRtypeOpWithRd(SMALTT),
      ZPSFType("SMALXDA") -> zpnRtypeOpWithRd(SMALXDA),
      ZPSFType("SMALXDS") -> zpnRtypeOpWithRd(SMALXDS),
      ZPSFType("SMAR64") -> zpnRtypeOpWithRd(SMAR64),
      ZPSFType("SMSLDA") -> zpnRtypeOpWithRd(SMSLDA),
      ZPSFType("SMSLXDA") -> zpnRtypeOpWithRd(SMSLXDA),
      ZPSFType("SMSR64") -> zpnRtypeOpWithRd(SMSR64),
      ZPSFType("SMUL16") -> zpnRtypeOpWithRd(SMUL16),
      ZPSFType("SMUL8") -> zpnRtypeOpWithRd(SMUL8),
      ZPSFType("SMULX16") -> zpnRtypeOpWithRd(SMULX16),
      ZPSFType("SMULX8") -> zpnRtypeOpWithRd(SMULX8),
      ZPSFType("UKADD64") -> zpnRtypeOpWithRd(UKADD64),
      ZPSFType("UKMAR64") -> zpnRtypeOpWithRd(UKMAR64),
      ZPSFType("UKMSR64") -> zpnRtypeOpWithRd(UKMSR64),
      ZPSFType("UKSUB64") -> zpnRtypeOpWithRd(UKSUB64),
      ZPSFType("UMAR64") -> zpnRtypeOpWithRd(UMAR64),
      ZPSFType("UMSR64") -> zpnRtypeOpWithRd(UMSR64),
      ZPSFType("UMUL16") -> zpnRtypeOpWithRd(UMUL16),
      ZPSFType("UMUL8") -> zpnRtypeOpWithRd(UMUL8),
      ZPSFType("UMULX16") -> zpnRtypeOpWithRd(UMULX16),
      ZPSFType("UMULX8") -> zpnRtypeOpWithRd(UMULX8),
      ZPSFType("URADD64") -> zpnRtypeOpWithRd(URADD64),
      ZPSFType("URSUB64") -> zpnRtypeOpWithRd(URSUB64),
    )
  }

//  def ZPN32ExtDecodingList = {
//    import Instructions.ZPN64Type
//    import PExtensionOperation._
//    Seq(
//      ZBPType("GORC") -> zpnRtypeOpWithRd(GORC),
//      ZBPType("GREV") -> zpnRtypeOpWithRd(GREV),
//      ZBPType("PACKU") -> zpnRtypeOpWithRd(PACKU),
//      ZBPType("SHFL") -> zpnRtypeOpWithRd(SHFL),
//      ZBPType("UNSHFL") -> zpnRtypeOpWithRd(UNSHFL),
//      ZBPType("XPERM16") -> zpnRtypeOpWithRd(XPERM16),
//      ZBPType("XPERM4") -> zpnRtypeOpWithRd(XPERM4),
//      ZBPType("XPERM8") -> zpnRtypeOpWithRd(XPERM8)
//    )}

  def decodingList(implicit
    params: Parameters,
  ): Seq[(BitPat, (UInt, UInt) => Operations)] = {
    val output =
      Seq(BaseDecodingList, AextDecodingList) ++ (if (params.enablePExt)
                                                    Seq(
                                                      ZPN32ExtDecodingList,
                                                      ZPN64ExtDecodingList,
                                                      ZPSFExtDecodingList,
                                                      ZBBExtDecodingList,
                                                      ZBTExtDecodingList,
                                                    )
                                                  else Seq())
    output.flatten
  }

  def genDecoder(inst: UInt, pc: UInt)(implicit
    params: Parameters,
  ): Operations = {
    // strategy1
    MuxCase(
      Operations.default,
      decodingList.map { case (a, b) => (a === inst) -> b(inst, pc) },
    )
    // strategy 2
//    val idx = MuxCase(
//      decodingList.length.U,
//      decodingList.map(_._1 === inst).zipWithIndex.map { case (cond, i) =>
//        cond -> i.U
//      }
//    )
//    val ops = VecInit(decodingList.map(_._2(inst, pc)) :+ Operations.default)
//    ops(idx)

    // strategy3
//    val idx =
//      decodingList
//        .map(_._1 === inst)
//        .zipWithIndex
//        .map { case (cond, i) =>
//          Mux(cond, (i + 1).U, 0.U)
//        }
//        .reduce(_ | _)
//
//    val ops = VecInit(
//      Seq(Operations.default) ++ decodingList.map(_._2(inst, pc))
//    )
//    ops(idx)

    // strategy4
//    val idxWidth = log2Up(decodingList.length)
//    val table = TruthTable(
//      decodingList.map(_._1).zipWithIndex.map { case (a, b) =>
//        a -> BitPat((b + 1).U(idxWidth.W))
//      },
//      BitPat(0.U(idxWidth.W))
//    )
//    val idx = decoder(inst, table)
//    val ops = VecInit(
//      Seq(Operations.default) ++ decodingList.map(_._2(inst, pc))
//    )
//    ops(idx)
  }
}

class DecodingMod(implicit params: Parameters) extends Module {
  val input = IO(Input(UInt(32.W)))
  val pc = IO(Input(UInt(64.W)))
  val out = IO(Output(new Operations()))
  out := Operations.genDecoder(input, pc)

  when(out.aluOp.valid) {
    for (a <- ALUOperation.all) {
      cover(out.aluOp.bits === a)
    }
  }

  if (params.enablePExt) {
    when(out.pextOp.valid) {
      for (p <- PExtensionOperation.all) {
        cover(out.pextOp.bits === p)
      }
    }
  }
}

object DecodingMod {
  def apply(instruction: UInt, programCounter: UInt)(implicit
    params: Parameters,
  ): Operations = {
    val m = Module(new DecodingMod())
    m.input := instruction
    m.pc := programCounter
    m.out
  }
}

object OperationDecoderApp extends App {
  implicit val params = Parameters()
  ChiselStage.emitSystemVerilogFile(
    new DecodingMod,
    firtoolOpts = Array(
      "--lowering-options=disallowLocalVariables,disallowPackedArrays,noAlwaysComb",
    ),
  )
}

class OptionalBundle[T <: Data](t: T) extends Bundle {
  val valid = Bool()
  val bits = t.cloneType

  def isValid: Bool = valid
  def validDataOrZero: T = Mux(valid, bits, 0.U.asTypeOf(t))
  def validDataOr(t2: T): T = Mux(valid, bits, t2)
}

object OptionalBundle {
  def apply[T <: Data](t: T): OptionalBundle[T] = new OptionalBundle(t)
  def valid[T <: Data](t: T): OptionalBundle[T] = {
    val w = Wire(new OptionalBundle(t))
    w.bits := t
    w.valid := true.B
    w
  }

  def invalid[T <: Data](t: T): OptionalBundle[T] = {
    val w = Wire(new OptionalBundle(t))
    w.bits := DontCare
    w.valid := false.B
    w
  }
}

object ALUOperation extends ChiselEnum {
  val BranchEqual, BranchNotEqual, BranchLessThan, BranchGreaterThanOrEqual,
    BranchLessThanUnsigned, BranchGreaterThanOrEqualUnsigned, Add, Sub, And, Or,
    Slt, Sltu, Xor, Sll, Srl, Sra, AddJALR, AddJAL, AddW, SllW, SrlW, SraW,
    SubW, Div, Mul, Rem = Value
}

object LoadStoreOperation extends ChiselEnum {
  val Load, LoadUnsigned, Store = Value
}

object AMOOperation extends ChiselEnum {
  val Add, And, Max, MaxU, Min, MinU, Or, Swap, Xor, Lr, Sc = Value
}

object AMOOperationWidth extends ChiselEnum {
  val Word, DoubleWord = Value
}

class AMOOrdering extends Bundle {
  val aquire = Bool()
  val release = Bool()
}

object AMOOrdering {
  def apply(aq: Bool, rl: Bool) = {
    val w = Wire(new AMOOrdering)
    w.aquire := aq
    w.release := rl
    w
  }
}

object LoadStoreWidth extends ChiselEnum {
  val Byte, HalfWord, Word, DoubleWord = Value
}

object CSROperation extends ChiselEnum {
  val ReadWrite, ReadSet, ReadClear = Value
}

object SendReceiveOperation extends ChiselEnum { //added by akamatsu
  val Send, Receive = Value
}

package b4processor.modules.cache

import b4processor.Parameters
import b4processor.modules.memory.{MemoryAccessChannels, MemoryReadRequest, MemoryReadResponse, MemoryWriteRequest, MemoryWriteRequestData, MemoryWriteResponse}
import b4processor.structures.memoryAccess.MemoryAccessWidth
import b4processor.utils.Tag
import chisel3._
import chisel3.util._
import chisel3.util.Queue

class DirectMappedCache(implicit params: Parameters) extends Module {
  val io = IO(new Bundle {
    val core = Flipped(new MemoryAccessChannels())
    val externalMemoryInterface = new MemoryAccessChannels()
  })
  // IOのデフォルト値設定
  io.externalMemoryInterface.read.request.valid := false.B
  io.core.read.response.bits := 0.U.asTypeOf(new MemoryReadResponse())

  val numentries = params.numentries
  // キャッシュのデータ構造
  val cacheEntries = RegInit(VecInit(Seq.fill(numentries)(0.U(64.W)))) // キャッシュライン doubleWord(8 Byte) per line
  val tagArray = RegInit(VecInit(Seq.fill(numentries)(0.U((64 - log2Ceil(numentries) - 3).W)))) // タグ
  val validBits = RegInit(VecInit(Seq.fill(numentries)(false.B))) // 有効ビット

  // アドレスからindex, tag, offsetを抽出する関数
  def indexFromAddress(address: UInt) = address(log2Ceil(numentries) + 2, 3)
  def tagFromAddress(address: UInt) = address(63, log2Ceil(numentries) + 3)
  def offsetFromAddress(address: UInt) = address(2, 0)
  // ヒット判定をする関数
  def cacheHit(index: UInt, tag: UInt) = validBits(index) && (tagArray(index) === tag)

  // 読み込みリクエストのアドレス分解、サイズ抽出、ヒット判定
  val readReqAddress = WireInit(0.U(64.W))
    readReqAddress := io.core.read.request.bits.address
  val readReqIndex = WireInit(0.U(log2Ceil(numentries).W))
    readReqIndex := indexFromAddress(readReqAddress)
  val readReqTag = WireInit(0.U((64 - log2Ceil(numentries) - 3).W))
    readReqTag := tagFromAddress(readReqAddress)
  val readReqOffset = WireInit(0.U(3.W))
    readReqOffset := offsetFromAddress(readReqAddress)
  val readReqSize = WireInit(0.U.asTypeOf(new MemoryAccessWidth.Type))
    readReqSize := io.core.read.request.bits.size
  val readReqHit = WireInit(false.B)
    readReqHit := cacheHit(readReqIndex, readReqTag) & io.core.read.request.valid

  // 書き込みリクエストのアドレス分解、ヒット判定
  val writeReqIndex = WireInit(0.U(log2Ceil(numentries).W))
    writeReqIndex := indexFromAddress(io.core.write.request.bits.address)
  val writeReqTag = WireInit(0.U((64 - log2Ceil(numentries) - 3).W))
    writeReqTag := tagFromAddress(io.core.write.request.bits.address)
  val writeReqHit = WireInit(false.B)
    writeReqHit := cacheHit(writeReqIndex, writeReqTag) & io.core.write.request.valid

  // キャッシュミス時のreadRequestの内容を保存するキュー
  class ReadRequestOption extends Bundle {
    val cacheIndex = UInt(log2Ceil(numentries).W)
    val cacheTag = UInt((64 - log2Ceil(numentries) - 3).W)
    val offset = UInt(3.W)
    val size = new MemoryAccessWidth.Type()
    val outputTag = new Tag()
  }
  val dataReadRequestOptionQueue = Module(new Queue(new ReadRequestOption, 8))
  dataReadRequestOptionQueue.io.enq.bits.cacheIndex := readReqIndex
  dataReadRequestOptionQueue.io.enq.bits.cacheTag := readReqTag
  dataReadRequestOptionQueue.io.enq.bits.offset := readReqOffset
  dataReadRequestOptionQueue.io.enq.bits.size := readReqSize
  dataReadRequestOptionQueue.io.enq.bits.outputTag := io.core.read.request.bits.outputTag
  dataReadRequestOptionQueue.io.enq.valid := false.B

  // coreへのreadResponseを格納するキューと出力を選択するアービタ
  val cacheHitQueue = Module(new Queue(new MemoryReadResponse(), 8))
  cacheHitQueue.io.enq.bits := 0.U.asTypeOf(new MemoryReadResponse())
  cacheHitQueue.io.enq.valid := false.B
  val memReadRespQueue = Module(new Queue(new MemoryReadResponse(), 8))
  memReadRespQueue.io.enq.bits := 0.U.asTypeOf(new MemoryReadResponse())
  memReadRespQueue.io.enq.valid := false.B
  val readRespArbiter = Module(new Arbiter(new MemoryReadResponse(), 2))
  readRespArbiter.io.in(0) <> cacheHitQueue.io.deq
  readRespArbiter.io.in(1) <> memReadRespQueue.io.deq
  readRespArbiter.io.out.ready := io.core.read.response.ready
  io.core.read.response.valid := readRespArbiter.io.out.valid
  when(readRespArbiter.io.out.valid){
    io.core.read.response.bits := readRespArbiter.io.out.bits
  }

  /** 書き込み処理 */
  val HitWriteData = WireInit(0.U(64.W))
  // ライトスルーなので要求とレスポンスを素通りさせる
  io.externalMemoryInterface.write.request <> io.core.write.request
  io.externalMemoryInterface.write.requestData <> io.core.write.requestData
  io.externalMemoryInterface.write.response <> io.core.write.response

  val writeReqMask = io.core.write.requestData.bits.mask
  val writeData = io.core.write.requestData.bits.data
  val writeReqValid = io.core.write.request.valid & io.core.write.requestData.valid
  //ヒットした場合にキャッシュラインを更新
  when(writeReqValid & writeReqHit){
    HitWriteData := cacheEntries(writeReqIndex)
    val allOnes = WireInit(255.U) // 8'b11111111
    val newData = WireInit(0.U(64.W))
    // マスクを適用したデータを作成
    newData := (HitWriteData & (writeReqMask ^ allOnes)) | (writeData & writeReqMask)
    cacheEntries(writeReqIndex) := newData
  }

  /** 読み込み処理 */
  // キャッシュミスした際はexternalMemoryInterface に対してキャッシュラインごとのrequestを送る
  val externalRequest = WireInit(0.U.asTypeOf(new MemoryReadRequest()))
  externalRequest.address := readReqAddress & Cat(Fill(61, 1.U), 0.U(3.W)) // offsetを0に変更
  externalRequest.size := MemoryAccessWidth.DoubleWord // ダブルワードサイズでリクエスト
  externalRequest.outputTag := io.core.read.request.bits.outputTag
  io.externalMemoryInterface.read.request.bits := externalRequest

  io.core.read.request.ready := io.externalMemoryInterface.read.request.ready & dataReadRequestOptionQueue.io.enq.ready & cacheHitQueue.io.enq.ready & !io.externalMemoryInterface.read.response.bits.isError
  io.externalMemoryInterface.read.response.ready := memReadRespQueue.io.enq.ready

  val cacheData = WireInit(0.U(64.W)) // キャッシュから読み出したデータ
  val shiftedHitData = WireInit(0.U(64.W)) // オフセット分シフトしたデータ
  val readData = WireInit(0.U(64.W)) // シフトし、データサイズ分だけ抽出されたデータ
  val HitResponse = WireInit(0.U.asTypeOf(new MemoryReadResponse()))

  when(io.core.read.request.valid & io.externalMemoryInterface.read.request.ready & cacheHitQueue.io.enq.ready){
    // キャッシュミスの処理
    when(!readReqHit) {
      // 外部メモリインタフェースへrequest送信
      io.externalMemoryInterface.read.request.valid := true.B
      // data復元に必要な情報をエンキュー
      dataReadRequestOptionQueue.io.enq.valid := true.B
    }.elsewhen(readReqHit){
      // キャッシュのヒット時の処理
      cacheData := cacheEntries(readReqIndex)
      shiftedHitData := cacheData >> (readReqOffset * 8.U)
      when(readReqSize === MemoryAccessWidth.Byte) {
        readData := shiftedHitData(7, 0)
      } .elsewhen(readReqSize === MemoryAccessWidth.HalfWord) {
        readData := shiftedHitData(15, 0)
      } .elsewhen(readReqSize === MemoryAccessWidth.Word) {
        readData := shiftedHitData(31, 0)
      } .elsewhen(readReqSize === MemoryAccessWidth.DoubleWord) {
        readData := cacheData
      }
      // コアに返すresponseを作成
      HitResponse.value := readData
      HitResponse.burstIndex := 0.U
      HitResponse.isError := false.B
      HitResponse.tag := io.core.read.request.bits.outputTag
      // コアにresponseを返す
      cacheHitQueue.io.enq.bits := HitResponse
      cacheHitQueue.io.enq.valid := true.B
    }
  }

  // メモリからのレスポンス処理
  val RRPdata = WireInit(0.U(64.W))
  val queueData = dataReadRequestOptionQueue.io.deq.bits
  val RRPindex = queueData.cacheIndex
  val RRPtag = queueData.cacheTag
  val RRPoffset = queueData.offset
  val RRPsize = queueData.size

  dataReadRequestOptionQueue.io.deq.ready := io.externalMemoryInterface.read.response.valid & (io.externalMemoryInterface.read.response.bits.tag === dataReadRequestOptionQueue.io.deq.bits.outputTag)

  val shiftedRRPdata = WireInit(0.U(64.W))
  val responceReadData = WireInit(0.U(64.W))
  val MissResponse = WireInit(0.U.asTypeOf(new MemoryReadResponse()))

  when(io.externalMemoryInterface.read.response.valid ) {
    RRPdata := io.externalMemoryInterface.read.response.bits.value
    when(io.externalMemoryInterface.read.response.bits.tag === dataReadRequestOptionQueue.io.deq.bits.outputTag) {
      when(io.externalMemoryInterface.read.response.bits.isError) {
        // readRequestのアドレスが無効だったときに再送
        io.externalMemoryInterface.read.request.valid := true.B
        io.externalMemoryInterface.read.request.bits.address := Cat(RRPtag, RRPindex, RRPoffset)
        io.externalMemoryInterface.read.request.bits.size := RRPsize
        io.externalMemoryInterface.read.request.bits.outputTag := queueData.outputTag
        io.externalMemoryInterface.read.request.bits.signed := false.B
        io.externalMemoryInterface.read.request.bits.burstLength := 1.U
      }.elsewhen(cacheHitQueue.io.enq.ready) {
        // キャッシュへの書込
        cacheEntries(RRPindex) := RRPdata
        tagArray(RRPindex) := RRPtag
        validBits(RRPindex) := true.B
        // readResponseの作成
        shiftedRRPdata := RRPdata >> (RRPoffset * 8.U)
        when(RRPsize === MemoryAccessWidth.Byte) {
          responceReadData := shiftedRRPdata(7, 0)
        }.elsewhen(RRPsize === MemoryAccessWidth.HalfWord) {
          responceReadData := shiftedRRPdata(15, 0)
        }.elsewhen(RRPsize === MemoryAccessWidth.Word) {
          responceReadData := shiftedRRPdata(31, 0)
        }.elsewhen(RRPsize === MemoryAccessWidth.DoubleWord) {
          responceReadData := RRPdata
        }
        MissResponse.value := responceReadData
        MissResponse.burstIndex := 0.U
        MissResponse.isError := false.B
        MissResponse.tag := queueData.outputTag
      }
    }.otherwise{
      // 再送リクエストのレスポンスを作成
      MissResponse := io.externalMemoryInterface.read.response.bits
    }
    // responseの出力
    memReadRespQueue.io.enq.bits := MissResponse
    memReadRespQueue.io.enq.valid := true.B
  }
}


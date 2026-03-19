# B4Processor-MP SendReceiveQueue2 (SRQ) ドキュメント

## 1. 概要
SendReceiveQueue2 (以下、SRQ) は、マルチスレッド環境 (B4Processor-MP) において**スレッド間でデータを直接交換する**ためのキュー（待ち行列）モジュールです。
各スレッドに1つずつインスタンス化され、スレッド間で `Send` (送信) 命令と `Receive` (受信) 命令を連携させることで、スレッド間通信を実現します。

---

## 2. IOインタフェースと接続先
SRQの主要な入出力ポートと、`B4Processor.scala`内での接続先は以下の通りです。

### 2.1 デコーダからの入力 (`io.decoders`)
- **役割:** デコーダから発行された `Send` または `Receive` 命令を受け取り、内部のキュー(`sendbuffer` または `receivebuffer`)に格納します。
- **接続:** 各デコーダの `io.sendReceiveQueue` と直結しています。

### 2.2 オペランドバイパス (`io.collectedOutput`)
- **役割:** 命令の発行時点では判明していなかった「送信する値」や「通信先のスレッドID」を、実行ユニット(ALUなど)の演算完了時に **スヌープ（横取り）** してキュー内のエントリを更新するための入力です。
- **接続:** 自スレッドの OutputCollector の出力 (`outputCollector.io.outputs`) と接続されます。

### 2.3 受信データの出力 (`io.recevedData`)
- **役割:** 他のスレッドからデータを受信できた際、そのデータをリオーダバッファ(ROB)やレジスタに書き戻すために出力します。
- **接続:** 自スレッドの OutputCollector の入力 (`outputCollector.io.sendReceiveQueue`) と接続されます。

### 2.4 スレッド間通信ポート (`io.requester` / `io.responser`)
- **型:** `Vec(params.threads, new MPQ2MPQ())`
- **役割:** 他のスレッドに対して「データをくれ」と要求(Request)を出したり、その要求に応じたデータ(Response)を相手に返すための双方向ポートです。
- **接続:** `B4Processor.scala`において、全てのスレッドのSRQ同士が **All-to-All (全結合) のクロスバスイッチ**のように直接結ばれています。
  ```scala
  // 該当する接続コード (B4Processor.scala)
  for(t <- 0 until params.threads){
      sendReceiveQueue(tid).io.responser(t).request := sendReceiveQueue(t).io.requester(tid).request
      sendReceiveQueue(tid).io.requester(t).response := sendReceiveQueue(t).io.responser(tid).response
  }
  ```

---

## 3. スレッド間データ交換の仕組み (内部動作フロー)
SRQは、内部で **送信キュー (`sendbuffer`)** と **受信キュー (`receivebuffer`)** を独立して管理しており、古い命令から順番(FIFO)に処理を行います。

### Step 1. 命令のエンキューとオペランドの待機
- プログラム中で `Send` または `Receive` 命令が発行されると、SRQ内の対応するキューに情報が登録されます。
- エンキュー直後は、「送信するデータ(数値)」や「通信相手のスレッドID」がまだ手元のレジスタに無い場合があります。
- SRQは `io.collectedOutput` を監視し続け、必要なデータが該当のタグ(ID)と共に流れてきた瞬間に、キュー内部のエントリを更新します (**オペランドバイパス**)。

### Step 2. データ交換のハンドシェイク (Request と Response)
データの交換は、**Receive側が「Request（データ要求）」を出し、Send側が条件に合致すれば「Response（データ送信）」を返す**、という手順で行われます。

1. **Receive側からの要求 (Request)**
   - 自スレッドの受信キューの先頭 (`receiveTail`) にある命令が対象です。
   - 「どこのスレッドから受信するか」が判明した場合、対象となるスレッドに対して `requester[対象スレッド].request.valid` を `true` にし、要求先のスレッドIDを含めてリクエストを送ります。

2. **Send側での応答判断 (Response)**
   - もう一方のスレッドの送信キューの先頭 (`sendTail`) にある命令が待機しています。
   - ここでは、「送信するデータ本体」と「送信先のスレッドID」が両方とも準備完了（Valid）になっている必要があります。
   - そこへ、Receive側のスレッドから Request が飛んできた場合、**「自分がこれから送信しようとしている相手ID」と「要求してきた相手のID」が一致**すれば、通信成立（ハンドシェイク完了）とみなします。
   - 通信が成立すると、Send側は `responser[要求元スレッド].response.valid` を `true` にし、データ(`SendData`)を乗せて返信します。その後、送信完了として自分のキューを一つ進めます。

3. **Receive側のデータ受け取り**
   - Receive側は、要求を出した相手から Response (`valid = true`) が返ってくるのを検知すると、中身のデータを受け取ります。
   - このデータを `io.recevedData` から OutputCollector に流し、レジスタ書き戻し等を行います。その後、受信完了として自分のキューを一つ進めます。

---

## 4. 実行の制御 (Stall制御)
- SRQは内部に `empty` (空) と `full` (満杯) の信号を持っています。
- `full` 信号は Fetch・Decode ステージに繋がり、キューがいっぱいの間はプロセッサのフェッチを停止（ストール）させることで溢れを防いでいます。
- `empty` 信号も状態管理に使われており、命令完了の判断基準などに用いられます。

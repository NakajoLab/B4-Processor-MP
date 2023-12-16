#define thread_num 2

long lr_w(long rd, long rs1) {
    // lr.w命令でロードと同時に条件付きロックを確立
    asm volatile("lr.w %0, (%1)"
                 : "=r" (rd)   // 出力オペランド: 読み込んだ値
                 : "r" (&rs1));  // 入力オペランド: アドレス
    return rd;
}

void sc_w(long rs1, long rs2) {
     long result;
     // 条件付きロックが確立されたら、値をインクリメントしてストア
     do {
         // sc.w命令で条件付きでストア
         asm volatile("sc.w %0, %1, (%2)"
                      : "=r" (result)   // 出力オペランド: 成功したかどうか (0: 成功, 1: 失敗)
                      : "r" (rs1), "r" (&rs2));  // 入力オペランド: ストアする値, アドレス
     } while (result);  // 条件付きストアが失敗した場合は繰り返し
}

long thread0(int data[], int result[]){
    long i,l;
    i = data[0] + data[1];
    return i;
}

long thread1(int data[], int result[]){
    long i,l;
    l = lr_w(l, data[2]);
    i = l + data[3];
    sc_w(i, data[2]);
    return i;
}


long main(long loop_count){
    long r;
    long s[thread_num];
    int data[10] = {1,2,3,4,5,6,7,8,9,10};
    int result[10];
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if(tid == 0){
        s[0] = thread0(data, result);
    }else{
        s[1] = thread1(data, result);
    }
    r = s[0] + s[1];
    return r;
}
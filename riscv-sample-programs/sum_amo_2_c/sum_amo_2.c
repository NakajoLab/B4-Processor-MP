#define ROOP_NUM 16
int res[10];
int r0=0, r1=0;

void atomic_increment(int add_value) {
    int result;
    int new_value = 0;

    do {
    // lr命令でロードと同時に条件付きロックを確立
        asm volatile("lr.w %0, (%1)"
                     : "=r" (result)   // 出力オペランド: 読み込んだ値
                     : "r" (&res[0]));  // 入力オペランド: カウンタのアドレス

        // 条件付きロックが確立されたら、値をインクリメントしてストア
        new_value = result + add_value;

        // sc命令で条件付きでストア
        asm volatile("sc.w %0, %1, (%2)"
                     : "=r" (result)   // 出力オペランド: 成功したかどうか (0: 成功, 1: 失敗)
                     : "r" (new_value), "r" (&res[0]));  // 入力オペランド: 新しい値, カウンタのアドレス
    } while (result);  // 条件付きストアが失敗した場合は繰り返し
}


long thread0(int data[]){
    int i,l,n;
    for(n=0; n<ROOP_NUM; n++){
        i = data[4*n] + data[4*n+1];
        atomic_increment(i);
    }
    return 1;
}

long thread1(int data[]){
    int i,l,n;
    for(n=0; n<ROOP_NUM; n++){
        i = data[4*n+2] + data[4*n+3];
        atomic_increment(i);
    }
    return 1;
}


long main(long loop_count){

    int data[64] = { 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,
                    17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                    33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
                    49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64};
    int check=1;
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if(tid == 0){
        r0 = thread0(data);
    }else{
        r1 = thread1(data);
    }

    res[0] += 10;
    return res[0];
}
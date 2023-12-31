#define S_SIZE 128
#define ROW 128
#define COLUMN 128
#define E_SIZE 128
volatile int check_flag = 0;
int data1[S_SIZE] =   { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
int data2[S_SIZE] =   { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
                     // 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
int output[ROW][E_SIZE];

void atomic_increment(int add_value ,int row, int column) {
    int result;
    int new_value;
    do {
    // lr命令でロードと同時に条件付きロックを確立
        asm volatile("lr.w %0, (%1)"
                     : "=r" (result)   // 出力オペランド: 読み込んだ値
                     : "r" (&output[row][column]));  // 入力オペランド: カウンタのアドレス
        // 条件付きロックが確立されたら、値をインクリメントしてストア
        new_value = result + add_value;
        // sc命令で条件付きでストア
        asm volatile("sc.w %0, %1, (%2)"
                     : "=r" (result)   // 出力オペランド: 成功したかどうか (0: 成功, 1: 失敗)
                     : "r" (new_value), "r" (&output[row][column]));  // 入力オペランド: 新しい値, カウンタのアドレス
    } while (result);  // 条件付きストアが失敗した場合は繰り返し
}

void atomic_set(int set_value) {
    int result;
    do {
    // lr命令でロードと同時に条件付きロックを確立
        asm volatile("lr.w %0, (%1)"
                     : "=r" (result)   // 出力オペランド: 読み込んだ値
                     : "r" (&valid[0]));  // 入力オペランド: カウンタのアドレス
        // sc命令で条件付きでストア
        asm volatile("sc.w %0, %1, (%2)"
                     : "=r" (result)   // 出力オペランド: 成功したかどうか (0: 成功, 1: 失敗)
                     : "r" (set_value), "r" (&valid[0]));  // 入力オペランド: 新しい値, カウンタのアドレス
    } while (result);  // 条件付きストアが失敗した場合は繰り返し
}

void load_wait() { //何らかの理由でキャッシュ, pcなどの値が無効になる
    volatile int result;
    do{
       asm volatile("lw %0, (%1)"
                    : "=r" (result)   // 出力オペランド: 読み込んだ値
                    : "r" (&valid[0]));  // 入力オペランド: メモリアドレス
       result = result + 0;    //上記のバグ回避用
    }while(!result);
}

void atomic_wait(int dest) {//何らかの理由でキャッシュ, pcなどの値が無効になる
    volatile int result;
    int value = 0;
    do {
        asm volatile("amoswap.w %0, %1, (%2)"
                      : "=r" (result)   // 出力オペランド: スワップ前の値
                      : "r" (value), "r" (&dest));  // 入力オペランド: スワップする値, メモリアドレス
        result = result + 0;    //上記のバグ回避用
    } while (!result);  // 条件付きストアが失敗した場合は繰り返し
}

long thread0(int data1[S_SIZE], int data2[S_SIZE]){
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=0; n<E_SIZE/8; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
    check_flag = 1;
    return check_flag;
}

void thread1(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=E_SIZE/8; n<E_SIZE/4; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

void thread2(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=E_SIZE/4; n<3*E_SIZE/8; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

void thread3(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=3*E_SIZE/8; n<E_SIZE/2; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

void thread4(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=E_SIZE/2; n<5*E_SIZE/8; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

void thread5(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=5*E_SIZE/8; n<3*E_SIZE/4; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

void thread6(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=3*E_SIZE/4; n<7*E_SIZE/8; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

void thread7(int data1[S_SIZE], int data2[S_SIZE]){
    int t=0;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=7*E_SIZE/8; n<E_SIZE; n++){
                i += data1[n]*data2[n];
            }
            atomic_increment(i,l,m);
        }
    }
}

long main(long loop_count){
    long r;
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if      (tid == 0){
        r = thread0(data1, data2);
    }else if(tid == 1){
            thread1(data1, data2);
    }else if(tid == 2){
            thread2(data1, data2);
    }else if(tid == 3){
            thread3(data1, data2);
    }else if(tid == 4){
            thread4(data1, data2);
    }else if(tid == 5){
            thread5(data1, data2);
    }else if(tid == 6){
            thread6(data1, data2);
    }else             {
            thread7(data1, data2);
    }
    r += 10;//確認用の計算
    return r;
}
#define S_SIZE 16
#define ROW    16
#define COLUMN 16
#define E_SIZE 16
volatile int check_flag = 0;
int data1[S_SIZE][S_SIZE] = {{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}};
int data2[S_SIZE][S_SIZE] = {{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
                             { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}};
                            // 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
int output[ROW][COLUMN];

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

void atomic_add(int add_value ,int row, int column) {
    int result;
    // lr命令でロードと同時に条件付きロックを確立
    asm volatile("amoadd.w %0, %1, (%2)"
                 : "=r" (result)   // 出力オペランド: 読み込んだ値
                 : "r" (add_value), "r" (&output[row][column]));  // 入力オペランド: カウンタのアドレス
}

long thread0(){
    for(int l=0; l<ROW/2; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=0; n<E_SIZE/2; n++){
                i += data1[l][n]*data2[n][m];
            }
            atomic_increment(i, l, m);
        }
    }
    check_flag = 1;
    return check_flag;
}

void thread1(){
    for(int l=0; l<ROW/2; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=E_SIZE/2; n<E_SIZE; n++){
                i += data1[l][n]*data2[n][m];
            }
            atomic_increment(i, l, m);
        }
    }
}

void thread2(){
    for(int l=ROW/2; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=0; n<E_SIZE/2; n++){
                i += data1[l][n]*data2[n][m];
            }
            atomic_increment(i, l, m);
        }
    }
}

void thread3(){
    for(int l=ROW/2; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=E_SIZE/2; n<E_SIZE; n++){
                i += data1[l][n]*data2[n][m];
            }
            atomic_increment(i, l, m);
        }
    }
}

long main(long loop_count){
    long r;
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if      (tid == 0){
        r = thread0();
    }else if(tid == 1){
            thread1();
    }else if(tid == 2){
            thread2();
    }else             {
            thread3();
    }
    r += 10;//確認用の計算
    return r;
}
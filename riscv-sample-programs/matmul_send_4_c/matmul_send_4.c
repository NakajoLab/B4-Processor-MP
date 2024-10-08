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

#define send(rs1, rs2, rs3)\
    do { \
    asm volatile(".insn  r4 0b1011011, 0b100, 0b11, %0, %1, %2, %3" \
                 : "=r" (rs1)                                  \
                 : "r"(rs1), "r"(rs2), "r"(rs3));                   \
    } while(0)
     //rd:意味なし、rs1:送信先のスレッドID、rs2:送信する値、rs3:channel

#define receive(rd, rs1, rs2)\
    do { \
    asm volatile(".insn r 0b1011011, 0b100, 0b0000110, %0, %1, %2" \
                 : "=r" (rd)                                       \
                 : "r" (rs1), "r"(rs2));                           \
    } while(0)
    //rd:送信された値、rs1:送信元のスレッドID、rs2:channel

long thread0(){
    int t=1;
    int c=0;
    int j;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=0; n<E_SIZE/4; n++){
                i += data1[l][n]*data2[n][m];
            }
            receive(j, t, c);
            output[l][m] = i + j;
            c += 1;
        }
    }
    return check_flag;
}

void thread1(){
    int t0=2, t1=0;
    int c=0;
    int j;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=E_SIZE/4; n<2*E_SIZE/4; n++){
                i += data1[l][n]*data2[n][m];
            }
            receive(j, t0, c);
            send(t1, i+j, c);
            c += 1;
        }
    }
}

void thread2(){
    int t0=3, t1=1;
    int c=0;
    int j;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=2*E_SIZE/4; n<3*E_SIZE/4; n++){
                i += data1[l][n]*data2[n][m];
            }
            receive(j, t0, c);
            send(t1, i+j, c);
            c += 1;
        }
    }
}

void thread3(){
    int t0=4, t1=2;
    int c=0;
    int j;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=3*E_SIZE/4; n<4*E_SIZE/4; n++){
                i += data1[l][n]*data2[n][m];
            }
            send(t1, i, c);
            c += 1;
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
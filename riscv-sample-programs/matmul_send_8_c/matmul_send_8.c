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

void send(long rs1, long rs2, long rs3){
    long zero;
    asm volatile(".insn  r4 0b0110011, 0b100, 0b11, %0, %1, %2, %3"
                 : "=r" (zero)
                 : "r"(rs1), "r"(rs2), "r"(rs3));
                 //rd:意味なし、rs1:送信先のスレッドID、rs2:送信する値、rs3:channel
}

long receive(long rd, long rs1, long rs2) {
    asm volatile(".insn r 0b0110011, 0b100, 0b0000110, %0, %1, %2"
                 : "=r" (rd)
                 : "r" (rs1), "r"(rs2));
                 //rd:送信された値、rs1:送信元のスレッドID、rs2:channel
    return rd;
}

long thread0(int data1[S_SIZE], int data2[S_SIZE]){
    int t1=1,t2=2,t3=3,t4=4,t5=5,t6=6,t7=7;
    for(int l=0; l<ROW; l++){
        for(int m=0; m<COLUMN; m++){
            int i = 0;
            for(int n=0; n<E_SIZE/8; n++){
                i += data1[n]*data2[n];
            }
            int j1 = receive(j1, t1, m);
            int j2 = receive(j2, t2, m);
            int j3 = receive(j3, t3, m);
            int j4 = receive(j4, t4, m);
            int j5 = receive(j5, t5, m);
            int j6 = receive(j6, t6, m);
            int j7 = receive(j7, t7, m);
            output[l][m] = i + j1 + j2 + j3 + j4 + j5 + j6 + j7;
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
            send(t, i, m);
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
            send(t, i, m);
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
            send(t, i, m);
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
            send(t, i, m);
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
            send(t, i, m);
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
            send(t, i, m);
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
            send(t, i, m);
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
#define ROOP_NUM 2

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

long thread0(int data[]){
    int i,l,t0,t1;
    int n,m;
    long result = 0;
    t0 = 1;
    t1 = 2;
    for(n=0; n<ROOP_NUM; n++){
        i = data[8*n] + data[8*n+1];
        l = receive(l, t0, n);
        result += i + l;
        l = receive(l, t1, n);
        result += l;
    }
    return result;
}

void thread1(int data[]){
    int i,l,t;
    int n;
    t = 0;
    for(n=0; n<ROOP_NUM; n++){
        i = data[8*n+2] + data[8*n+3];
        send(t, i, n);
    }
}

void thread2(int data[]){
    int i,l,t0,t1;
    int n;
    t0 = 3;
    t1 = 0;
    for(n=0; n<ROOP_NUM; n++){
        i = data[8*n+4] + data[8*n+5];
        l = receive(l, t0, n);
        i += l;
        send(t1, i, n);
    }
}

void thread3(int data[]){
    int i,l,t;
    int n;
    t = 2;
    for(n=0; n<ROOP_NUM; n++){
        i = data[8*n+6] + data[8*n+7];
        send(t, i, n);
    }
}

long main(long loop_count){
    long r;
    int data[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if(tid == 0){
        r = thread0(data);
    }else if(tid == 1){
        thread1(data);
    }else if(tid == 2){
        thread2(data);
    }else{
        thread3(data);
    }
    r += 10;//確認用の計算
    return r;
}
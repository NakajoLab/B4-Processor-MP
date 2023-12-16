#define thread_num 2

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
    long i,l,t;
    int n,m;
    long result = 0;
    t = 1;
    for(n=0; n<4; n++){
        i = data[4*n] + data[4*n+1];
        l = receive(l, t, n);
        result += i + l;
    }
    return result;
}

void thread1(int data[]){
    long i,l,t,zero=0;
    int n;
    t = 0;
    for(n=0; n<4; n++){
        i = data[4*n+2] + data[4*n+3];
        send(t, i, n);
    }
}


long main(long loop_count){
    long r=0;
    long t[thread_num];
    int data[16] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if(tid == 0){
        t[0] = thread0(data);
    }else{
        thread1(data);
    }
    r = t[0];
    return r;
}
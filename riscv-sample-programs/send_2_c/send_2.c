#define ROOP_NUM 64

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
    int n;
    long result = 0;
    t = 1;
    for(n=0; n<ROOP_NUM; n++){
        result = receive(result, t, n);
    }
    return result;
}

void thread1(int data[]){
    long i,l,t;
    int n;
    t = 0;
    for(n=0; n<ROOP_NUM; n++){
        i = data[n] ;
        send(t, i, n);
    }
}


long main(long loop_count){
    long r=0;
    int data[64] = { 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,
                    17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                    33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
                    49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64};
    int tid;
    asm volatile("csrr %0, mhartid" : "=r"(tid));
    if(tid == 0){
        r = thread0(data);
    }else{
        thread1(data);
    }
    r += 10;
    return r;
}
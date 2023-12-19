#define ROOP_NUM 32
int data[64] = { 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,
                17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
                49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64};

long thread0(int data[]){
    int n;
    long result = 0;
    for(n=0; n<ROOP_NUM; n++){
        result += data[2*n] + data[2*n+1];
    }
    return result;
}

long main(long loop_count){
    int *ptr = (int*)0x80100100;
    long r = thread0(ptr);
    r += 10;//確認用の計算
    return r;
}
int mul[10];
int div[10];
int rem[10];

void mtype(int data[]){
    for(int i=1; i<10; i++){
        mul[i] = data[i] * data[31 - i];
        div[i] = data[31 - i] / data[i] ;
        rem[i] = data[31 - i] % data[i];

    }
}

long main(long loop_count){
    int data[64] = { 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,
                        17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
                        33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
                        49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64};
    mtype(data);
    return 0;
}
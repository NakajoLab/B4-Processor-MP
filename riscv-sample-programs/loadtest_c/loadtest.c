int data1[5] = {1,2,3,4,5};
int data2[5];

long main(long loop_count){
    long r;
    int data_l[5];
    int i,l;
    for(i=0; i<5; i++){
        data_l[i] = data1[i];
    }
    for(i=0; i<5; i++){
        data_l[i] = data_l[i] + 1;
        data2[i] = data_l[i];
    }
    return r;
}
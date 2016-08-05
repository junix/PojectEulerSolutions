#include<stdio.h>
#include<math.h>

long length,pos,target,x,y,powers[16][115];
char stat[15000],flag;

void init(){
        for(x=2,flag=1;flag;x++){
                for(y=2;;y++){
                        powers[x][y]=pow(y,x);
                        if(12000+x*(y-1)<powers[x][y]){
                                if(y==2) flag=0;
                                break;
                        }
                }
        }
}

int solve(int position, int start, long sum, long product, int p){
        if(position==length){
                if(sum%(product-1)==0){
                        x=sum*product/(product-1);
                        if(x<target) target=x;
                }
                return 1;
        }
        while(1){
                x=product*powers[p][start];
                y=sum+start*p;
                if(x>=target||y>=target||x>y) return 0;
                solve(position+1,start,sum+start,product*start,p-1);
                start++;
        }
}

int main(){
        long sum,product;
        init();
        for(length=2;length<=12000;length++){
                sum=length-2;
                product=1;
                target=2000000000;
                for(pos=sum,flag=1;pos>=0&&flag;pos--,sum--){
                        for(int i=2;;i++){
                                if(powers[length-pos][i]>i*(length-pos)+pos){
                                        if(i==2) flag=0;
                                        break;
                                }
                                solve(pos+2,i,sum+i,product*i,length-pos-1);
                        }
                }
                stat[target]=1;
        }
        for(x=4,sum=0;x<14999;x++){
                if(stat[x]) sum+=x;
        }
        printf("%li\n",sum);
}

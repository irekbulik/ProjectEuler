/*
 * =====================================================================================
 *
 *       Filename:  288.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/17/2018 19:27:25
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 [0] g++ -O3 -o 288.x 288.cpp ; time ./288.x
Answer 288  = 605857431263981935

real	0m1.851s
user	0m1.702s
sys	0m0.081s
 */
#include <iostream>
#include <vector>

using namespace std;

unsigned long long sn(){
  static unsigned long long sn=0;
  if(sn)
    sn=(sn*sn)%50515093;
  else
    sn=290797;
  return sn;
}

int main(){
    int prim=61,q=10000000,m=10;
    vector<long long> t(q+1,0);
    vector<long long> pows(m+1,0);
    for(int i=0;i<t.size();t[i]=sn()%prim,i++);
    pows[0]=1;
    for(int i=1;i<pows.size();pows[i]=pows[i-1]*prim,i++);
    long long ans=0L;
    for(int p=1;p<=q+1;p++){
       long long val = 0;
       int  lim = p+m<=q?m:q-p+1;
       for(int k=0;k<lim;k++){
         val+=(t[p+k]*pows[k]);
         val%=pows[m];
       }
       ans+=val;
       ans%=pows[m];
    }
    cout << "Answer 288  = " << ans << endl;
}






/*
 * =====================================================================================
 *
 *       Filename:  618.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/14/2018 16:23:33
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */
 //[0] g++ -O3 -o 618.x 618.cpp ; time ./618.x
//Answer 618 = 634212216
//real	0m17.242s
//user	0m15.340s
//sys	0m1.116s

#include <iostream>
#include <vector>
using namespace std;

void get_primes(vector<int> &p,int n){
  p.resize(n);
  for(int i=0;i<p.size();p[i++]=1);
  p[0]=-1;
  for(int i=2;i<n;i++)
    if(p[i-1]==1)
      for(int k=2*i;k<n;k+=i)
        p[k-1]=-1;
  int np=0;
  for(int i=0;i<n;i++)
    if(p[i]==1)
      p[np++]=i+1;
  p.resize(np-1);
  return;
}

long long psfib(const vector<int> &p, int s, int sum, long long psf, vector<vector<long long>> &memo){
  long long mod=1000000000LL; 
  if(memo[sum][s]!=-1)
    return (psf*memo[sum][s])%mod;
  if(sum==0)
    return psf%mod;
  if(s>=p.size()||p[s]>sum||sum<0)
    return 0LL;
  long long key1 = psfib(p,s,sum-p[s],1,memo); 
  if(sum-p[s]>=0) 
    memo[sum-p[s]][s]=key1%mod;
  long long key2 = psfib(p,s+1,sum,1,memo);
  if(s+1<p.size())
    memo[sum][s+1]=key2%mod;
  return (key1*psf*p[s]+key2*psf)%mod;
}


int main(){
  vector<int> fib(24);
  fib[0]=1,fib[1]=1;
  for(int i=2;i<24;fib[i]=fib[i-1]+fib[i-2],i++);
  vector<int> primes;
  get_primes(primes,fib[23]);
  long long ans =0LL;
  vector<vector<long long>> memo(1+fib[23],vector<long long>(primes.size(),-1));
  for(int i=1;i<24;i++){
    ans+=psfib(primes,0,fib[i],1,memo);
  }
  cout << "Answer 618 = " << ans%1000000000;     
  return 0;
}




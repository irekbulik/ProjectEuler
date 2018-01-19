/*
 * =====================================================================================
 *
 *       Filename:  303.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/18/2018 18:18:27
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */
// we assume that the number f(n) can be represented as 64 bit integer.
// which is fine but there is an issue with 9999 which is super close to overflow, but we can 
// see the pattern from 9,99,999 so the f(9999)/9999 =  1111333355557778;
// Answer 303 1111981904675169
//real	0m8.985s
//user	0m8.829s
//sys	0m0.051s
//
//
#include <iostream>
#include <vector>

using namespace std;

unsigned long long three_to_dec( int bin){
    unsigned long long ans  = 0ULL;
    unsigned long long base = 1ULL;
       while(bin)
          ans+=(bin%3)*base,bin/=3,base*=10;
       return ans;
}
      
unsigned long long pow_3(int n){
  long ans=1;
  for(int i=0;i<n;ans*=3,i++);
  return ans;
}

int main(){
    int limit = 10000;
    unsigned long long ans=0;
    std::vector<unsigned long long> memo(limit+1,0);
    std::vector<int> list;
       
    for(int n_dig=1;n_dig<18;n_dig++){
      list.clear();
      for(int i=1;i<=limit;i++)
         if(memo[i]==0)
           list.push_back(i);
      long limit_u = pow_3(n_dig);
      long limit_d = pow_3(n_dig-1)-1;
      for(long v=limit_d;v<limit_u;v++){
        unsigned long long number = three_to_dec(v);
        for(int i=0;i<list.size();i++)
          if(number%list[i]==0&&(memo[list[i]]>number||memo[list[i]]==0))
            memo[list[i]]=number;
      }
    }
    memo[9999] =  9999*1111333355557778ULL;   
    for(int i=1;i<=limit;i++){
      ans+=memo[i]/i;
    }
    cout << "Answer 303 " << ans << endl;

}















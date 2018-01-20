/*
 * =====================================================================================
 *
 *       Filename:  297.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/19/2018 20:39:39
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */
// As per wiki, a greedy algorithm works. That is enough to find solution.
// [0] g++ -O3 297.cpp -o 297.x ; time ./297.x
//Answer 297 = 2252639041804718029
//
//real	0m0.005s
//user	0m0.002s
//sys	0m0.002s

#include <iostream>
#include <vector>
#include <climits>
using namespace std;

int zack_n(long long num, vector<unsigned long long> &fibs){
    int i = fibs.size()-1;
    int ans=0;
    while(num){
       while(fibs[i]>num)
         i--;
       num-=fibs[i],ans++;
         i-=2;
    }
    return ans;
}
// convoluted fibonnaci numbers, but the pattern was clear.
// https://oeis.org/A001629
unsigned long long con_f(int  n , vector<unsigned long long> &fibs){
  if(n==1)
    return 1;
  else
    return ((n+1)*(fibs[n]+fibs[n-2])-fibs[n-1])/5;
}

int main(){
    long long limit=100000000000000000;
    //long long limit=100000000;
    unsigned long long limit_f=LLONG_MAX;
    vector<unsigned long long> fibs;
    fibs.push_back(1),fibs.push_back(2);
    while(fibs[fibs.size()-1]+fibs[fibs.size()-2]<limit_f/2)
      fibs.push_back(fibs[fibs.size()-1]+fibs[fibs.size()-2]);
    unsigned long long ans = 0;

    while(limit){
       int i = fibs.size()-1;
       while(fibs[i]>limit)
         i--;
       ans+=con_f(i,fibs);
       limit-=fibs[i];
       ans+=limit;
    }
    cout << "Answer 297 = " <<  ans << endl;
}





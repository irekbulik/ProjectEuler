/*
 * =====================================================================================
 *
 *       Filename:  183.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/15/2018 21:30:10
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================

Had to use quad precision but well...
 [0] g++ -O3 -o 183.x 183.cpp ; time ./183.x
Answer 183 = 48861552

real	0m4.944s
user	0m4.886s
sys	0m0.023s
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

bool just_25(int n){
  while(n%2==0) n/=2;
  while(n%5==0) n/=5;
  return n==1;
}

int main(){
    long long ans=0;
    for(int n=5;n<=10000;n++){
      long double max=0.0;
      int k;
      for(int p=1;p<=n;p++){
        long double val = n;
        val/=p,val=pow(val,p);
        if(val>max)
          max=val,k=p;
      } 
        if(just_25(k/__gcd(k,n)))
          ans-=n;
        else
          ans+=n;
    }
    cout << "Answer 183 = " <<  ans << endl;
    return 0;
}




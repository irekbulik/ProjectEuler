/*
 * =====================================================================================
 *
 *       Filename:  595.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/20/2018 14:40:51
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */
/*
 * We can work out some cases by hand: 
 *    s(1) = 0
 *    s(2) = 1
 *    s(3) = 7/3
 *    s(5) = 4213/871
 * 
 * the problem boils to finding how many groups of subsorted n cards 
 * we can have. Let us say we have n cards
 *
 *    1 2 3 4 ... n 
 *
 * there are n-1 spaces where we put d bars to have d+1 group. But out of the (d+1)! 
 * factorial permutations, we can take only the ones where no element is in order. 
 * That is just the value of m group with m cards. We can compute that as m!-sum of known terms.
 *
 * 
 * [0] g++ -O3 595.cpp  -o 595.x ; time ./595.x
 * Answer 595     54.17529329
 *
 * real	0m0.005s
 * user	0m0.002s
 * sys	0m0.002s
 */
#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

double fact(int n){
  double ans=1.;
  for(int i=2;i<=n;ans*=i,i++);
  return ans;
}

double bin(int n,int m){
  return fact(n)/fact(m)/fact(n-m);
}

int main(){
    int n_cards=52;
    vector<double> prefactor(1+n_cards,0.);
    vector<double> s(1+n_cards);
    prefactor[1]=1.;
    for(int i=2;i<=n_cards;i++){
       double tot=0.;
       for(int j=1;j<i;j++)
         tot+=bin(i-1,j-1)*prefactor[j];
       prefactor[i]=fact(i)-tot;
    }
    s[1]=0;    
    for(int i=2;i<=n_cards;i++){
       double rhs=0.,lhs=0.;
       for(int j=2;j<i;j++)
         rhs+=prefactor[j]*bin(i-1,j-1)/fact(i)*(1.+s[j]);
       lhs=prefactor[i]/fact(i);
       s[i]=(rhs+lhs)/(1-lhs);
    }
       printf(" Answer 595 %15.8f \n",s[n_cards]);
}

/*                  
 * =====================================================================================
 *
 *       Filename:  223.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/08/2018 20:06:08
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */
// we need to find solution to equation a^2+b^2=c^2+1 with the constraint that p=a+b+c is smaller than limit and a<=b<=c
//     
//     first, let us rewrite the equation as 
//            -2pa -2pb +2ab + p^2 + 1 = 0 
//     which clearly has no solution when p is even.
//
//     then, we can write it as 
//
//            (a-p)*(b-p) = 1/2*(p-1)*(p+1) 
//      we get for free that c >= b for free. then to have b >= a we must enforce somehow.
//      Now, P is 2k+1. then the equation is 
//            (p-a)*(p-b) = 2*k*(k+1)
//
//      Now, let us say that k is odd.  Then 2*(k+1) and k are coprime 
//      Now, let us say that k is even. Then 2*k and (k+1) are coprime.
//
//      Sigma_0 is multiplicative but not totally multiplicative. We therefore know already how
//      many a,b there are. P = 2 k + 1. We must precompute  the divisor sigma_0 for limit+1 and we would be almost done
//      if not that pesky condition on lengths being nonegative..., we can bruteforce a little 
//
//Answer 223 = 61614848
//
//real	2m35.053s
//user	2m19.676s
//sys	0m8.624s
//
#include <iostream>
#include <vector>

using namespace std;

void get_simga_0(vector<vector<long long>> &a){
     int lim=a.size()-1;
     for(int i=0;i<=lim;a[i][0]=1,i++);
     for(int j=2;j<=lim/2;j++)
       for(int i=2*j;i<=lim;i+=j)
         a[i].push_back(j);
     for(int i=2;i<=lim;a[i].push_back(i),i++);
     return;
}

int main(){
  const int limit=25000000;
  vector<vector<long long>> sigma(limit+2,vector<long long>(1,0));  
  get_simga_0(sigma);
  long long ans=0;
  for(int p=1;p<=limit;p+=2){
    long long k=(p-1)/2;
    long long a,b,c,d1,d2;
    if(k&1){
      for(int i1=0;i1<sigma[k].size();i1++)
        for(int i2=0;i2<sigma[2*(k+1)].size();i2++){
            d1=sigma[k][i1]*sigma[2*(k+1)][i2];
            d2=(2*k*(k+1))/d1;
            a=p-d1,b=p-d2;
            c=p-a-b;
            ///cout << "p d1 d2 " << p << " " << d1 << " " << d2  << endl;
            //cout << "a b c " << a << " " << b << " " << c << endl;
            if(a>0&&a<=b&&b<=c)
              ans++;
        }
    }
    else{
      for(int i1=0;i1<sigma[2*k].size();i1++)
        for(int i2=0;i2<sigma[k+1].size();i2++){
            d1=sigma[2*k][i1]*sigma[k+1][i2];
            d2=(2*k*(k+1))/d1;
            a=p-d1,b=p-d2;
            c=p-a-b;
            //cout << "p d1 d2 " << p << " " << d1 << " " << d2  << endl;
            //cout << "a b c " << a << " " << b << " " << c << endl;
            if(a>0&&a<=b&&b<=c)
              ans++;
         }
    }
  }

  cout << "Answer 223 = " <<  ans << endl;

}


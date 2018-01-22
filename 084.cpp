/*
 * =====================================================================================
 *
 *       Filename:  84.cpp
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  01/21/2018 18:59:16
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  Ireneusz W. Bulik (), 
 *   Organization:  
 *
 * =====================================================================================
 */
#include <iostream>
#include <vector>
// That was the most tedious problem so far. Not interesting at all
//
//
using namespace std;

vector<double> move_sp(vector<double> p){
     vector<double> n=p;
      // go to jail
     n[10]+=p[30],n[30]=0.;
     // CC
     n[0]+=p[2 ]/16,n[10]+=p[2 ]/16,n[2 ]-=p[2 ]/8; //CC1
     n[0]+=p[17]/16,n[10]+=p[17]/16,n[17]-=p[17]/8; //CC2
     n[0]+=p[33]/16,n[10]+=p[33]/16,n[33]-=p[33]/8; //CC3
     // CH: Fixed: 
     n[0]+=p[7 ]/16,n[10]+=p[7 ]/16,n[11]+=p[7 ]/16,n[24]+=p[7 ]/16,n[39]+=p[7 ]/16,n[5 ]+=p[7 ]/16;n[ 4]+=p[7 ]/16;
     n[0]+=p[22]/16,n[10]+=p[22]/16,n[11]+=p[22]/16,n[24]+=p[22]/16,n[39]+=p[22]/16,n[5 ]+=p[22]/16;n[19]+=p[22]/16;
     n[0]+=p[36]/16,n[10]+=p[36]/16,n[11]+=p[36]/16,n[24]+=p[36]/16,n[39]+=p[36]/16,n[5 ]+=p[36]/16;n[33]+=p[36]/16;
     // CH: Next R
     n[5]+=p[36]/8,n[15]+=p[7 ]/8,n[25]+=p[22]/8; 
     // CH: Next U
     n[12]+=p[36]/16,n[12]+=p[7 ]/16,n[28]+=p[22]/16;
     // 
     n[7]-=p[7]*10/16,n[22]-=p[22]*10/16,n[36]-=p[36]*10/16;
     return n;
}


vector<double> move(vector<double> p, int s){
     vector<double> n(120,0.);
     int s2=s*s;
     for(int i1=1;i1<=s;i1++)
       for(int i2=1;i2<=s;i2++){
         if(i1!=i2){
              for(int k=0;k<40;k++){
                     n[(k+i1+i2)%40]+=1./s2*p[k   ];
                     n[(k+i1+i2)%40]+=1./s2*p[k+40];
                     n[(k+i1+i2)%40]+=1./s2*p[k+80];
              }
         }
         else
              for(int k=0;k<40;k++){
                     // record a double
                     n[(k+i1+i2)%40+40]+=1./s2*p[k   ];
                     n[(k+i1+i2)%40+80]+=1./s2*p[k+40];
                     // go to jail
                     n[10]+=1./s2*p[k+80];
              }
        }
       for(int k=0;k<3;k++){
          p.clear();
          for(int i=0;i<40;i++)
              p.push_back(n[i+k*40]);
          p=move_sp(p);
          for(int i=0;i<40;i++)
              n[i+k*40]=p[i];
       }
       return n;
}

int main(){
    vector<double> p(120,0.0);
    vector<double> np(120,0.0);
    int side=4;
    p[0]=1.;
    np=move(p,side);
    double ver=0.;
    for(int i=0;i<500;i++)
      p=np,np=move(p,side);
//    for(int i=0;i<40;i++)
//      printf("field and percent : %i %5.2f %\n",i,(p[i]+p[i+40]+p[i+80])*100);
    // sum over doubles
    for(int i=0;i<40;i++)
       p[i]=p[i]+p[i+40]+p[i+80];

    for(int k=0;k<3;k++){
      double max=0.;
      int    max_i=0;
      for(int i=0;i<40;i++)
        if(p[i]>max)
          max=p[i],max_i=i;
      printf("field and percent : %i %5.2f %\n",max_i,p[max_i]*100);
      p[max_i]*=-1;
    }
    return 0;
}












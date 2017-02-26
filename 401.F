! Sum[sigma2(k) , {k,1,n}] = Sum[k^2*Floor[n/j],{k,1,n}]
!
! Sum [k^2 * Floor[n/k] , {k,1,Floor[n/2]} +  Sum_{k^2,{k,Floor[n/2]+1,n}] 
!
! Sum[i^2,{i,a,b}] = (2*(b^3-a^3) + 3*(b^2+a^2) + (b-a))/6 
!

!suma[a_, b_] := -(1/6) (-1 + a - b) (-a + 2 a^2 + b + 2 a b + 2 b^2)
!su[lim_] := Module[{ans, i},
!  ans = 0;
!  For[i = 1, i <= Min[Floor[Sqrt[lim]], lim], i++,
!   ans = Mod[ans + i*suma[Floor[lim/(i + 1)] + 1, Floor[lim/i]], 
!     10^9]
!   ];
!  ans = ans + Sum[k^2*Floor[lim/k], {k, 1, Floor[lim/(i)]}];
!  Mod[ans, 10^9]
! ]
!
!that is not the most optimal, but yeah, I can get answer in 
#define __limit__ 10**15
!gfortran -fdefault-integer-8 401.F -o 401 ; time ./401 
!Answer 401 =  281632621
!
!real    0m4.288s
!user    0m4.263s
!sys     0m0.009s
        program Eu401
        implicit none 
        integer  tot,up,dn     
        integer  i,k         
        integer  sumsq
        
           tot = 0
        do 100 i = 1 ,  int(sqrt(real(__limit__)))
           up  = sumsq(__limit__/(i+1)+1,__limit__/i)
           up  = mod(up*i,10**9)
100        tot = mod(tot+up,10**9)
        do 200 k = 1 , __limit__/i
           up  = mod(k*k,10**9)
           dn  = mod(int(__limit__/k),10**9)
200        tot = mod(tot+up*dn,10**9)
        write(*,1800) tot   
1800    Format('Answer 401 = ',I10)

        return
        end 

        integer function sumsq(a,b)
        implicit none 
        integer  a,b
        integer  sumaa,sumab
        integer  pa,pb

        pa = mod(a,10**9)
        pb = mod(b,10**9)
        
        if(mod(b,2).eq.0) then 
          sumab = mod(b/2,10**9)
          sumab = mod(sumab*(pb+1)  ,10**9)
        else 
          sumab = mod((b+1)/2,10**9)
          sumab = mod(sumab*pb     ,10**9)
        endif 
          pb    = mod(2*pb+1,10**9)
          sumab = mod(sumab*pb,10**9)
          sumab = mod(sumab*666666667,10**9)
        
        if(mod(a-1,2).eq.0) then 
          sumaa = mod((a-1)/2,10**9)
          sumaa = mod(sumaa*pa  ,10**9)
        else 
          sumaa = mod(a/2,10**9)
          sumaa = mod(sumaa*(pa-1)      ,10**9)
        endif 
          pa    = mod(2*pa-1,10**9)
          sumaa = mod(sumaa*pa,10**9)
          sumaa = mod(sumaa*666666667,10**9)
        sumsq = sumab - sumaa
20      if(sumsq.lt.0) then
                sumsq = sumsq+10**9
                goto 20 
        endif 
        return
        end 

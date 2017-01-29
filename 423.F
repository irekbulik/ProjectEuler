! GIVEN NUMBER OF THROWS N AND P FOR NUMBER OF PARIS THE 
! NUMBER OF CONFIGURATIONS IS 
!
!  C(N,P) = 6 * BINOM[N-1,P] * 5 ** ( N - P - 1) 
!
!  C(N) = SUM(C(N,K),{K,1,Pi(N)})
!
! LET US DENOTE S(N,K) = SUM(C(N,P),P=0,K)
!
! THEN S(N,K) = 6 * S(N-1,K) + 6 * BINOM(N-2,K)*5**(N-K-2)
!
! USING BINOM(N,K) = BINOM(N-1,K) + BINOM(N-1,K-1)
!
!Answer 423 =        653972374
!
!real    1m23.159s
!user    1m22.869s
!sys     0m0.065s
!
! OK, MAYBE ONE CAN IMPROVE IT, I DO NOT CARE NOW.
!
!
#define __MOD__ 1000000007
#define __SIZ__ 50000000
!
        program Eu423 
        implicit none
        integer  binom 
        integer  pi(__SIZ__)
        integer  total 
        integer  modexp
        integer  i,j
        integer  p
        integer  suma,oldpi,sub,bin1,bin2

        call getpi(pi,__SIZ__)
        p = __MOD__
         
        suma  = 6 
        oldpi = 0 
        total = suma 
        bin1  = 1 
        bin2  = 1  
!   
        do i = 2 , __SIZ__
! JUST TAKE CARE OF THE INITIAL PART 
          if(oldpi.eq.i-2) then 
          bin1 = 1 
          else 
          bin1 = mod(mod(bin1*(i-2),p)*modexp(i-oldpi-2,p-2,p),p)
          endif 
        sub  = mod(bin1*modexp(5,i-2-oldpi,p),p)
        suma = mod(6*(suma-sub),p)
        if(pi(i).ne.oldpi) then
          if(i-1.eq.oldpi+1) then 
            bin2 = 1 
          else 
!UPDATE lower 
            bin1 = mod(mod(bin1*(i-2-oldpi),p)*modexp(oldpi+1,p-2,p),p)
!UPDATE upper 
            bin2 = mod(mod(bin1*(i-1),p)*modexp(i-oldpi-2,p-2,p),p)
          endif 
        oldpi = pi(i) 
        suma = suma + 
     $   6*mod(bin2*modexp(5,i-1-oldpi,p),p)
        suma = mod(suma,p)
        endif 
        total = mod(total+suma,p)
        enddo 
        if(total.lt.0) then 
          total = total + (-total/p+1)*p
        endif 
        write(*,1800) total 
1800    format("Answer 423 = ",I16)
        return
        end 



! COMPUTES BINOMIAL GIVEN PRIME LARGER THAN N,M
        integer function binom(n,m,p)
        implicit none
        integer  m,n,p
! p is assumed prime 
        integer  i,den
        integer  modexp
         
        if(n.eq.m.or.m.eq.0.or..true.) then
          binom = 1 
          return
        endif

        binom = n 
        do 10 i = n-1, n-m+1, -1
10         binom = mod(binom*i,p)
        den = 1 
        do 20 i = 2 , m 
20         den = mod(den*i,p)
! Fermat little theorem 
        binom = mod(binom*modexp(den,p-2,p),p)
!
        return
        end         


        integer function modexp(b,p,m)
! modexp 
        integer b,p,m
        integer a,n,r

        r = 1 
        a = b 
        n = p 

100     continue 

        if(mod(n,2).eq.1) r = mod(r*a,m)
        if(n/2.gt.0) then 
           a = mod(a * a,m)
           n = n / 2 
        else
           goto 200 
        endif 
        goto 100 
200     continue 

        modexp = mod(r,m) 

        return
        end 

        subroutine getpi(res,maxval)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  val, loop

        do 1 loop = 2 , maxval 
1         res(loop) = 1  
        
        res(1) = 0        

        val = 2

50         loop = val        
20         loop = loop + val 
           if(loop.gt.maxval) goto 10 
           res(loop) = 0 
           goto 20
10      continue 
! FIND NEXT PRIME
30      val = val + 1 
        if(val.gt.maxval)    goto 40 
        if(res(val).eq.0)   goto 30
        goto 50
40      continue 
                

        val = 0      
        do 60 loop = 1 , maxval 
           val = val + res(loop) 
60         res(loop) = val

        return
        end       

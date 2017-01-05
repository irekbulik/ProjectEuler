        program eu160
        implicit none
        external tdoff 
        integer  i
!Answer 160 =  16576
!
!real    0m0.006s
!user    0m0.002s
!sys     0m0.002s

!
! So, the solution is as follows. n! will have k factors of 5 and l
! factors of 2 with l > k. there are k trailing 0. we require d digits 
! after removing trailing 0. we start with assumtions that n/5 > d.
! otherwise we implement the direct approach. 
!   
!    if d < n/5  then 
!
!    n! = [n/5]! * 5^[n/5] * product of all numbers not divisible by 5 
!    
!
!    we now know that the last d digits of n! is the last d digits of
!    [n/5]! (remove 0) and * product off all numbers not divisible by 5 
!    divided by 2^[n/5] (there is at least 2^[n/5] factors of 2 so > d)  
!
!    we can compute the last digits by crt. calling c all numbers not
!    divisible by 5 smaller than n as c we get 
!
!       c / 2^[n/5] mod 5^d from modular inverse as 2 and 5 are coprime. 
! 
!    finally, we solve crt 
! 
!    0 = x mod 2^d 
!    a = x mod 5^d 
!    to get 
!    x mod 10^d    
!  
        
        call tdoff(10**12,5,i,tdoff)
        write(*,1805) i 

1805    format('Answer 160 = ',I6)


        return
        end 

        subroutine tdoff(n,d,ans,recur)
        implicit none 
        integer  n,d,ans
        external recur 
        integer  i 
        integer  c 
!
        integer  smfac, mulinv, modexp, prodm5
        integer  crtsol 
!
! CHEKC IF THE DIRECT SOLUTION IS REQUIRED 
!
!
        if(n/5.lt.d) then 
           ans = 1 
           do 10  i = 2 , n 
           ans = ans * i 
20         continue            
           if(mod(ans,5).eq.0) then 
              ans  = ans / 10 
              goto 20 
           endif 
              ans = mod(ans,10**d)
10         continue
!
        else 
!
! determine the last d digits of [n/5]!
         call recur(n/5,d,smfac,recur)        
! determine the last d digits of product of all numbers smaller than n/5
! divided by 2^[n/5]. we start by solving this product modulo 5**d.
! let us call it c. we look for value C such that c = C * 2 ^ [n/5] 
!  
!   c mod(5**d) =  C mod 5**d . 2 ^ [n/5] mod 5**d 
!
        c = mod(prodm5(n,d) * mulinv(modexp(2,n/5,5**d),5**d),5**d)
!
! solve the chinese reminder theroem 
!
        ans = crtsol(0,c,2**d,5**d)
        
! add the smaller factorial 
        ans = mod(ans*smfac,10**d) 
!
        endif 

        return 
        end 

        integer function prodm5(n,d)
        implicit none 
        integer  n, d 
        integer  nblock, blval, ival
        integer  modexp
!
! we can loop over the blocks of 5**d 
!        
        nblock = n/5**d
        prodm5 = 1 
        blval  = 1 

           if(nblock.ge.1) then 
           do 10 ival = 1 , 5**d - 1 
           if(mod(ival,5).eq.0) goto 10 
           blval = mod(blval*ival,5**d) 
10         continue    
           prodm5 = modexp(blval,nblock,5**d)                  
           endif 

           do 20 ival = nblock * 5**d + 1 , n 
           if(mod(ival,5).eq.0) goto 20 
           prodm5 = mod(prodm5*ival,5**d) 
20         continue 

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

        modexp = r 

        return
        end 

         integer function mulinv(n,m) 
         implicit none 
         integer  n,m
         integer  gcd,x,y
! find multiplicative inverse of n module m          
         call gcdabi(n,m,x,y,gcd)
         mulinv = mod(x,m) 
          if(mulinv.lt.0) then
             x = abs(mulinv)/m + 1 
             mulinv = mulinv + x * m 
          endif 
         return
         end 
        
        

         integer function crtsol(a,b,n,m)  
! chinese reminder theorem solver for equations of the type 
! 
!  given 
! 
!  a = x mod n 
!  b = x mod m   
!  gcd(m,n) = 1 
!
!  returns 
! 
!  crtsol = x mod n * m 
!
         implicit none 
         integer a,b,n,m
         integer x,y,gcd,c
!
! using solution of n . x + m . y = gcd(n,m) 
! if gcd(n,m) = 1 then mod(x,m) is modular inverse of n mod m. 
! with that we have 
!     
!      x = n * t + a 
!      n * t + a = b mod m  
!      t = n^-1(b-a) mod m 
!      t = c         mod m   
!      x = n * ( m * s + c ) + a 
!      x = n * c + a mod m * n 
!
       call gcdabi(n,m,x,y,gcd)
       if(gcd.ne.1) then 
          write(*,*) 'not coprime n,m in crtsol'
          stop
       endif     

       c = mod(x * ( b - a )   , m) 
       crtsol = mod( n * c + a , m*n)
       if(crtsol.lt.0) then 
          crtsol = crtsol + c * m * n 
       endif 

       return
       end  
!
! extended euclid algorithm that solves the problem
!
!  a . x + b . y = gcd(a,b) 
!         
        subroutine gcdabi(a,b,x,y,gcd)
        implicit none
        integer  a,b,x,y,gcd
        integer  r0,r1,r2,s0,s1,s2,t0,t1,t2
        integer  q 

        r0 = a 
        r1 = b
        if(a.lt.b) then 
         r0 = b 
         r1 = a 
        endif 
        s0 = 1 
        s1 = 0
        t0 = 0
        t1 = 1 

100     continue 
!         
         q  = r0/r1
!
         r2 = r0 - q * r1 
         s2 = s0 - q * s1
         t2 = t0 - q * t1 
         
         if(r2.eq.0) goto 200
         r0 = r1
         s0 = s1
         t0 = t1 
        
         r1 = r2
         s1 = s2
         t1 = t2 

         goto 100 

200      continue 

        gcd = r1 
        x   = s1
        y   = t1 
        
          if(a.lt.b) then
          x = t1
          y = s1 
          endif 

        return
        end 

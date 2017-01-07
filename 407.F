#define MAX_FACTORS 9
#define MAX_VAL     10**7
!Answer 407 =   39782849136421
!
!real    0m56.285s
!user    0m56.128s
!sys     0m0.017s
! factorization should be improved but well, less than minute
        program eu407
! THERE CANNOT BE MORE THAN 9 FACTORS 2*3*5*7*..19 > 10^7 
        implicit none
        integer  facts(MAX_FACTORS)
        integer  numbs(MAX_VAL)
        integer  i,k,np
        integer  total
!
        call sieve(numbs,MAX_VAL)
        total = 0 
!
         do i = 2 , MAX_VAL 
          call factor(i,facts,np,numbs) 
          call lar_id(i,facts,np,k)
          total = total + k 
         enddo 

          write(*,1805) , total 

1805     format('Answer 407 = ',I16)
100      format(i3,2x,9(i2,1x))
         return
         end 
 

         subroutine lar_id(n,facts,np,large) 
         implicit none 
         integer  n 
         integer  facts(*), np, large 
         integer  rhs(MAX_FACTORS)
         integer  k,l,kp
         integer  syscrt

          if(np.eq.1) then 
             large = 1 
          else 
            large = 0 
            do 100 k = 0 , 2**np - 1 
               kp = k 
               do 200 l = np , 1 , -1
               rhs(l) = kp/2**(l-1)
200            kp = kp - rhs(l)*2**(l-1)
               kp = syscrt(np,rhs,facts)
!               print *, 'n:kp',n,kp 
               if(kp.lt.n) large = max(large,kp)
100        continue 
          endif 
          return
          end          


         subroutine factor(n,p,np,primes)
         implicit none 
         integer  n, p(MAX_FACTORS),np, primes(*) 
         integer  i,m,ip

!
! CHECK IF NUMBER IS PRIME 
!
         np = 0
!
         if(primes(n).eq.1) then 
           np = np + 1 
           p(np) = n 
         else 
          m  = n 
          ip = 1 
! FIND PRIME 
10         ip = ip + 1 
           if(primes(ip).ne.1) goto 10    
!
           if(ip**2.gt.m) goto 20 
!
           if(mod(m,ip).eq.0) then 
              np = np + 1 
              p(np) = 1 
30            m = m/ip
              p(np) = p(np)*ip 
              if(mod(m,ip).eq.0) goto 30 
           endif 
          goto 10 
20        continue 

          if(m.gt.1) then 
            np = np + 1 
            p(np) = m 
          endif

          endif 

          return
          end       


         integer function syscrt(neq,rhs,mods)
         implicit none
         integer  neq
         integer  rhs(*)
         integer  mods(*) 
         integer  i, a, b, c, n, m 
         integer  crtsol
         
         if(neq.lt.2) stop
        
         a = rhs (1) 
         n = mods(1)    
         b = rhs (2) 
         m = mods(2) 

           c = crtsol(a,b,n,m) 
         
         do 100 i = 3 , neq 

           a = c 
           n = n * m 
           b = rhs(i)
           m = mods(i)
           c = crtsol(a,b,n,m)

100      continue 

         c = mod(c,n*m) 
         syscrt = c 
         if(c.lt.0) then 
           syscrt = syscrt + n*m*(-c/(n*m)+1)
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
          write(*,*) 'not coprime n,m in crtsol',n,m
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
!
        subroutine sieve(res,maxval)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  val, loop
!
        res(1) = -1 
        do 100 val = 2 , maxval 
100        res(val) = 1 
!
        val = 2

50         loop = val        
20         loop = loop + val 
           if(loop.gt.maxval) goto 10 
           res(loop) = -1 
           goto 20
10      continue 
! FIND NEXT PRIME
30      val = val + 1 
        if(val.gt.maxval)    goto 40 
        if(res(val).eq.-1)   goto 30
        goto 50
40      continue 


        return
        end       

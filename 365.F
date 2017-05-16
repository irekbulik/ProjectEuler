!
! WELL, THAT IS EASY APPLICATION OF LUCAS THEOREM AND CRT. 
! WE ARE GOOD BECAUSE THE NUMBERS ARE ALL NICE AND COPRIME 
!
!
        program Eu365
        implicit none
        integer  primes(1000)
        integer  numbas(5000)
        integer  nprim
        integer  total,p,q,r
        integer  crtsol,crt1
        integer  maxl,minl,m,n

        maxl = 5000
        minl = 1000
        m    = 10**18
        n    = 10**9
        
        call sieve(numbas,maxl,primes,nprim,minl)
        call getsm(m,n,primes,nprim,numbas) 
        total = 0 
        do 10 p = 1    , nprim 
        do 20 q = p + 1, nprim
           crt1 = crtsol(numbas(p),numbas(q),primes(p),primes(q)) 
        do 30 r = q + 1, nprim 
           crt1 = crtsol(crt1,numbas(r),primes(p)*primes(q),primes(r))
           total  = total + crt1 
        
30      continue         
20      continue         
10      continue         

        write(*,1800) total

1800    format('Answer = ',I18)


        return
        end 



        subroutine getsm(m,n,p,np,ans)
        implicit none
        integer  m,n,p(*),np,ans(*)
        integer  binom 
        integer  maxn 
        integer  ip,iv,ln,lm,ie,in,im,imod

!        
        if(n.gt.m) then
          print *, 'n.gt.m'
          stop
        endif 
!
        do ip = 1 , np 

        lm = m 
        ln = n 

        maxn = int(log(real(lm))/log(real(p(ip))))
         
           imod = 1 
           do ie = maxn , 0, -1 
!
           im = lm/p(ip)**ie
           in = ln/p(ip)**ie
!
           lm = lm - im * p(ip)**ie
           ln = ln - in * p(ip)**ie
         
           imod = mod( imod * binom(im,in,p(ip)) ,p(ip))
!
           enddo 
!             
        ans(ip) = imod
!
        enddo

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
          c = -crtsol/(m*n)+1
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
        subroutine sieve(res,maxval,prime,np,nmin)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  prime(*)
        integer  np,nmin
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
        np = 0 
        do 200 val = nmin + 1 , maxval 
           if(res(val).eq.1) then 
             np = np + 1 
             prime(np) = val
           endif 
200     continue
        return
        end       

! COMPUTES BINOMIAL GIVEN PRIME LARGER THAN N,M
        integer function binom(n,m,p)
        implicit none
        integer  m,n,p
! p is assumed prime 
        integer  i,den
        integer  modexp
        
        if(m.gt.n) then
          binom = 0
          return
        endif

        if(n.eq.m.or.m.eq.0) then
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
        if(binom.lt.0) then
          den = -binom/p + 1 
          den = den + den * p 
        endif 
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
        if(modexp.lt.0) then
           r =  -modexp/m  +1 
           modexp = modexp + r*m
        endif 

        return
        end 

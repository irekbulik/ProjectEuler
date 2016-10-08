#define __MAX_FACT__   1000
#define __MAX_PRIMES__ 10000000
#define __LIMIT__  10**8
!>@author IWB
!>@brief  euler 549
!
!
!
!Euler 549 =   476001479068717
!
!real    3m25.245s
!user    3m24.242s
!sys     0m0.129s
        program eu549
        integer  legdiv
        integer  fac(2,__MAX_FACT__)
        integer  nf 
        integer  mini
        integer  tot
        integer  work(__LIMIT__)
        integer  primes(__MAX_PRIMES__),nprime
        common   /prim/primes,nprime


        call prmsiv(__LIMIT__,work,primes,nprime)
          
        tot = 0 

        do 100 i = __LIMIT__ , 2 , -1 
           if(i.eq.primes(nprime)) then
              nf     = 1 
            fac(1,1) = i 
            fac(2,1) = 1 
              nprime = nprime - 1 
           else  
           call facint(fac,nf,i)
           endif 
           mini = legdiv(fac(1,1),fac(2,1))
           do 200 j = 2 , nf 
200        mini = max(mini,legdiv(fac(1,j),fac(2,j)))

        tot = tot + mini

100     continue         
 
        write(*,1800) tot 

1800    format('Euler 549 = ',I17)
        
        return
        end



        subroutine facint(fac,nf,int2f)
        implicit none 
        integer  fac(2,__MAX_FACT__)
        integer  nf 
        integer  int2f
! PRIMES 
        integer  primes(__MAX_PRIMES__),nprime
        common   /prim/primes,nprime
! LOCAL
        integer  n,i,div
        integer  nfact 
        integer  iprim 


        nfact = __MAX_FACT__
! COPY INPUT 
        n     = int2f        
        nf    = 0
!
        if(nfact.lt.1) then
             print *, 'number of factors exeeded'
             stop
        endif 

        iprim = 0
100     continue 
        iprim = iprim + 1                  
        div = primes(iprim)
        if(div.gt.int(sqrt(real(n)))) then
! IT IS PRIME 
           nf = nf + 1 
           fac(1,nf) = n
           fac(2,nf) = 1 
           return 
        endif 
         if(mod(n,div).eq.0) then 
            nf        = nf + 1 
            fac(1,nf) = div 
            fac(2,nf) = 1 
200         n=n/div
            if(mod(n,div).eq.0) then
            fac(2,nf) = fac(2,nf) + 1 
            goto 200
            endif 
         endif       
         if(n.gt.1) goto 100 
      

        return
        end 

        integer function legdiv(p,k0)
! GIVEN P AND K, FIND THE SMALLEST N THAT SATISFIES 
!
!       SUM[FLOOR[N/P^I],{I,1,Inf}] >= K 
!
        implicit none
        integer  p,k0
        integer  ipow,limit
        integer  k
!
        k      = k0 
        legdiv = 0 
!
100     continue        
!
        limit = 1
        ipow  = 0
!        
150     continue          
        ipow  = ipow  + 1   
! THE EQUALITY IS NEEDED IF K.EQ.P. THEN WE KNOW THAT P^2 WILL BE THE
! SMALLEST N AND WE OVERSHOOT K. SIMILARLY, WE WILL ALWAYS OVERSHOOT IF
! K.EQ.P, P*(P+1) , P*(P^2+P+1) .. 
        if(k.le.p*limit) goto 200 
        limit = limit + p**ipow
        goto 150 
! FOUND MAX POWER. REMOVER IT FROM K AND UPDATE ANSWER. LOOK FOR NEW
! ONE.
200     continue 
        legdiv = legdiv + (k/limit)*p**(ipow)
        k      = k      - (k/limit)*limit
        if(k.le.0) goto 300 
        goto 100        
300     continue
        return
        end 

        subroutine prmsiv(n,work,primes,nprime)
        implicit none
        integer  n, nprime
        integer  work(*), primes(*)
        integer  loop, val 


        work(1)   = 0 
        do 10 val = 2 , n 
10      work(val) = 1 

        loop = 2
        val  = 2 
        
20      loop = loop + val 
        if(loop.gt.n) goto 30 
        work(loop) = 0 
        goto 20 
30      val = val + 1
        if(val.gt.n)  goto 40
        if(work(val).eq.0) goto 30
        loop = val 
        goto 20 

40      nprime = 0 
        do 100 val = 1 , n         
          if(work(val).eq.1) then
          nprime = nprime + 1 
          if(nprime.gt.__MAX_PRIMES__) then
            print *, 'prime overflow'
            stop
          endif 
          primes(nprime) = val
          endif
100     continue  


        return
        end 

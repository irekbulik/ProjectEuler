#define SLIMIT 1000100
#define  LIMIT 1000000
#define MAXCPU 2
!
! THE CODE DOES NOT DISTRIBUTE VERY WELL 
!
!answer 134:    18613426663617118
!
!real    0m0.187s
!user    0m0.112s
!sys     0m0.107s
!
! A BIT MUCH TO PARALLELIZE 
!
        program e134
!                            A BIT MUCH THOUGH 
        integer vals(SLIMIT), primes(SLIMIT)
        integer nprim,i
        integer eu134
        integer p2 
        integer prim1, prim2 
        integer res(MAXCPU)
        integer icpu    
        integer getcpu,ncpu,setcpu,val
        

        call prmsiv(SLIMIT,vals,primes,nprim)
! START FROM PRIME THAT IS 5, SO PRIME NUMBER 3 FOR P1 AND 7 FOR P2
        ncpu = setcpu()
!
        do 10 p2 = 1 , ncpu
10       res(ncpu) = 0
        
!$omp   parallel do if(ncpu.gt.1) default(shared) schedule(static,1) 
!$omp+  private(icpu,prim1,prim2,val)
        do 100 p2 = 4, nprim
        icpu  = getcpu()
        prim1 = primes(p2-1)
        prim2 = primes(p2  ) 

        if(prim1.lt.LIMIT) then
          val = eu134(prim1,prim2)
          res(icpu) = mod(res(icpu) + val,10**18)
        endif 
100     continue 

        do 200 p2 = 2 , ncpu
200      res(1) = res(1) + res(p2)

        write(*,1800) res(1)        

1800    format('answer 134: ',I20)

        return
        end 

        integer function setcpu()
        implicit none 
#if defined (_OPENMP)
        integer  omp_get_max_threads
        setcpu = min(omp_get_max_threads(),MAXCPU)
        call omp_set_num_threads(setcpu)
#else 
        setcpu = 1 
#endif 
        return
        end 


        integer function getcpu()
        implicit none
#if defined (_OPENMP)
        integer  omp_get_thread_num 
        getcpu = omp_get_thread_num() + 1 
#else 
        getcpu = 1 
#endif 
        return
        end 



        integer function eu134(p1,p2)
        implicit none 
        integer  p1, p2
        integer  d,r,s,n
        integer  npow
        integer  ndig
!
! THE PROBLEM ASKS TO SUM NUMBERS S THAT ARE DEFINED FOR ALL CONSECUTIVE
! PRIMES STARTING FROM 5 THAT SATISFY (P1<P2) 
!
!       MOD[S,P2]                       = 0
!       MOD[S,10**(FLOOR[LOG10(P1])+1)] = P1 
!
! LET US THEN NOTE THAT P2 IS AT LEAST 7 SO IT IS ALWAYS COPRIME TO  10
!
! IF WE DO THE CHINEESE REMINDER THEOREM FOR THAT CASE, WE LOOK FOR
! SMALLEST SOLUTION OF THE FORM WHERE N = FLOOR[LOG10(P1])+1 
!
!      P1 * R * P2 MOD (P2*10**N)
! 
! AND  R SATISFIES EQUATION R * P2 + S * 10**N = 1 
!                                     
! COMPUTE THE NUMBER OF DIGITS OF P1          
!
        ndig = floor(Log10(p1*1.0d0))+1 
        npow = 10**ndig
        call gcdabi(p2,npow,r,s,d)
        eu134 = mod(mod(p1*p2,p2*npow)*r,p2*npow)
! MAKE SURE THAT IS IT POSITVIE 
        if(eu134.lt.0) then
         eu134 = (eu134/(p2*npow)+1)*p2*npow+eu134
        endif 
        
        

        return
        end         
!
! IMPLEMENTS EXTENDED EUCLIDEAN ALGORITHM 
!
!  SOLVES PROBLEM A.X + B.Y = GCD(A,B) 
!  GIVEN  A AND B RETURNS X AND Y AND GCD(A,B)
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
          primes(nprime) = val
          endif
100     continue  


        return
        end 
        
         
        

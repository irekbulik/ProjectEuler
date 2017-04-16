!      153651073760958
! U must take 2 away as I summed 1 and 2. 
!      153651073760956
!
!
!real    121m4.330s
!user    120m42.760s
!sys     0m2.771s
#define __MAX__ 2*10**7
#define __MAXFACTOR__  50 
! MAX FACTORS: Log(2) LIM^2
        program Eu451 
        implicit none
        integer  numba(__MAX__)
        integer  primes(__MAX__)
        integer  factors(__MAXFACTOR__) 
        integer  list(__MAXFACTOR__)
        integer  nprim,nfac
        integer  val,ival 
        integer  binom
        integer  i,ifa,ich,k
        integer  maxfac
        
!
! GENERAL IDEA OF SOLUTION. FOR SURE, THE SMALLEST SOLUTION 
! IS 1 FOR ALL N. WE SET THE ARRAY TO THAT. THEN, FROM 2 TO LIMIT-2, 
! WE GENERATE PRIME FACTORS OF (A+1)*(A-1). WE THEN GENERATE ALL 
! NUMBERS THAT CAN DIVIDE IT, BY GENERATING DIVISORS. WE DO IT BY CHOOSE
! FUNCTION. WE TAKE ALL SINGLES, DOUBLES, TRIP ... BUT WE CAN STOP AT
! FIRST DOUBLE TO OVERSHOOTS, THEN WE GO TRIPLE ETC. 
!
        do 10 i = 1 , __MAX__
10         numba(i) = 1 
        
        call sieve(primes,__MAX__,nprim)
        maxfac = 0 

        do 100 i = 1 , __MAX__ 
           call getpri(primes,nprim,i,factors,nfac)
           maxfac  = max(maxfac,nfac)
           do 200 ifa = 1, nfac
              call choose(1,nfac,ifa,list)
! QUIT IF TOO LARGE 
              if(val(ifa,list,factors).gt.__MAX__) goto 100
! OTHERWISE CONTINUE 
              do 210 ich = 1 , binom(nfac,ifa) 
              call choose(ich,nfac,ifa,list)
              ival = val(ifa,list,factors) 
!              if(ival.gt.__MAX__) goto 200
! CHECK IF WE UPDATE THE LIST 
              if(ival.le.__MAX__) then 
                if(numba(ival).lt.i.and.i.lt.ival-1) 
     $             numba(ival) = i   
              endif 
210           continue                 
200        continue         
100     continue         

        maxfac = 0 
        do i = 1 , __MAX__
          maxfac = maxfac + numba(i)
        enddo
        print *, maxfac
! Note the answer is 2 too big as I summed from 1
        return
        end 


        integer function val(n,a,b)
        implicit none
        integer  n,a(*),b(*)
        integer  i
         val = 1 
         do 10 i = 1 , n 
10         val = val * b(a(i))
         return
         end 

        subroutine getpri(primes,nprim,a,factor,nfac)
        implicit none
        integer  primes(*),nprim,a,factor(*),nfac
        integer  nf1,nf2
        integer  ap,am 
        integer  i
        ap = a + 1 
        am = a - 1 
        nf1 = 0 
        nf2 = 0
        call getfac(primes,nprim,ap,factor,nf1)
        call getfac(primes,nprim,am,factor(1+nf1),nf2)
        nfac = nf1 + nf2
        call sort(factor,nfac)
        return  
        end 

        subroutine getfac(p,np,n,f,nf)
        implicit none
        integer  p(*),np,n,f(*),nf
        integer  i 

        do 20 i = 1 , np 
        if(p(i)**2.gt.n) goto 30
10      continue            
        if(mod(n,p(i)).eq.0) then 
           nf = nf + 1 
           f(nf) = p(i)
           n = n / p(i)
           goto 10
        endif 
20      continue 
30      continue 
        if(n.ne.1) then 
           nf = nf + 1 
           f(nf) = n 
        endif   
        return
        end 



        subroutine sieve(res,maxval,nprime)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  val,loop,nprime
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

        nprime = 0 
        do loop = 2 , maxval
         if(res(loop).eq.1) then
            nprime = nprime + 1 
            res(nprime) = loop
         endif
        enddo


        return
        end       

       
        Integer Function BINOM(M,N)
!          ( M )
! COMPUTES (   ) returns 0 if n > m 
!          ( N ) 
!
      Implicit None
      Integer  M, N
      Integer  P,I,N1,R
      integer  memo(0:40,0:40)
      logical  ini
      save     ini,memo
      data     ini/.true./
      
      IF(INI) THEN              
        DO 1 , P = 0 , 40
        DO 1 , I = 0 , 40
1         MEMO(I,P) = 0  
        INI = .FALSE.
      ENDIF 
!      If(M.lt.0.or.N.lt.0.or.n.gt.m) THEN
!        STOP("Incorrect Input For Binom")
!      EndIF
!
      if(MEMO(M,N).ne.0) then
        binom = MEMO(M,N)
      else
      N1 = N
      P  = M - N1
      If(N1.ge.P) goto 10
      P  = N1
      N1 = M - P
10    R = N1  + 1
      If(P.eq.0) R = 1
      If(P.LT.2) goto 30
      Do 20 I = 2 , P
      R = (R*(N1+I))/I
20    Continue
30    Binom = R
      MEMO(M,N) = BINOM
      endif
      Return
      End
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
        subroutine choose(i,n,p,r)
        implicit none
        integer  i,n,p,r(*)
        integer  ii,ij,is,iv,ip
        integer  binom 
! FILL POSITIONS               
           ii = i 
           is = 1 
        do 10 ip = 1 , p 
           do 20 iv = is , n
             ii = ii - binom(n-iv,p-ip) 
             if(ii.le.0) then 
                r(ip)=iv
                is   =iv+1
                ii   =ii + binom(n-iv,p-ip)
                goto 10 
             endif 
20         continue
10       continue        
        return
        end                


        subroutine sort(a,n)
        implicit none
        integer  a(*),n
        integer  ne,nb,bc,w,b

! HIRING PART. WE START WITH N/2+1 HIRED PEOPLE AND WE EMPLOY POTENTIAL 
! N/2 BOSSED
        if(n.le.1) return
        ne = n
        nb = n/2+1
200     continue         
        if(nb.gt.1) then 
! HIRE GUY AND THEN ASSES HIM            
           nb = nb - 1 
           bc = a(nb)
        else 
! RETIRE THE TOP GUY, TAKE THE LAST ONE AND GIVE HIM THE PLACE. THEN
! VERIFY  
           bc    = a(ne)
           a(ne) = a(1) 
           ne    = ne - 1 
             if(ne.eq.1) then 
                a(1) = bc 
                return
             endif 
!
         endif 
! SET AS THE BOSS THE ITH ELEMENTS 
        b = nb 
        w = 2 * b 
! IN THE HIRE LOOP, FOR BOSS B WE MAKE SURE THAT NO WORKER W
! SUPERVISED BY B HAS BETTER QUALIFICATIONS. THE WORKERS ARE 
! 2*B AND 2*B + 1 . IS THE SWITCH IS DONE, THEN WE NEED TO MAKE SURE 
! NO MORE LEVELS OF DEMOTION ARE NEEDED. BOOS QUALIFICATIONS ARE BC.
! THERE ARE NE PEOPLE.
100      continue 
         if(w+1.le.ne) then 
           if(a(w+1).gt.a(w)) w = w + 1 
         endif 
         if(bc.lt.a(w)) then 
            a(b) = a(w)         
! CHECK IF YOU NEED TO DEMOTE B FURTHER                    
            b    = w
            w    = 2 * b 
         else 
! YOU DON'T, SKIP TO THE LAST EMPLOYEE 
            w = ne + 1 
         endif 
         if(w.le.ne) goto 100 
! NO MORE DEMOTION IS NEEDED. PUT OLD BOSS QUALIFICATIONS INTO HIS NEW
! POSITION 
         a(b) = bc 

         goto 200 

         return
         end 

!PROBLEM 581: 
!  
! QUICK SEARCH OF ERDOS PAPERS GIVES US THE HINT: STORMER THEOREM. 
! WIKI NOW SAYS THAT THE SET OF TWO ADJACENT integerS THAT ARE P-SMOOTH 
! IS FINITE. THEY ALSO SAY WE CAN FIND ALL THE PAIRS BY SOLVING PELL
! EQUATIONS. WE USE OLD LAME ONE.
!
! NOW, HERE IS WHERE THE MAGIC HAPPENS. WE ARE SURE THAT THE BIGGEST
! SOLUTION HAS TO BE BELOW ~10**15
!  
!    https://oeis.org/A002071 
!
! WHY? NO IDEA. SO (X-1)/2 < 10**15 SO X <  ~ 2**15 
! 
![0] gfortran -O5 -fdefault-integer-8 -fdefault-real-8 581.F -o 581.x ; time ./581.x 
!euler 581 =   2227616372734
!
!real    0m1.212s
!user    0m1.202s
!sys     0m0.004s
! 
!
!
!
#define MAXCFE    1000000
#define MAXPRIMES 15 
#define MAXPRIMUS 15

        program Eu581 
        implicit none 
        integer primes(MAXPRIMES) 
        integer prods(2**MAXPRIMES)
        integer cfe(MAXCFE+1)
! CHECK IF WE GOT SQUARE NUMBER OR WE DID NOT FIND PERIOD 
        logical gotsq, gotpe 
        integer x0,y0,len,d
        integer xn,yn,xm,ym,n 
        integer total 
        integer pmax, prod, iprod  
        integer useprm,maxprm,lencfe
! FUNCTIONS 
        logical reduce 
!
        data    primes/2,3,5,7,11,13,17,19,23,29,31,37,41,43,47/
!
        total  = 0 
        useprm = MAXPRIMUS
        maxprm = MAXPRIMES
        lencfe = MAXCFE
        pmax = primes(useprm)
        call frmprd(useprm,primes,prods)

       do 200 iprod = 1, 2**useprm
         
        prod = prods(iprod) 
          if(prod.eq.2) goto 200 
!
        d =  2 * prod 

        call cfesq(d,lencfe,cfe,len,gotsq,gotpe)
          if(gotsq) then 
            print *, 'found square number'      
            stop                
          endif 
        
          if(.not.gotpe) then  
            print *, 'failed to find period',d
            stop                
          endif 
! GENERATE PRIMITIVE SOLUTIONS. 
! WELL, THEY SAY WE JUST NEED TO LOOK FOR SOLUTIONS THAT ARE NOT BIGGER
! THAN 10**15 (https://oeis.org/search?q=A117581+-id:A117581)
          call solvep(cfe,len,x0,y0,2*10**15,gotpe)
          if(.not.gotpe) goto 200
! NOW, GENERATE FAMILY OF SOLUTIONS 
          xn = x0
          yn = y0 
          do 100 n = 1 , max(3,(pmax+1)/2)
! CHECK IF THE PAIR IS CORRECT 
          if(mod(xn,2).eq.1) then 
            if(reduce((xn-1)/2,primes).and.reduce((xn+1)/2,primes)) then
            total = total + (xn-1)/2      
            endif 
          endif        
          xm = xn 
          ym = yn 
          xn = x0 * xm + d * y0 * ym 
          yn = y0 * xm +     x0 * ym 
100       continue 
!
200       continue 
!
          write(*,1800) total 
!
1800      format('euler 581 =',I16)
!
          return
          end 
!

          logical function reduce(a,p)
          integer a,p(MAXPRIMUS)
          integer i,b

          b = a 
          i = 1 
100       continue         
          if(i.gt.MAXPRIMUS) then 
             reduce = .false. 
             return 
          endif 
200       continue 
          if(mod(b,p(i)).eq.0) then 
             b = b/p(i) 
             goto 200 
          endif          
          if(b.eq.1) then
             reduce = .true.
             return
          endif 
          i = i + 1 
          goto 100 
          return 
          end 

! SOLVING PELL EQUATION 
        subroutine solvep(cfe,len,x,y,maxv,solved)
        implicit none
        integer  cfe(0:*), len
        integer  x,y
        integer  i,limit
        integer  h2,h1,h0,k2,k1,k0
        logical  solved 
        integer  maxv
! AS MATHWORLD SAYS, IF
!
!       LEN - 1 ODD    : LEN-1 CONVERGENT 
!       LEN - 1 EVEN   : 2 * ( LEN - 1 ) + 1 
        
        solved = .true.

        if(len.lt.0) then
          stop
        endif 

        h2 = cfe(0)
        k2 = 1
        
         
         if(len.lt.1) then 
           stop
         endif 

         h1 = cfe(1)*cfe(0)+1
         k1 = cfe(1)
!        
         if(len.eq.1.or.len.eq.2) then 
           x = h1  
           y = k1
           return
         endif 
!
!        
                               limit = len - 1 
         if(mod(len-1,2).eq.0) limit = 2 * limit + 1 
!         
         do 100 i = 2 , limit 
! BUILD RECURRENCE 
           h0 = cfe(mod(i-1,len)+1) * h1 + h2 
           k0 = cfe(mod(i-1,len)+1) * k1 + k2 
!
           if(h0.gt.maxv) then 
             solved = .false.
             return
           endif 
!
          h2 = h1
          k2 = k1
          h1 = h0
          k1 = k0

100       continue 

          x = h0
          y = k0

200        continue 
           return
           end 
!
        subroutine cfesq(m,maxlen,res,len,gotsq,gotpe)
        implicit none
        integer  m,maxlen,res(0:maxlen),len
        logical  gotsq,gotpe
        integer  n1,n0,r1,r0,d1
        integer  i,limit

!      
! WE START AT ITERATION 0, SETTING A0 = INT(SQRT(m)) 
! NOW, LET US DEFINE 
!
!    d(i) = m - n(i-1)*n(i-1)
!    r(i) = d(i)/r(i-1) 
!    n(i) = a(i) * r(i) - n(i-1)
! with initial condition r(0) = 1 n(0)=a(0)=int(sqrt(m))
! we can proove by induction that r(i-1) divides always d(i)
!  
! at each iteration i.ge.1 we deal with the situation
!               r(i-1)                    sqrt(m) - n(i)
! i step )   -----------------  = a(i) + ----------------   
!             sqrt(m) - n(i-1)                 r(i)  
!
! due to result proove by Lagrange, we either have square number or
! quadratic surd, so we either terminate when a(n) = 0 or a(n) = 2 a(0)  
! and then the number will repeat 
!       
        if(maxlen.lt.0) then
           print *, 'maxlen to small'
           stop
        endif 
        
        res(0) = int(sqrt(real(m)))
        r0 = 1 
        n0 = res(0)

        gotsq = .false.
        gotpe = .false.

        if(m-n0*n0.eq.0) then 
          gotsq = .true.
          return
        endif 

        do 100 i = 1 , maxlen  

           d1     =  m - n0 * n0 
           r1     =  d1/r0 
           res(i) = int((sqrt(real(m))+n0)/r1)
!
! SEE IF PERIOD IS FOUND 
!
           if(res(i).eq.2*res(0)) then 
            gotpe = .true.
            len   = i
            goto 200 
           endif 

           n0     = res(i) * r1 - n0
           r0     = r1 

100        continue 
                   
200        continue

         return
         end                  






        subroutine frmprd(n,p,prod)
        implicit none
        integer  n, p(n), prod(2**n)
        integer  i,j,m
!
!
! SET ALL PRODUCTS TO 1 
!
        do 10 i = 1 , 2**n 
10        prod(i) = 1 
!       
        do 100 m = 1 , n 
        do 100 i = 1 + 2**(m-1), 2**n, 2**m
        do 100 j = i , min(i + 2**(m-1)-1,2**n )
100         prod(j) = prod(j) * p(m)
        
!        do i = 1 , 2**n
!          print *, prod(i) 
!        enddo 

        return 
        end 
                               

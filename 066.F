!gfortran -fdefault-integer-8 -fdefault-real-8 66.F 
!euler 66 =    661
!
!real    0m0.006s
!user    0m0.003s
!sys     0m0.003s
!> @author IWB
!> @brief  Euler66
#define LIMIT 100
#define MAXEXP 3
        program eu66
        implicit none 
        integer  n,m
        integer  cfe(LIMIT+1)
        integer  lfe
        real*8   x,y
        real*8   maxx
        integer  maxd
        integer  i

        logical  gotsq,gotpe
        
        maxx = 0 
        maxd =-10 

        do 100 i = 1 , 10**MAXEXP
        lfe = LIMIT
! GET THE CONTINUED FRACTION
        call  cfesq(i,LIMIT,cfe,lfe,gotsq,gotpe)
         if(gotsq) goto 100
         if(.not.gotpe) then
            print *, 'error',i
            stop
         endif 
! SOLVE THE PEN EQUATION BY CONVERGENTS. 
        call solvep(cfe,lfe,x,y)
        if(x.gt.maxx) then 
        maxx = max(x,maxx)
        maxd = i 
        endif 

100     continue

        write(*,1800) maxd

1800    format('euler 66 = ',I6)

        return
        end 


        subroutine solvep(cfe,len,x,y)
        implicit none
        integer  cfe(0:*), len
        real*8   x,y
        integer  i,limit
        real*8   h2,h1,h0,k2,k1,k0
        logical  solved 
 
!
! AS MATHWORLD SAYS, IF
!
!       LEN - 1 ODD    : LEN-1 CONVERGENT 
!       LEN - 1 EVEN   : 2 * ( LEN - 1 ) + 1 
!
        if(len.lt.0) then
          stop
        endif 

        h2 = real(cfe(0))
        k2 = 1.0d0
        
         
         if(len.lt.1) then 
           stop
         endif 

         h1 = real(cfe(1))*real(cfe(0))+1.0d0
         k1 = real(cfe(1))
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
           h0 = real(cfe(mod(i-1,len)+1)) * h1 + h2 
           k0 = real(cfe(mod(i-1,len)+1)) * k1 + k2 

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





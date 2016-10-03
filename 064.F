!Euler 64 =       1322
!
!real    0m0.019s
!user    0m0.018s
!sys     0m0.001s
!> @author IWB
!> @brief  Euler 64
#define MAXLEN  1000
        program eu64
        implicit none
        integer  m,len,res(0:MAXLEN)
        logical  gotsq,gotpe

        integer  i 
        integer  odd 

        odd = 0 
        do 100 i = 1 , 10000
        call cfesq(i,MAXLEN,res,len,gotsq,gotpe)
          if(.not.gotsq) then 
             if(.not.gotpe) then
             print *, 'did not find period',i
             stop
             endif 
             if(mod(len,2).eq.1) odd = odd + 1 
          endif    
100     continue 

        write(*,1800) odd

1800    format('Euler 64 = ', I10)

        return
        end         




        subroutine cfesq(m,maxlen,res,len,gotsq,gotpe)
        implicit none
        integer  m,maxlen,res(0:maxlen),len
        integer  n1,n0,r1,r0,d1
        integer  i 
        logical  gotsq,gotpe

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




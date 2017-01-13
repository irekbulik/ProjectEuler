! NOTE THAT THE PROBABILITY THAT ITH COIN IN N LONG CHAIN IS NOT FLIPPED  
!                         (I-1)**2 + (N-I)**2
!               P(I,N) = ---------------------
!                                 N**2 
! 
! PROBABILITY THAT AFTER K TURNS COIN I IS WHITE SIDE UP, ASSUMING IT 
! STARTED LIKE THAT IS 
!                             1  [ (             )      ]
!               W(I,K,N) =   --- [ ( 2 P(I,N) -1 )  + 1 ]
!                             2  [ (             )      ]
!
! FOR ANY P [0,1] (2P-1)**K -> 0 AS K-> INF 
!
!
! [0] gfortran -fdefault-real-8 -fdefault-integer-8 -O3 430.F -o 430 ; time ./430 
!Answer 430 =    5000624921.38
!
!real    0m34.664s
!user    0m34.587s
!sys     0m0.000s
        program eu430
        implicit none
        integer  i,k,n
        real*8   tot,tol
        real*8   p,w
        p(i,n)   = (real(i-1)**2+real(n-i)**2)/(real(n)**2)
        w(i,n,k) = ((2*p(i,n)-1.0d0)**k + 1.0d0)/2.0d0
!       
        k = 4000
        n = 10**10
        tot = 0.0d0
!        
! WE WANT ~3 DECIMAL DIGITS WITH MAX 10**10 TERMS 
!
        tol = 10.0d0**(-12.0d0/K) 
!
        do 100 i = 1 , n/2 
           if(abs(2*p(i,n)-1.0d0).lt.tol) goto 200 
           tot = tot + 2 * w(i,n,k)
100     continue 

200     continue 
        i = i - 1
! REST CONTRIBUTES 0.5 
        tot = tot + (n-2*i)/2.0d0
        write(*,1800) tot
1800    format('Answer 430 = ', F16.2)
        return
        end 

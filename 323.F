#define max_bit 32
!Answer 323 =   6.3551758451
!
!real    0m0.004s
!user    0m0.001s
!sys     0m0.003s
!
        program eu323 
        implicit none
        real*8   nz(0:max_bit)
        integer  i,j 
        real*8   rbin
        real*8   f12 
        data     f12/0.50d0/
        nz(0) = 0.0d0
        do 10 i = 1 , max_bit                 
           nz(i) = 0.0d0
           do 20 j = 1 , i  
20          nz(i) = nz(i) +  rbin(i,j)*(1.0d0+nz(i-j))
10          nz(i) = (nz(i) + 1.0d0)*(f12**i/(1.0d0-f12**i))

        write(*,1800) nz(max_bit)     
        
1800    format('Answer 323 = ',F14.10)

        return
        end 

        real*8 function rbin(n,m)
        implicit none
        integer  n,m,k
        rbin = n*1.0d0
         do 10 k = 1 , m - 1
10          rbin = rbin * ( n - k ) / ( k + 1 )
        return
        end 
          

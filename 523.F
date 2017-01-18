!Answer 523 =      37125450.44
!
!real    0m0.003s
!user    0m0.000s
!sys     0m0.003s
        program eu523
        implicit none
        real*8   e(30)
        real*8   s 
        integer  i 
        integer  j 
        integer  n 
        
        e(2) = 0.5d0
        n    = 30 
!
! THE LAST DIGIT IT TOUCHED AT THE END. IT CAN BE ONE OF THE N NUMEBRS,
! BUT ALL THE PREVIOUS ONES ARE ALREADY SORTED.
!       
        do i = 3 , 30
         s = 0.0d0
         do j = 1 , i-1 
            s = s + 2.0d0**(j-1)
         enddo
         e(i) = e(i-1) + s/i
        enddo
        
        write(*,1800) e(30)

1800    format('Answer 523 = ',F16.2) 
        return
        end 



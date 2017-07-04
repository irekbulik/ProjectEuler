! [0] gfortran -fdefault-integer-8 -fdefault-real-8  211.f95 -o 211 ; time ./211 
!           1922364685
!
!real    0m24.124s
!user    0m23.848s
!sys     0m0.099s

        program Eu211
        implicit none
        integer , parameter  :: n = 64000000 -1 
        integer              :: sig2(n)
        integer              :: i , tot 

        call sive_sigma2(n,sig2) 

        tot = 0 
        do i = 1 , n 
           if(is_sq(sig2(i))) tot = tot + i 
        enddo 
        print *, tot 


        return 

        
        contains

        subroutine sive_sigma2(n,s2)
        implicit none
        integer , intent(in)  :: n
        integer               :: s2(n)
        integer               :: i , k 

        s2 = 1 
        do i = 2 , n 
           k = 1 
           do while(k*i.le.n)  ; s2(k*i) = s2(k*i) + i*i ; k = k + 1 ; enddo
        enddo
        
        return
        end subroutine 

        elemental function is_sq(n)
        implicit none
        logical             :: is_sq
        integer, intent(in) :: n 
        
        is_sq = floor(sqrt(real(n)))**2.eq.n.or.(floor(sqrt(real(n)))+1)**2.eq.n

        return
        end 


        end program 

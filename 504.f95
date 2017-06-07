! This is so simple that i do not need to enforce any symmetry.
!
! gfortran -fdefault-real-8 -O3 504.f95  ; time ./a.out 
! Answer 504      694687
!
! real    0m0.858s
! user    0m0.854s
! sys     0m0.001s
!
        program Eu504 
        implicit none
        integer, parameter ::  max_size=100
        integer            ::  ct(max_size,max_size) 
        integer            ::  a,b,c,d,i,j
        logical            ::  is_sq
        real*8             ::  val

        ct = 0 
        do b = 1 , max_size
        do a = 1 , max_size 
        do i = 1 , a-1
           j = 1
           do while (real(j).lt.val(a,b,i)) 
           ct(a,b) = ct(a,b) + 1 
           j = j + 1 
           enddo
        enddo
        enddo
        enddo
        

        i = 0 
        do d = 1 , max_size 
        do c = 1 , max_size 
        do b = 1 , max_size 
        do a = 1 , max_size 
           if(is_sq(a+b+c+d-3+ct(a,b)+ct(a,d)+ct(c,b)+ct(c,d))) i = i + 1 
        enddo
        enddo
        enddo
        enddo
          print *, "Answer 504", i

        end

        function val(a,b,i)
        implicit none
        integer     :: a,b,i
        real*8      :: val 
             val = real(b)-real(b*i)/real(a)
        return
        end 

        function is_sq(n)
        implicit none
        integer      :: n 
        logical      :: is_sq
          if(n.lt.0) stop
          is_sq = int(sqrt(real(n)))**2.eq.n
        return
        end 



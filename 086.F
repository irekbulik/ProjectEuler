! [0] gfortran -fdefault-integer-8 -fdefault-real-8 -O3 086.F -o 086 ; time ./086
! Answer 086                  1818
!
!real    0m0.029s
!user    0m0.028s
!sys     0m0.001s

        program Eu086
        implicit none
        integer  a,b,t
        integer  f      
        logical  is_sq
        t = 0
        a = 0
10      a = a + 1 
        do b = 1 , 2 * a 
           if(is_sq(a**2+b**2)) t = t + f(b,a) 
        enddo
           if(t.lt.10**6) goto 10

        print *, 'Answer 086 ', a


        end 


        logical function is_sq(a)
        implicit none
        integer  a 
          is_sq = a .eq. int(sqrt(real(a)))**2 
        return
        end 
       


        integer function f(x,y)
        implicit none
        integer x,y
        f = x/2
        if(x-1.gt.y) f = f - (x-y-1)
        return
        end 

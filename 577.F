! Answer 577 =    265695031399260211
!
!real    0m0.088s
!user    0m0.084s
!sys     0m0.003s

#define MAX_SIZE 12345
        program Eu577 
        implicit none
        integer*8  nh
        integer*8  k 
        integer*8  ans, km1
        
        km1 = 0 
        ans = 0
        do k = 3 , MAX_SIZE
           km1 = km1 + nh(k)
           ans = ans + km1
        enddo        
        
        print *, "Answer 577 = ", ans 

        return
        end 

        integer*8 function nh(n)
        implicit none
        integer*8  n,siz,len
! COMPUTES HOW MANY HEXAGONS WE CAN ADD, THAT DO INCLUDE THE LINE N.
! OTHER ARE COUNTED. THERE ARE POINTS 1...N+1 AND EACH OF THEM CAN BE
! MEMBER OF HEXAGON WITH DIFFERENT SIZE. 
                
        nh  = 0 
        siz = 0

10      siz = siz + 1  
        if(3*siz.gt.n) return
        nh  = nh + siz*(n-3*siz+1)
        goto 10 
        return
        end 


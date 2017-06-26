! 40 min, not proud but i am too lazy to optimize it now and i don't
! expect to revisit this one.
! 
! answer 4989
! 
        program Eu135
        implicit none
        integer,  parameter   :: maxn = 10**6-1
        integer               :: a,x,n(maxn),v,tot

        n = 0 
        do a = 1 , maxn 
        do x = a , 6*a 
           v = 6*a*x-x*x-5*a*a
           if(v.ge.1.and.v.le.maxn.and.x-2*a.gt.0) n(v) = n(v)+1
        enddo
        enddo 

        tot = 0 

        do a = 1 , maxn
           if(n(a).eq.10) tot = tot + 1 
        enddo

        print *, tot 

        return
        end 



        program Eu136
        implicit none
        integer,  parameter   :: maxn = 5*10**7-1
        integer               :: a,x,n(maxn),v,tot

        n = 0 
        do x = 1 ,3*maxn/4
        a = x/3+1
          do while((3*a-x)*(a+x).le.maxn) 
           n((3*a-x)*(a+x)) = n((3*a-x)*(a+x))+1
           a = a + 1 
          enddo 
        enddo

        tot = 0 
        do a = 1 , maxn
           if(n(a).eq.1) tot = tot + 1 
        enddo

        print *, tot 

        return
        end 


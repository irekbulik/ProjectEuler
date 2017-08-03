!
! well, that is an easy problem where the only trick is to use bayes way to compute the probability 
! of choosing healthy sheep if we know to that some are sick. We ask what is the chance of choosing i healthy sheep out of s sheep 
! then we know that at least one is sick. that is pb(i,s,p)
! The rest, is some micro to avoid doing the min in the inner loop, but that is trivial and I am not fixing it now. 
! I will run in few minutes instead of microseconds. 
! 
! about 7 minutes
! Answer =   378563.260589
!
!
!
        program Eu352 
        implicit none
        integer  :: i 
        real     :: answer
        answer = 0.0
        do i = 1, 50
        answer = answer +  Expected_Number_Of_Tests(i/100.,10000)
        enddo 
        
        write(*,1800) answer 
1800    format('Answer = ',F15.6)        
        contains 

        real function Expected_Number_Of_Tests(p,n)
        implicit none
        real   ,intent(in)      :: p       
        integer,intent(in)      :: n 
        
        integer                 :: i,k
        real,allocatable        :: e0(:),e1(:)
        real                    :: min_e0,min_e1
        real,parameter          :: one=1.0

!        integer                 :: pb

        allocate(e0(0:n))
        allocate(e1(0:n))
         
        e0(0) = 0.0
        e0(1) = 1.0 
        e1(1) = 0.0 

        do k = 2 , n 
           min_e1 = pb(1,k,p)*(one+e1(k-1)) + (one-pb(1,k,p))*(one+e0(k-1)+e1(1))                       
           do i = 2 , k - 1 
              min_e1 = min(min_e1,pb(i,k,p)*(one+e1(k-i)) + (one-pb(i,k,p))*(one+e0(k-i)+e1(i)))
           enddo 
           e1(k) = min_e1
           min_e0 = (one-p)*(one+e0(k-1)) + p * (one + e1(1)+e0(k-1))
           do i = 2 , k 
              min_e0 = min(min_e0,(one-p)**i*(one+e0(k-i)) + (one-(one-p)**i) * (one + e1(i)+e0(k-i))) 
           enddo 
           e0(k) = min_e0
!
        enddo 
        
        Expected_Number_Of_Tests = e0(n)        

        deallocate(e1)
        deallocate(e0)

        return
        end 
                
        pure real function pb(i,s,p)
        implicit none
        integer,intent(in)   :: i,s
        real   ,intent(in)   :: p 
            pb = (1.0-(1.0-p)**(s-i))*(1.0-p)**i
            pb = pb/(1.0-(1.0-p)**s)
        return
        end 


        end 

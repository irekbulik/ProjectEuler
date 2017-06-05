!
! Rather self explanatory 
!
! Answer 485          51281274340
! real    0m27.378s
! user    0m26.858s
! sys     0m0.173s
!

        program eu485
        implicit none
        integer,parameter  :: maxu=10**8,k=10**5,maxd=maxu+k
        integer            :: sigma0(maxd) 
        integer            :: max_val_window,pos_val_window
        integer            :: n 
        integer            :: ans
        integer            :: locate

        call form_divisor_sigma0(maxd,sigma0)
!
! BEGIN A SCAN. FIND THE MAXIMUM VALUE IN THE WINDOW AND THE POSITION OF IT. 
!
        max_val_window = maxval(sigma0(1:k)) 
        pos_val_window = maxloc(sigma0(1:k),1)
!
! BEGIN WALKING THE ARRAY, CHECKING THE MAXIMA.
!
        ans = 0
!
        do n = 1 , maxu-k+1
! IF WE FOUND A NEW MAXIMUM IN THE RANGE, WE ARE FINE.
           if(sigma0(n+k-1).gt.max_val_window) then
              max_val_window = sigma0(n+k-1) 
              pos_val_window = n+k-1
           endif                     
! THE PROBLEM APPEARS IF WE ARE BEYOND THE WINDOW OF THE PREVIOUS MAXIMUM. 
           if(n.gt.pos_val_window) then 
             max_val_window =   maxval(sigma0(n:n+k-1)  )
             pos_val_window = n+maxloc(sigma0(n:n+k-1),1)-1 
           endif
        ans = ans + max_val_window
        enddo 

        print *, 'Answer 485' , ans


        end
        
        subroutine form_divisor_sigma0(maxd,sigma0)
        implicit none 
        integer       :: maxd, sigma0(maxd)
        integer       :: i , j
        sigma0    = 2
        sigma0(1) = 1  
        do j = 2 , maxd/2
           do i = 2*j,maxd,j
              sigma0(i) = sigma0(i) + 1 
           enddo 
        enddo  

        return
        end subroutine


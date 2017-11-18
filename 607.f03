!Answer 607 =  13.1265108586
!
!real	0m0.004s
!user	0m0.001s
!sys	0m0.002s
!
! we start at A at (0,0) and go to B at (100/sqrt(2),100/sqrt(2)) going straight lines in constant velocity areas.
! we parameterize each path with angle 
!
! t = tf + sum_i(x_i/v_i/cos(theta_i)) for i = 1 , 6 
!
! tf = is the path from p6 to B. that path is hypotenuse of triangle (100/sqrt(2)-50)/2 and 100/sqrt(2)-p6.
! y coordinate of p6 is sum_i(x_i*tan(theta_i)) for i = 1 , 6  
!
       program Euler607 
       implicit none
       integer,parameter:: dp=selected_real_kind(15,300)
       real(kind=dp)    :: pi=4*Atan(1.0_dp)
       real(kind=dp)    :: theta(6), vel(7),x1,x2,p6,to,tn,tf,dtfdp6,dt
       integer          :: i 
       

       do i = 1 , 6
         theta(i) = pi/4
       enddo
       vel(1) = 10.0_dp
       vel(2) =  9.0_dp
       vel(3) =  8.0_dp
       vel(4) =  7.0_dp
       vel(5) =  6.0_dp
       vel(6) =  5.0_dp
       vel(7) = 10.0_dp 
       x1 = (100.0_dp/sqrt(2.0_dp)-50)/2
       x2 = 10.0_dp
       
       to = 14.0_dp 

       do while(.true.) 
          p6 = x1*tan(theta(1))
          do i = 2 , 6
          p6 = p6 + x2*tan(theta(i))
          enddo 
          tf     = sqrt(x1*x1+(100.0_dp/sqrt(2.0_dp)-p6)**2)/vel(7)
          tn     = x1/vel(1)/cos(theta(1))+tf
          do i = 2 , 6 
          tn = tn + x2/vel(i)/cos(theta(i))    
          enddo 
          if(abs(to-tn).lt.1.0e-14_dp) exit  
          to = tn 
          dtfdp6 = -(100.0_dp/sqrt(2.0_dp)-p6)/tf/vel(7)**2          
          ! update the angles. Make little dumping so it does not diverge at the start  
          do i = 1 , 6 
          dt = asin(-dtfdp6*vel(i))-theta(i)
          theta(i) = theta(i) + dt/5
          enddo    
       enddo 

       write(*,'("Answer 607 = ",F14.10)') tn 

       end 
               

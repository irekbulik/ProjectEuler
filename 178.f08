! gfortran -fdefault-integer-8  -O3 178.f08 -o 178.x ; time ./178.x
! Answer 178 =          126461847755
!
!real	0m0.005s
!user	0m0.002s
!sys	0m0.002s
       program Eu178
       implicit none
       integer,parameter   :: max_size=40
       integer             :: states(0:999)
       integer             :: k,tot=0

       states=0
       do k = 1 , 9 
       states(ind(k,k,k))=1
       enddo

       do k = 2,40
          call make_step(states)
          tot = tot + pan_num(states)
       enddo

       print *, "Answer 178 = " , tot 


       contains


       pure integer function pan_num(states)
       implicit none
       integer, intent(in) :: states(0:999)
       integer             :: i 
       
       pan_num = 0 
       do i = 0,9
          pan_num = pan_num + states(ind(i,0,9))
       enddo
       return
       end 



       subroutine make_step(states)
       implicit none
       integer,intent(inout) :: states(0:999)
       integer               :: i,ld,mind,maxd
       integer               :: pld,pmind,pmaxd
       integer,allocatable   :: new_state(:)

       allocate(new_state(0:999))
       new_state=0       

       do i = 0 , 999
          ld   = leading_digit(i)
          mind = minimum_digit(i)
          maxd = maximum_digit(i)
          if(ld>0) then 
             pld   = ld-1
             pmind = min(pld,mind)
             pmaxd = maxd 
             new_state(ind(pld,pmind,pmaxd)) = new_state(ind(pld,pmind,pmaxd))+states(i)   
          endif 
          if(ld<9) then 
             pld   = ld+1
             pmind = mind
             pmaxd = max(maxd,pld) 
             new_state(ind(pld,pmind,pmaxd)) = new_state(ind(pld,pmind,pmaxd))+states(i)   
          endif 
       enddo
       states=new_state
       deallocate(new_state)
       return 
       end        
       



       pure integer function leading_digit(i) result(ld)
       integer,intent(in) :: i 
       ld = mod(i,10)
       return
       end 

       pure integer function minimum_digit(i) result(md)
       integer,intent(in) :: i 
       md = mod(i/10,10)
       return
       end 

       pure integer function maximum_digit(i) result(ld)
       integer,intent(in) :: i 
       ld = i/100
       return
       end 

       pure integer function ind(ld,min_d,max_d) result(i)
       integer,intent(in) :: ld,min_d,max_d
       i = 100*max_d+10*min_d+ld
       return
       end 


       end 

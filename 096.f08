! [0] gfortran -O3 096.f08 -o 096 ; time ./096
! Euler 096 =        24702
!
!real	0m0.602s
!user	0m0.591s
!sys	0m0.005s

       program Eu096
       implicit none
       integer  :: puz(9,9)
       integer  ::ns,i
       integer  :: ans 

       ns  = 0
       ans = 0
       open(file="p096_sudoku.txt",unit=11,status="old")
       do while(ns<50) 
          call read_grid(11,puz)
          ns = ns +1 
          if(.not.solve_grid(puz)) then 
            print *, "error"
            stop
          endif
          ans=ans+100*puz(1,1)+10*puz(1,2)+puz(1,3)
       enddo        
       print * , "Euler 096 = ", ans

       close(11)

       contains

       recursive logical function solve_grid(puz) result(ok)
       implicit none 
       integer      :: puz(9,9),puzk(9,9)
       integer      :: i,j,is,js,k
       logical      :: found
       
       ok = .false.
       found = .false.
       do j = 1 , 9
       do i = 1 , 9
          if(puz(i,j)==0) then 
             found = .true.
             is=i
             js=j
             exit
           endif
       enddo
           if(found) exit 
       enddo

       if(.not.found) then 
           ok = .true.
           return 
       endif 

       do k = 1 , 9 
          puz(is,js) = k 
          if(ver_grid(puz)) then 
            if(solve_grid(puz)) then
                ok = .true.
                return 
            endif 
          endif
          puz(is,js) =0  
       enddo 
       
       return
       end function


       subroutine read_grid(fil,puz)
       implicit none
       integer  :: fil,puz(9,9)
       integer  :: il
       
       read(fil,*)
       do il  = 1 , 9 
       read(11,'(9(I1))') puz(il,1:9)
       enddo
       return

       end subroutine 

       pure logical function ver_grid(puz)
       implicit none
       integer,intent(in) :: puz(9,9)
       integer            :: i,j,ii,jj,nums(0:9)
       ver_grid = .true.
       do i=1,9
          nums=0
          do j=1,9
             nums(puz(i,j))=nums(puz(i,j))+1
             if(puz(i,j)/=0.and.nums(puz(i,j))>1) then 
                ver_grid = .false.
                return 
             endif         
          enddo     
       enddo 

       do i=1,9
          nums=0
          do j=1,9
             nums(puz(j,i))=nums(puz(j,i))+1
             if(puz(j,i)/=0.and.nums(puz(j,i))>1) then 
                ver_grid = .false.
                return 
             endif         
          enddo     
       enddo 

       do i=1,3
          do j=1,3
            nums=0
            do ii = (i-1)*3+1,i*3
            do jj = (j-1)*3+1,j*3  
             nums(puz(jj,ii))=nums(puz(jj,ii))+1
             if(puz(jj,ii)/=0.and.nums(puz(jj,ii))>1) then 
                ver_grid = .false.
                return 
             endif         
             enddo
             enddo
          enddo     
       enddo 
       return 
       end function


       end 


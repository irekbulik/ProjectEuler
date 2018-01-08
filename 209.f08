! [0] gfortran -O3 -fdefault-integer-8 209.f08 -o 209.x ; time ./209.x
! Answer 209 =        15964587728784
!
!real	0m19.972s
!user	0m19.608s
!sys	0m0.111s
       program test
       implicit none  
       integer ::  i,k
       integer ::  b2b(0:63)
       integer ::  sb
       integer ::  n_cyc=1,cyc_len
       integer ::  ans=1,tot

       do i = 0,63
          b2b(i) = tran_bin(i)
       enddo

       do while(.true.) 
         i = -1
         do k = 0,63
            if(b2b(k)/=-1) then 
              i = k 
              exit
            endif 
         enddo 
         if(i==-1) exit  
         sb = i 
         cyc_len = 1
         tot     = 0 
         if(b2b(i)==sb) then 
            b2b(i)=-1
         else 
           do while(b2b(i)/=sb)
              k = i 
              i = b2b(i)
              b2b(k) = -1
              cyc_len = cyc_len + 1 
           enddo
             b2b(i) = -1
         endif 
           n_cyc=n_cyc+1
           tot = iand_to_0(0,0,2,cyc_len) + iand_to_0(1,1,2,cyc_len) 
           ans = ans * tot 
       enddo       
       if(sum(b2b)/=-64) then
          print *,"Logical error, not all cycles identified"
          stop
       endif 
       print *, "Answer 209 = " , ans 

       contains

       recursive integer function iand_to_0(vofb,vopb,pos,len) result(cnt)
       implicit none
       integer,intent(in)  :: vofb,vopb,pos,len
       
       if(len==1) then
          cnt = 1-vofb
          return
       endif 

       if(pos==len) then
          if(vopb==1.or.vofb==1) then
            cnt = 1
          else
            cnt = 2 
          endif 
       else
                       cnt =       iand_to_0(vofb,0,pos+1,len)
           if(vopb==0) cnt = cnt + iand_to_0(vofb,1,pos+1,len) 
       endif 

       !print *, vofb,vopb,pos,len,cnt

       return
       end        


       integer function tran_bin(i)
       implicit none
       integer,value ::  i 
       integer       ::  j, bin(6)
       integer       ::  a,b,c
       
       do j = 1,6
          bin(j)=mod(i,2)
          i = i/2
       enddo

       a = bin(6)
       b = bin(5)
       c = bin(4)

       do j = 6,2,-1 
          bin(j) = bin(j-1)        
       enddo   
          bin(1) = xor(a,iand(b,c))

       tran_bin=0

       i = 1
       do j = 1 , 6 
          tran_bin = tran_bin + bin(j)*i
          i = i * 2 
       enddo

       return
       end 


       end 





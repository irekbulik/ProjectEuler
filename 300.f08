![0] gfortran -O3 -ffast-math 300.f08 -o 300.x ; time ./300.x
!      263916
! Euler 300 answer =    8.0540771484375000
!
!real	3m42.655s
!user	3m34.597s
!sys	0m2.387s
!
! well, even if i use inversion of string symmetry i wont cut under minute. probably i should rethink it.
!
       program Eu300
       implicit none 
       integer,parameter :: n_max=15
       integer           :: bin(n_max),map(-n_max:n_max,-n_max:n_max)
       integer           :: i,tot,l1,n1,k

       tot=0
       do i = 0,2**n_max-1 
         map =-1
         bin = 0
         call to_bin(i,bin)
         l1=0
         n1=0
         do k = 1 , n_max
            if(bin(k)==1) then
               l1=k
               n1=n1+1 
            endif
         enddo 
         l1 = min(n_max,l1+1)
         l1 = n_max
         if(n1>1) then 
           map(0,0) = bin(1)
           tot=tot+get_bonds(0,1,2,l1)
         endif
       enddo 
       print *, tot
       print *, "Euler 300 answer = " , real(tot,8)/2**n_max

       contains

       recursive integer function get_bonds(i,j,cp,l1) result(bonds)
       implicit none 
       integer   :: i,j,cp,cb,l1
       logical   :: moved
       ! if arrives at bad move, u must make sure that you not consider that fold
       bonds=-n_max**2
       map(i,j)=bin(cp)
       cb = add_bonds(i,j)
       if(cp<l1) then
                if(map(i+1,j)==-1.and.cp>2) bonds=max(bonds,cb+get_bonds(i+1,j,cp+1,l1))
                if(map(i-1,j)==-1)          bonds=max(bonds,cb+get_bonds(i-1,j,cp+1,l1))
                if(map(i,j-1)==-1)          bonds=max(bonds,cb+get_bonds(i,j-1,cp+1,l1))
                if(map(i,j+1)==-1)          bonds=max(bonds,cb+get_bonds(i,j+1,cp+1,l1))
       else
              bonds=cb
       endif   
       map(i,j)=-1
       return
       end 


       integer function add_bonds(i,j)
       implicit none 
       integer,intent(in) :: i,j
       add_bonds=0
       if(map(i,j)==1) then
          if(map(i+1,j)==1) add_bonds=add_bonds+1    
          if(map(i-1,j)==1) add_bonds=add_bonds+1    
          if(map(i,j+1)==1) add_bonds=add_bonds+1    
          if(map(i,j-1)==1) add_bonds=add_bonds+1    
       endif
       return
       end 


       subroutine to_bin(i,b)
       implicit none 
       integer,value :: i 
       integer       :: b(n_max)
       integer       :: k
       k = 1
       do while(i/=0)
         b(k)=iand(i,1)
         i=i/2
         k=k+1
       enddo
       return
       end 


       end 




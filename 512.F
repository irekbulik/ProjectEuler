#define __MAX__ 5*10**8
!Answer 512  50660591862310323
!
!real    0m26.335s
!user    0m25.840s
!sys     0m0.289s
! After one sees that
!     n^n - 1 
!   ----------- = mod(n,2) we are done. 
!      n - 1  
!

        program Eu512
        implicit none
        integer   Phi(__MAX__)
        integer   k
        integer*8 tot   
        
        call sivphi(__MAX__,phi)
        tot = 0 
        do 10 k = 1 , __MAX__,2 
10       tot = tot + phi(k)
        write(*,1800) tot 
1800    format('Answer 512 ',I18)

        return 
        end 

        subroutine sivphi(n,phi)
        implicit none
        integer  n,phi(*)
        integer  j,k
        do 10 j = 1 , n 
10        phi(j) = j 
        do 20 j = 2 , n 
           if(phi(j).eq.j) then 
             phi(j) = j - 1 
             k = 1
30           k = k + 1 
             if(k*j.gt.n) goto 40
             phi(k*j) = phi(j*k)-phi(j*k)/j
             goto 30 
40           continue
           endif 
20      continue
        return
        end 
        

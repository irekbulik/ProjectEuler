! Probably can make it even faster. 
!
! Answer =          209110240768
!
!real    0m33.275s
!user    0m32.982s
!sys     0m0.053s
!
! I thought we would have issue with the rotational clashes, but seems we do not. 
! I can probably take it under minute with some effort.
!
        program Eu265
        implicit none
        integer, parameter    :: big_n=5
        integer               :: arr(2**big_n),vals(2**big_n),dig(big_n)
        integer               :: i,tot 
        tot = 0 
! It has to start with n zeros, and then we have to have a one, the number must end with 1.
         do i = 2**(2**big_n-big_n-1)+1, 2**(2**big_n-big_n),2
           call dec_to_bin(i,arr,2**big_n) 
           if(is_unique(big_n,arr,vals,dig)) then
             tot = tot + i
             print *, i 
           endif 
        enddo 
        print *, 'Answer = ' , tot 

        return

        contains

        integer pure function bin_to_dec(n,arr)
        integer, intent(in)   :: n, arr(n) 
        integer               :: i 
        
        bin_to_dec = 0 
        do i = 1 , n 
          bin_to_dec = bin_to_dec + arr(i)*2**(i-1)
        enddo 

        return
        end function 


        subroutine dec_to_bin(val,arr,n)
        integer    :: val, n, arr(n) 
        integer    :: l_val,i
        
         if(val.gt.2**n-1) then 
           print *, val,2**n-1
           stop 
         endif  
         
         l_val = val 

         do i = n , 1 , -1 
            arr(i) = mod(l_val,2)
            l_val  = l_val/2
         enddo 

        return
        end subroutine 

        logical function is_unique(n,arr,vals,dig)
        integer      :: n,arr(2**n),vals(0:2**n-1),dig(n)  
        integer      :: i,j,num

        vals = 0 
        
        is_unique = .true.

        do i = 1 , 2**n 
           num = 0 
           do j = 1 , n
           num = num + arr(cyc_index(i+j-1,2**n))*2**(n-j)
           enddo 
           if(vals(num).eq.1) then
             is_unique=.false.
             return 
           else 
             vals(num) = 1 
           endif 
        enddo 

        return
        end function 


        integer pure function cyc_index(i,n) 
        integer, intent(in) :: i , n 
        
          cyc_index = mod(i-1,n)+1
         
        return
        end 



        end program 

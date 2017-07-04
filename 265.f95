! more than a minute, but ok for now, with O3 we have 90 sec.
!
! Answer =          209110240768
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
        integer      :: n,arr(2**n),vals(2**n),dig(n)  
        integer      :: i,j


        do i = 1 , 2**n 
           do j = 1 , n
           dig(j) = arr(cyc_index(i+j-1,2**n))
           enddo 
           vals(i) = bin_to_dec(n,dig)
        enddo 

        call sort(vals,2**n)

        is_unique = .true.
        do i = 2 , 2**n
           if(vals(i).eq.vals(i-1)) is_unique = .false.
           if(.not.is_unique) return
        enddo 

        return
        end function 


        integer pure function cyc_index(i,n) 
        integer, intent(in) :: i , n 
        
          cyc_index = mod(i-1,n)+1
         
        return
        end 

        subroutine sort(a,n)
        implicit none
        integer  :: a(*),n
        integer  :: ne,nb,bc,w,b
! HIRING PART. WE START WITH N/2+1 HIRED PEOPLE AND WE EMPLOY POTENTIAL 
! N/2 BOSSED
        ne = n
        nb = n/2+1
        if(n.le.1) return 
200     continue         
        if(nb.gt.1) then 
! HIRE GUY AND THEN ASSES HIM            
           nb = nb - 1 
           bc = a(nb)
        else 
! RETIRE THE TOP GUY, TAKE THE LAST ONE AND GIVE HIM THE PLACE. THEN
! VERIFY  
           bc    = a(ne)
           a(ne) = a(1) 
           ne    = ne - 1 
             if(ne.eq.1) then 
                a(1) = bc 
                return
             endif 
!
         endif 
! SET AS THE BOSS THE ITH ELEMENTS 
        b = nb 
        w = 2 * b 
! IN THE HIRE LOOP, FOR BOSS B WE MAKE SURE THAT NO WORKER W
! SUPERVISED BY B HAS BETTER QUALIFICATIONS. THE WORKERS ARE 
! 2*B AND 2*B + 1 . IS THE SWITCH IS DONE, THEN WE NEED TO MAKE SURE 
! NO MORE LEVELS OF DEMOTION ARE NEEDED. BOOS QUALIFICATIONS ARE BC.
! THERE ARE NE PEOPLE.
100      continue 
         if(w+1.le.ne) then 
           if(a(w+1).gt.a(w)) w = w + 1 
         endif 
         if(bc.lt.a(w)) then 
            a(b) = a(w)         
! CHECK IF YOU NEED TO DEMOTE B FURTHER                    
            b    = w
            w    = 2 * b 
         else 
! YOU DON'T, SKIP TO THE LAST EMPLOYEE 
            w = ne + 1 
         endif 
         if(w.le.ne) goto 100 
! NO MORE DEMOTION IS NEEDED. PUT OLD BOSS QUALIFICATIONS INTO HIS NEW
! POSITION 
         a(b) = bc 

         goto 200 

         return
         end subroutine 


        end program 

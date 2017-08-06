!
! This is rather an easy problem. 
!
! gfortran -O3 -fdefault-integer-8 172.f95 -o 172 ; time ./172 
! Answer =    227485267000992000
!
!real    0m0.694s
!user    0m0.681s
!sys     0m0.006s
        program Eu172
! We just need to keep track how many digits there are of given type, and we care only for 0,1,2,3
        implicit none
        integer       :: bin_number(0:4**10-1), bin_step(0:4**10-1), arr(0:9), cnt(0:9)
        integer       :: i_digit,i_number,added_digit,added_place
        
        bin_number = 0 
        do i_digit = 1 , 9
           arr          = 0 
           arr(i_digit) = 1 
           bin_number(arr_to_num(arr)) = 1
        enddo 
! bin_number is now set to the first digit distribution.                
        do i_digit = 2 , 18 
        bin_step   = bin_number 
        bin_number = 0
           do i_number = 0,4**10-1
              call num_to_arr(i_number,arr)
              do added_digit = 0,9 
              cnt = arr 
                 if(cnt(added_digit)<3) then   
                    cnt(added_digit) = cnt(added_digit) + 1 
                    added_place = arr_to_num(cnt)
                    bin_number(added_place) = bin_number(added_place) + bin_step(i_number)
                 endif   
              enddo
           enddo 
        enddo   
        
        print *, 'Answer = ' , sum(bin_number)

        return

        contains 

        subroutine num_to_arr(num,arr)
        implicit none
        integer      :: num, arr(0:9)
        integer      :: k, val
        val = num
        do k = 0 , 9 
           arr(k) = mod(val,4)
           val = val/4
        enddo 
        return
        end subroutine 


        pure integer function arr_to_num(arr)
        implicit none 
        integer,intent(in)  :: arr(0:9) 
        integer             :: mul,i
!
        arr_to_num = 0 
        mul = 1 
        do i = 0 , 9 
           arr_to_num = arr_to_num + arr(i) * mul 
           mul = mul * 4 
        enddo 
!
        return
        end 


        end program 

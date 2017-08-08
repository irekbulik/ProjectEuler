! It seems by the given answer that the dices are distinguishable, which
! rather simplifies the problem
! 
!  Had to go quad on ints, but I could be careful with the factorials. 
!
! [0] gfortran -O3 240.f95 -o 240 ; time ./240 
! Answer =  7448717393364181966
!
!real    0m0.021s
!user    0m0.016s
!sys     0m0.004s
        
        program test 
        implicit none 
        integer(kind=16),parameter :: number_of_die_top=10,number_of_die=20,number_of_sides=12,needed_sum=70,&
                             max_reminder=needed_sum/number_of_die_top 
        integer(kind=16)       :: i_1,i_2,i_3,i_4,i_5,i_6,i_7,i_8,i_9,i_10,min_value,max_value
        integer(kind=16)      :: ordered_statistics(number_of_die),denominator,repeat_minimum
        integer(kind=16)      :: factorial_array(0:number_of_die)
        integer(kind=16)      :: reminder_die(max_reminder,number_of_die-number_of_die_top),i_reminder,i_repeated
        integer(kind=16)      :: scaling_of_combination
        integer(kind=16)      :: total,total_step
        
        factorial_array(0) = 1 
        do i_1 = 1 , number_of_die
           factorial_array(i_1) = factorial_array(i_1-1)*i_1
        enddo 
! get the reminder die 
        reminder_die = 0
        do i_1  = 1, max_reminder
           ordered_statistics(1) = i_1
        do i_2  = i_1 , max_reminder
           ordered_statistics(2) = i_2
        do i_3  = i_2 , max_reminder
           ordered_statistics(3) = i_3
        do i_4  = i_3 , max_reminder
           ordered_statistics(4) = i_4
        do i_5  = i_4 , max_reminder
           ordered_statistics(5) = i_5
        do i_6  = i_5 , max_reminder
           ordered_statistics(6) = i_6
        do i_7  = i_6 , max_reminder
           ordered_statistics(7) = i_7
        do i_8  = i_7 , max_reminder
           ordered_statistics(8) = i_8
        do i_9  = i_8 , max_reminder
           ordered_statistics(9) = i_9
        do i_10 = i_9 , max_reminder
           ordered_statistics(10) = i_10
           max_value = i_10
           call get_factors(ordered_statistics,number_of_die-number_of_die_top,denominator,repeat_minimum) 
           reminder_die(max_value,repeat_minimum) = reminder_die(max_value,repeat_minimum) +&
           factorial_array(number_of_die-number_of_die_top-repeat_minimum)/denominator     
        enddo 
        enddo 
        enddo
        enddo
        enddo
        enddo 
        enddo 
        enddo
        enddo
        enddo


        total = 0
        do i_1 = number_of_sides   , 1 , -1 
           ordered_statistics(1) = i_1
        do i_2 = i_1 , 1 , -1 
           ordered_statistics(2) = i_2
        do i_3 = i_2 , 1 , -1 
           ordered_statistics(3) = i_3
        do i_4 = i_3 , 1 , -1 
           ordered_statistics(4) = i_4
        do i_5 = i_4 , 1 , -1 
           ordered_statistics(5) = i_5
        do i_6 = i_5 , 1 , -1 
           ordered_statistics(6) = i_6
        do i_7 = i_6 , 1 , -1 
           ordered_statistics(7) = i_7
        do i_8 = i_7 , 1 , -1 
           ordered_statistics(8) = i_8
        do i_9 = i_8 , 1 , -1 
           ordered_statistics(9) = i_9
           i_10 = needed_sum - i_1 - i_2 -i_3 -i_4 -i_5 -i_6 -i_7 -i_8 -i_9
! We only need ordered statistics 
           if(i_10<=0.or.i_10>i_9)  cycle 
           ordered_statistics(10) = i_10
           min_value = i_10
           total_step = 0 
! Now, we need to now how many of the top dices repeats value and how many are the same as the 
           call get_factors(ordered_statistics,number_of_die_top,denominator,repeat_minimum) 
           scaling_of_combination = factorial_array(number_of_die)/denominator
! Add the ones when the reminder die overlaps is equal to minimum
              do i_repeated = 1 , number_of_die-number_of_die_top
              total_step = total_step + scaling_of_combination/factorial_array(i_repeated+repeat_minimum)/&
                           factorial_array(number_of_die-number_of_die_top-i_repeated)*reminder_die(min_value,i_repeated)
              enddo 
! Add the ones when the reminder die are smaller than the minimum
             do i_reminder = 1 , min_value - 1 
             do i_repeated = 1 , number_of_die-number_of_die_top
              total_step = total_step + scaling_of_combination/(factorial_array(i_repeated)*factorial_array(repeat_minimum))&
                                      * reminder_die(i_reminder,i_repeated)&
                                      / factorial_array(number_of_die-number_of_die_top-i_repeated)
             enddo 
             enddo 
!
        total = total + total_step
!
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

        print *, 'Answer = ' , total

        contains 
        
        subroutine get_factors(ordered_statistics,number_of_die_top,denominator,repeat_minimum)
        implicit none 
        integer(kind=16),intent(in)  :: number_of_die_top,ordered_statistics(number_of_die_top)
        integer(kind=16),intent(out) :: denominator,repeat_minimum                 
        integer(kind=16)             :: i,group,group_value
        repeat_minimum = 1 
        denominator    = 1 
        
        i = number_of_die_top -1 
        if(i.ne.0) then 
        do while(ordered_statistics(i).eq.ordered_statistics(number_of_die_top)) 
           repeat_minimum = repeat_minimum + 1 
           i = i - 1 
        enddo 
        endif 
                
        group       = 1 
        group_value = ordered_statistics(1)  
        do i = 2 , number_of_die_top - repeat_minimum     
           if(ordered_statistics(i).eq.group_value) then 
             group = group + 1 
           else 
             denominator = denominator * factorial_array(group) 
             group       = 1 
             group_value = ordered_statistics(i)    
           endif     
        enddo 
            denominator = denominator * factorial_array(group)
        return
        end subroutine 

        end program

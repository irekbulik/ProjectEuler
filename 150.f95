! Under second in single precision
! [0] gfortran  -O3 150.f95 -o 150 ; time ./150 
! Answer 150 =  -271248680
!
!real    0m0.736s
!user    0m0.728s
!sys     0m0.005s

        program Eu150
        implicit none
        integer  s(500500),t,k,two20,two19
        integer  list(2000),len,ans
        integer  i_level,j_level,k_val,n_levels
!
        t     = 0 
        two19 = 2**19
        two20 = two19*2
! We know there are negative numbers :)
        ans   = 0 
!
        do k = 1 , 500500
           t    = Mod(615949*t+797807,two20)
           s(k) = t-two19
        enddo
        n_levels = 1000
! We start at given level 
        do i_level = 1 , n_levels
!    Test each starting value 
!          Prepare the list for the left most starting value
             len  = n_levels-i_level+1
             do j_level = 1 , len
                list(j_level) = trace(s(1+triag(i_level+j_level-2)),j_level)
             enddo 
             ans = min(ans,min_arr_sum(list,len))
!          Move to the right
           do k_val = 2 , i_level
             do j_level = 1 , len 
                list(j_level) = list(j_level) - s(triag(i_level+j_level-2)+        k_val-1)              &
                                              + s(triag(i_level+j_level-2)+j_level+k_val-1)
             enddo
             ans = min(ans,min_arr_sum(list,len))
           enddo        
        enddo 

        print *, "Answer 150 =", ans




        contains 

        integer function trace(a,n)
        implicit none
        integer, intent(in) :: n, a(n)
           trace = sum(a)
        return
        end 


        integer function triag(n)
        implicit none
        integer, intent(in)  :: n
        triag = n * (n+1)/2
        return
        end         

        integer function min_arr_sum(arr,len)
! We look for smallest sum starting at 1.
        integer :: len,arr(len),suma,j
        min_arr_sum = arr(1)
        suma = arr(1)
        do j = 2, len
           suma = suma + arr(j)
           min_arr_sum = min(min_arr_sum,suma)
        enddo
        return
        end 


        end

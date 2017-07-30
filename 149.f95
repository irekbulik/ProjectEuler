        program Eu149
        implicit none
        integer  :: s(2000*2000),a(2000,2000)
        integer  :: i,j,k,len 
        integer  :: max_val
        
        do i = 1 , 55 
           s(i) = mod(100003-200003*i+300007*i**3,1000000)-500000
        enddo
        do i = 56,2000*2000
           s(i) = mod(s(i-24)+s(i-55)+1000000,1000000)-500000
        enddo 

        k = 1 
        do j = 1 , 2000
        do i = 1 , 2000
           a(i,j) = s(k)
           k = k + 1 
        enddo
        enddo
        max_val = -1
! Loop rows 
        do i = 1 , 2000
           len = 2000 
           do k = 1 , len
              s(k) = a(k,i) 
           enddo 
           max_val = max(max_val,max_arr_sum(s,len))
        enddo 
! Loop columns 
        do i = 1 , 2000
           len = 2000 
           do k = 1 , len
              s(k) = a(i,k) 
           enddo 
           max_val = max(max_val,max_arr_sum(s,len))
        enddo 
! Loop diagonals: starting from the top row 
        do i = 1 , 2000
           len = 2000-i+1 
           do k = 1 , len 
              s(k) = a(k,k+i-1)
           enddo 
           max_val = max(max_val,max_arr_sum(s,len))
        enddo 
! Loop diagonals: starting from the first columm
        do i = 1 , 2000
           len = 2000-i+1 
           do k = 1 , len 
              s(k) = a(k+i-1,k)
           enddo 
           max_val = max(max_val,max_arr_sum(s,len))
        enddo 
! Loop antidiagonal Starting from top row 
        do i = 1 , 2000
           len = i 
           do k = 1,len
              s(k) = a(k,i-k+1)
           enddo 
           max_val = max(max_val,max_arr_sum(s,len))
        enddo 
! Loop antidiagonal starting from last column 
        do i = 1 , 2000
           len = 2000 - i + 1 
           do k = 1, len
              s(k) = a(i+k-1,2000-k+1)
           enddo 
           max_val = max(max_val,max_arr_sum(s,len))
        enddo 


        print *, 'answer 149 = ' , max_val
        
        contains 

        integer function max_arr_sum(arr,len)
        implicit none
        integer    :: len, arr(len)
        integer    :: suma,min_sum,j
        suma        = arr(1)
        max_arr_sum = arr(1)
        min_sum     = arr(1)
        do j = 1 , len
         suma = suma + arr(j)
         if(suma-min_sum>max_arr_sum) max_arr_sum = suma-min_sum
         if(suma<min_sum) min_sum = suma
        enddo
        return
        end 

        end 

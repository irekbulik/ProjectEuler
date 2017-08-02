!answer      7130034
!
!real    0m56.421s
!user    0m56.107s
!sys     0m0.053s
        program Eu166   
        implicit none
        integer             :: population(0:36), values(0:36,10000) 
        integer             :: suma
        integer             :: d1,d2,d3,d4
        integer             :: dc1(4),dc2(4),dc3(4),dc4(4)
        integer             :: possible_sum    
        integer             :: possible_d1(0:10),possible_d2(0:99),possible_d3(0:999),possible_d4(0:9999)
        integer             :: p1d,p2d,p3d,l2d,l3d
        integer,parameter   :: len_p2=10**7
        integer             :: possible_pair(2,len_p2)
        integer             :: has_p2,i_p,k,i_value,val,total,local_total
        logical             :: correct

        population = 0 

        do d1 = 0, 9 
        do d2 = 0, 9 
        do d3 = 0, 9 
        do d4 = 0, 9 
           suma = d1+d2+d3+d4
           population(suma) = population(suma)+1
           values(suma,population(suma)) = d1*1000+d2*100+d3*10+d4
        enddo        
        enddo        
        enddo        
        enddo        
        
        total = 0 
        do possible_sum = 0,18 
        local_total = 0 
!
!
           possible_d1 = 0   
           possible_d2 = 0   
           possible_d3 = 0   
           possible_d4 = 0
!

           do i_value = 1 , population(possible_sum)
           d1 = digit(values(possible_sum,i_value),1)
           d2 = digit(values(possible_sum,i_value),2)
           d3 = digit(values(possible_sum,i_value),3)
           d4 = digit(values(possible_sum,i_value),4)
              p1d =              d1
              p2d =        d1*10+d2
              p3d = d1*100+d2*10+d3
              possible_d1(p1d)             = 1     
              possible_d2(p2d)             = 1     
              possible_d3(p3d)             = 1     
              possible_d4(values(possible_sum,i_value)) = 1 
           enddo
! Compute possible pairs                
           has_p2 = 0 
            do i_p = 1 , population(possible_sum)
                do k = 1 , 4 
                 dc1(k) = digit(values(possible_sum,i_p),k)
                enddo 
            do i_value = 1 , population(possible_sum)
                do k = 1 , 4 
                 dc2(k) = digit(values(possible_sum,i_value),k)
                enddo 
                correct = .true.
                do k = 1 , 4 
                   val = dc1(k)*10+dc2(k)
                   correct = correct.and.possible_d2(val).eq.1
                enddo
                val = dc1(1)*10+dc2(2)
                correct = correct.and.possible_d2(val).eq.1
              if(correct) then 
                 has_p2 = has_p2 + 1 
                   if(has_p2.gt.len_p2) then 
                      print *, 'len_p2 too small',len_p2,possible_sum
                      stop
                   endif 
                  possible_pair(1,has_p2) = values(possible_sum,i_p)
                  possible_pair(2,has_p2) = values(possible_sum,i_value)                  
              endif 
            enddo
            enddo 
! Compute possible triples and then finish.
            do i_p = 1 , has_p2
                do k = 1 , 4 
                 dc1(k) = digit(possible_pair(1,i_p),k)
                 dc2(k) = digit(possible_pair(2,i_p),k)
                enddo 
            do i_value = 1 , population(possible_sum)
                do k = 1 , 4 
                 dc3(k) = digit(values(possible_sum,i_value),k)
                enddo 
                correct = .true.
                do k = 1 , 4 
                   val = dc1(k)*100+dc2(k)*10+dc3(k)
                   correct = correct.and.possible_d3(val).eq.1
                enddo
                val = dc1(1)*100+dc2(2)*10+dc3(3)
                correct = correct.and.possible_d3(val).eq.1
              if(correct) then 

                do k = 1 , 4 
                dc4(k) = possible_sum - dc1(k) - dc2(k) - dc3(k)
                correct = correct.and.dc4(k).ge.0.and.dc4(k).le.9
                enddo 
                   val = dc1(1)*1000+dc2(2)*100+dc3(3)*10+dc4(4)
                   correct = correct.and.possible_d4(val).eq.1                      
                   val = dc4(1)*1000+dc3(2)*100+dc2(3)*10+dc1(4)
                   correct = correct.and.possible_d4(val).eq.1                      
                   if(correct) local_total = local_total + 1 

              endif 
            enddo
            enddo 
                
                             total = total + local_total 
        if(possible_sum/=18) total = total + local_total

        enddo 

        print *, 'answer ', total




        contains
        pure integer function digit(val,n) 
        integer, intent(in) :: val, n 
        digit = mod(val,10**(4-n+1))/10**(4-n)
        return
        end 

        end 

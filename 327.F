!answer 327 =  34315549139516
!
!real    0m0.003s
!user    0m0.001s
!sys     0m0.002s
        program test
        integer suma,i
        suma = 0 
         do i = 3 , 40
            suma = suma + m(i,30)
         enddo
           write(*,1800) suma
1800    format('answer 327 =',I16)

        return
        end 


        integer function m(c,r)
        implicit none
        integer c,r
        integer ma(50)
        integer i 
        integer mt,rt 

        if(r.gt.50) then
          print *, 'change alloc'       
          stop
        endif 
        if(c.lt.2) then 
          print *, 'c too small'
          stop
        endif 
        
        if(r.lt.c) then 
          m = r + 1 
        else
          ma(c) = c + 3 
          if(r.eq.c) then       
             m = ma(r) 
             return
          endif     
! COMPUTE HOW MANY CARDS WE NEED TO STORE AND HOW WE CAN PROCESS THEM 
          do 100 i = c + 1 , r          
          mt = (ma(i-1)-c+1)/(c-2) 
          rt = (ma(i-1)-c+1) - (c-2)*mt 
          ma(i) = (mt +1 )* c        
          if(rt.gt.0) ma(i) = ma(i) + 2 + rt 
100       continue 
          m = ma(r)
        endif 
        return
        end 
         
         



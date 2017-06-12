! Well, I bruteforced that one. I solved 105 first and the code was ready so lamest loop while cooking dinner.
! Turns out, one minute is enough, but I could make it smarter
!
! answer 105 = 255  20313839404245
!
!real    1m1.749s
!user    1m1.505s
!sys     0m0.045s
!
!
        program Eu103
        implicit none
        integer   :: sets(7),min_set(7)
        integer   :: min_set_sum,i7,i6,i5,i4,i3,i2,i1
        logical   :: good_set 

        min_set_sum = 10**8
        min_set = 0 
        do i7 = 50 , 1 , - 1
        sets(7) = i7
        do i6 =  i7-1,1,-1
        sets(6) = i6
        do i5 =  i6-1,1,-1
        sets(5) = i5
        do i4 =  i5-1,1,-1
        sets(4) = i4
        do i3 =  i4-1,1,-1
        sets(3) = i3
        do i2 =  i3-1,1,-1
        sets(2) = i2
        if(2*i2.lt.i7) cycle
        do i1 =  i2-1,1,-1
        sets(1) = i1

           if(good_set(7,sets)) then 
              if(sum(sets).lt.min_set_sum) then 
              min_set_sum = sum(sets)
              min_set = sets
              endif 
           endif 
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo

              write(*,1800), min_set_sum, min_set(:)

1800    format('answer 105 = ',i3,2x,7(i2))
        return
        end 


        function good_set(n,set)
        implicit none
        integer             :: n, set(*)
        logical             :: good_set 
        integer,allocatable :: set_sum(:),set_count(:),set_key(:)
        integer             :: i,ib
        
        allocate(set_sum(2**n-1  ))
        allocate(set_count(2**n-1))
        allocate(set_key(2**n-1  ))
        set_sum = 0 
        set_count = 0     

        do i = 1 , 2**n-1
           do ib = 1 , n 
              if(btest(i,ib-1)) then
                set_count(i) = set_count(i) + 1
                set_sum(i)   = set_sum(i) + set(ib)       
              endif 
           enddo
        enddo 

        call sort(set_sum,set_key,2**n-1)

        good_set = .true.
        do i = 2 , 2**n-1 
           if(set_sum(set_key(i)).eq.set_sum(set_key(i-1))) then 
! SUM IS NOT UNIQUE
              good_set = .false.
              exit 
           endif          
           if(set_count(set_key(i)).lt.set_count(set_key(i-1))) then 
! SMALLER SET HAS LARGER SUM
              good_set = .false.
              exit 
           endif          
        enddo 

        deallocate(set_sum) 
        deallocate(set_count)
        deallocate(set_key)


        return
        end 



! SORTING WITH THE KEY 
        subroutine sort(a,map,n)
        implicit none
        integer , intent (in)   ::  n
        integer , intent (in)   ::  a(*)
        integer , intent (inout)::  map(*) 
        integer                 ::  ne,nb,bc,w,b
        if(n.eq.1) then 
          map(1) = 1 
          return
        endif 
        do ne = 1 , n 
           map(ne) = ne 
        enddo 
!
        ne = n
        nb = n/2+1
200     continue         
        if(nb.gt.1) then 
           nb = nb - 1 
           bc = map(nb)
        else 
           bc    = map(ne)
           map(ne) = map(1) 
           ne    = ne - 1 
             if(ne.eq.1) then 
                map(1) = bc
                return
             endif 
!
         endif 
        b = nb 
        w = 2 * b 
100      continue 
         if(w+1.le.ne) then 
           if(a(map(w+1)).gt.a(map(w))) w = w + 1 
         endif 
         if(a(bc).lt.a(map(w))) then 
            map(b) = map(w)
            b    = w
            w    = 2 * b 
         else 
            w = ne + 1 
         endif 
         if(w.le.ne) goto 100 
         map(b) = bc

         goto 200 

         return
         end 

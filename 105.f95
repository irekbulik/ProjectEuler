! answer 105       73702
!
!real    0m0.022s
!user    0m0.019s
!sys     0m0.003s
!
! THAT IS ONE EASY PROBLEM.
!
        program Eu105
        implicit none
        integer   :: sets(0:14,100)
        integer   :: ans , i 
        logical   :: good_set 

        call read_sets(sets)
        ans = 0 
        do i =  1 , 100
           if(good_set(sets(0,i),sets(1,i))) ans = ans + sum(sets(1:sets(0,i),i))
        enddo 

        print *, 'answer 105' , ans 

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



        subroutine read_sets(sets)
        implicit none
        integer   ::    sets(0:14,100),i,k,l,length
        character ::    line*100
        open(file='p105_sets.txt',status='old',unit=11)
        do i = 1 , 100 
           read(11,'(A)') line
           length = len_trim(line)
           k = length 
           l = 1 
             do while(k.gt.0) 
                if(line(k:k).eq.',') then
                  l = l + 1 
                  line(k:k) = ' '
                endif 
                  k = k - 1 
             enddo 
             read(line,*) sets(1:l,i) 
                          sets(0  ,i) = l       
        enddo
        return
        end subroutine
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



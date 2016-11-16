! [0]  gfortran -O3 068.F -o 086 ; time ./086  | tr -d ' ' | sort -n 
!2594936378711015
!2951051817673439
!6357528249411013
!6531031914842725
!11069627285843410
!11085864693972710
!16103104548782926
!18102107379496568
!21049436378715110
!24105101817673934
!27858434106101917
!28797161103104548
!
!real    0m0.004s
!user    0m0.001s
!sys     0m0.003s

#define MAX_ARR 1000
        program Eu68
        implicit none
        integer  n1(10,MAX_ARR)
        integer  n2(10,MAX_ARR)
        integer  i,j,k,l
        integer  c,d,ic,id
        logical  taken
        integer  TAR 


        do 100 TAR = 6 , 30

! FILL THE FIRST NODE 
        call iclear(10*MAX_ARR,n1) 
        call iclear(10*MAX_ARR,n2) 
        
        c = 0 
        do i = 1 , 6 
           do j = 1 , 10 
              if(j.eq.i) cycle 
           do k = 1 , 10 
              if(k.eq.i) cycle 
              if(k.eq.j) cycle
              if(i+k+j.eq.TAR) then
                c = c + 1 
                if(c.gt.MAX_ARR) then 
                print *, 'first',c
                stop
                endif 
                n1(1,c) = i
                n1(2,c) = j
                n1(3,c) = k 
              endif   
            enddo
            enddo
        enddo 
! FILL THE SECOND ONE. 
        d = 0 
        do ic = 1 , c 
          do i = 1 , 10 
             if(i.le.n1(1,ic)) cycle
             if(taken(n1(1,ic),i)) cycle 
          do j = 1 , 10 
             if(j.eq.i) cycle 
             if(taken(n1(1,ic),j)) cycle 
             if(n1(3,ic)+i+j.eq.TAR) then
                d = d + 1 
                if(d.gt.MAX_ARR) then 
                 print *, 'second',d
                stop
                endif 
                do l = 1 , 3 
                 n2(l,d) = n1(l,ic) 
                enddo
                 n2(4,d) = i 
                 n2(5,d) = j 
             endif  
          enddo
          enddo
        enddo 
! FILL THE THIRD ONE. 
        c = 0 
        do id = 1 , d
          do i = 1 , 10 
             if(i.le.n2(1,id)) cycle
             if(taken(n2(1,id),i)) cycle 
          do j = 1 , 10 
             if(j.eq.i) cycle 
             if(taken(n2(1,id),j)) cycle 
             if(n2(5,id)+i+j.eq.TAR) then
                c = c + 1 
                if(c.gt.MAX_ARR) then 
                 print *, 'third',c
                stop
                endif 
                do l = 1 , 5
                 n1(l,c) = n2(l,id) 
                enddo
                 n1(6,c) = i 
                 n1(7,c) = j 
             endif  
          enddo
          enddo
        enddo  
! FILL THE FOURTH ONE. 
        d = 0 
        do ic = 1 , c 
          do i = 1 , 10 
             if(i.le.n1(1,ic)) cycle
             if(taken(n1(1,ic),i)) cycle 
          do j = 1 , 10 
             if(j.eq.i) cycle 
             if(taken(n1(1,ic),j)) cycle 
             if(n1(7,ic)+i+j.eq.TAR) then
                if(d.gt.MAX_ARR) then
                 print *, 'fourth', d
                 stop
                endif 
                d = d + 1 
                do l = 1 , 7
                 n2(l,d) = n1(l,ic) 
                enddo
                 n2(8,d) = i 
                 n2(9,d) = j 
             endif  
          enddo
          enddo
        enddo 
! FILL THE LAST ONE. 
        c = 0 
        do id = 1 , d
          do i = 1 , 10 
             if(i.le.n2(1,id)) cycle
             if(taken(n2(1,id),i)) cycle 
             if(n2(9,id)+n2(2,id)+i.eq.TAR) then
                c = c + 1 
                if(c.gt.MAX_ARR) then
                 print *, 'fith', c
                 stop
                endif 
                do l = 1 , 9
                 n1(l,c) = n2(l,id) 
                enddo
                 n1(10,c) = i 
             endif  
          enddo
        enddo  
        
        do ic = 1 ,c 
         write(*,1800) n1(1,ic),n1(2,ic),n1(3,ic), 
     $                 n1(4,ic),n1(3,ic),n1(5,ic),
     $                 n1(6,ic),n1(5,ic),n1(7,ic),
     $                 n1(8,ic),n1(7,ic),n1(9,ic),
     $                 n1(10,ic),n1(9,ic),n1(2,ic)
        enddo        

100     continue 

1800    format(15(i2,1x))

        return
        end 
        
        logical function taken(n,i)
        implicit none
        integer  i,n(*),k
        
         taken = .false.
         do k = 1, 10 
           if(n(k).eq.i) taken = .true.
           if(taken) goto 100
         enddo
100      continue
         return
         end 


        subroutine iclear(n,a)
        implicit none
        integer  n,i,a(*)
          do i = 1 , n 
             a(i) = 0
          enddo 
        return
        end 
      


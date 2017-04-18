#define NSQ 30000
#define NCU 3000
! Rather lame brute force 
! [0] gfortran -fdefault-integer-8 -O3  348.F -o 348 ; time ./348
!              5229225
!             37088073
!             56200265
!            108909801
!            796767697
!
!real    0m22.344s
!user    0m22.052s
!sys     0m0.103s

        program Eu348
        implicit none
        integer  vals(NSQ,NCU) 
        
        integer i,j

        do j = 1 , NCU
        do i = 1 , NSQ
           vals(i,j) = i**2+j**3
        enddo
        enddo  
        
        call sort(vals,NCU*NSQ)
        call rep(vals,NCU*NSQ)

        return
        end 

        subroutine rep(vals,n)
        implicit none 
        integer    n,vals(*)
        integer    i,c
        logical    ispal

        c = 1 
        do i = 2, n 
           if(vals(i).eq.vals(i-1)) then 
             c = c + 1    
             if(c.eq.4) then 

               if(i.eq.n.and.ispal(vals(i))) then
                 print *, vals(i)
               elseif(vals(i+1).ne.vals(i).and.ispal(vals(i))) then
                 print *, vals(i)
               endif

             endif 

           else
             c = 1 
           endif 

        enddo

        return
        end 

        subroutine sort(a,n)
        implicit none
        integer  a(*),n
        integer  ne,nb,bc,w,b
        if(n.lt.2) return
! HIRING PART. WE START WITH N/2+1 HIRED PEOPLE AND WE EMPLOY POTENTIAL 
! N/2 BOSSED
        ne = n
        nb = n/2+1
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
         end 


        logical function ispal(nn)
        implicit none
        integer  nn,n
        integer  ndig 
        integer  digs(100)
        integer  i 
        
        
        n = nn 
        ndig = 1 + floor((log10(real(n))))
        
        do i = ndig , 1 , -1 
           digs(i) = n/10**(i-1)
           n = n - digs(i)*10**(i-1)
        enddo 
        
        ispal = .true.
        do i = 1 , ndig
         ispal = ispal.and.digs(i).eq.digs(ndig-i+1)
        enddo
        return
        end 


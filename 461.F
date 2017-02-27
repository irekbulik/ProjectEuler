![0] gfortran -fdefault-real-8 -fdefault-integer-8 -O4 -funroll-loops -ffast-math 461.F -o 461 ; time ./461 
!Answer 461       159820276
!
!real    0m57.046s
!user    0m56.107s
!sys     0m0.155s
!-- small improvemnt to help with sort
! [0] gfortran -fdefault-real-8 -fdefault-integer-8 -O4 -funroll-loops -ffast-math 461.F -o 461 ; time ./461 
!Answer 461       159820276
!
!real    0m38.106s
!user    0m37.680s
!sys     0m0.079s

        program Eu461
        implicit none 
        real*8   vals(14500*14501/2)
        real*8   f
        real*8   pi
        integer  n,lim,tot
        integer  ist,ien,en
        integer  gbs,gbe,be
        real*8   b,gb
        integer  nb 
        real*8   v0,ve
        integer  ie0,ie1
        
        n   = 10000
        lim = 14500
        pi   = 4.0d0*ATan(1.0d0)
!
        call fillv(vals,n,lim,tot)        
!
        b   = 100d0
        gb  = 100d0
!
        en  = tot  
!
        do 10 ist = 1 , tot 
           ien = en 
! WELL, I SHOULD BE OVERSHOOTING
! SET THE LOCAL B: 
           b = 100d0
20         continue
! THE ERROR HERE MUST BE GOING DOWN HILL. IT CAN BE LOWER THAN THE
! GLOBAL ERROR, BUT DOES NOT NEED TO BE 
           if(abs(vals(ist)+vals(ien)-pi).lt.b) then
               be = ien
               b  = abs(vals(ist)+vals(ien)-pi)
               if(b.lt.gb) then 
               gbs = ist
               gbe = be 
               gb  = b 
               endif
! WE MUST ALWAYS IMPROVE LOCALLY
            ien = ien - 1 
            goto 20 
            endif   
! NOW, WE DETERMINE WHETHER WE NEED TO GO BACK A LITTLE TO MAKE SURE WE
! OVERSHOOT AGAIN
           en = be 
           do 30 ien = be, tot          
! next one must be greater than then the error we had 
              if(vals(ist+1)+vals(ien).gt.pi+b) then
! IT IS POSSIBLE THAT STEPPING TO LARGER VALUE GIVES MY BETTER ANSWER.
                 en = be
                 goto 40
              endif 
30         continue     
40      continue 
        if(en.lt.ist) goto 50    
10      continue  
50      continue 
        call look(vals(gbe),vals(gbs),n,lim)
        end 

        subroutine look(en,st,n,l) 
        implicit none
        integer  n,l
        integer  ba,bb,bc,bd
        integer  a,b
        real*8   en,st
        real*8   f
        real*8   bs,be
        f(l,n) = exp(real(l)/real(n))-real(1)

        bs  = 100.0d0
        be  = 100.0d0
        do 10 a = 1 , l 
        do 10 b = a , l 
           if(abs(f(a,n)+f(b,n)-st).lt.bs) then
                 bs = abs(f(a,n)+f(b,n)-st)
                 ba = a
                 bb = b 
           endif 
           if(abs(f(a,n)+f(b,n)-en).lt.be) then
                 be = abs(f(a,n)+f(b,n)-en)
                 bc = a
                 bd = b 
           endif 
10      continue
        write(*,1800) ba**2+bb**2+bc**2+bd**2
1800    format('Answer 461 ',I15)
        return
        end 

        subroutine fillv(v,n,l,i)
        implicit none
        real*8   v(*)
        integer  n,l,a,b,i
        real*8   f,pi
        f(a,n) = exp(real(a)/real(n))-real(1)
        pi   = 4.0d0*ATan(1.0d0)

        i = 0 
        do 10 a = 1 , l 
        do 20 b = a , l 
        i = i + 1 
        v(i) = f(a,n)+f(b,n)
        if(v(i).gt.pi) goto 10 
20      continue 
10      continue 
        call sort(v,i)
        do 30 a = i , 1 , -1 
         if(v(i).lt.pi) then 
         i = i + 1 
         return
        endif 
30      continue

        return
        end 

        subroutine sort(a,n)
        implicit none
        real*8   a(*),bc
        integer  n 
        integer  ne,nb,w,b

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

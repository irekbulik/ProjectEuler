! BRUTE-FORCED IT A BIT. NOT PROUD
!
! gfortran -Ofast 
!
! Euler 572 =  19737656
!
!real    4m57.297s
!user    4m56.805s
!sys     0m0.002s
!
!
#define MAX_DIVISORS 1000
#define MAX_LOOP     200
        program eu572 
        implicit none 
        integer  a,b,c,d,e,f,g,h,i,r
        integer  f1,f2,f3
        integer  c1,c2,c3
        integer  ffh,fbd,fcg
        integer  fh(2,MAX_DIVISORS)
        integer  bd(2,MAX_DIVISORS)
        integer  cg(2,MAX_DIVISORS)
        integer  n1,n2,n3,i1,i2,i3,s1,s2,s3
        integer  total
        integer  loctot
        
!
! WE KNOW THE ANSWER FOR RANK = 0 AND RANK = 3, SO WE JUST NEED TO CHECK
! RANKS 1 AND 2 
!
        total = 2 ! RANK 3 AND 0 
!
        do 100 a = - MAX_LOOP, MAX_LOOP
        do 100 e = - MAX_LOOP, MAX_LOOP 
! LOOP OVER RANKS 
        do 100 r = 1 , 2 
           i = r - a - e 
           if(i.lt.-MAX_LOOP.or.i.gt.MAX_LOOP) goto 100
         
        f1 =  a * ( - a + 1 )
        f2 =  e * ( - e + 1 )
        f3 =  i * ( - i + 1 )
!
! F2 - F1 + F3 = 2 * FH 
! F1 - F3 + F2 = 2 * BD
! F3 - F2 + F1 = 2 * CG 

        ffh = f2 - f1 + f3
        fbd = f1 - f3 + f2
        fcg = f3 - f2 + f1 

        if(mod(ffh,2).ne.0.or.mod(fbd,2).ne.0.or.mod(fcg,2).ne.0) 
     $  goto 100
!
        ffh = ffh/2
        fbd = fbd/2
        fcg = fcg/2
!
! APPLY SECOND SCREENING 
!
        c1 = a + e - 1 
        c2 = a + i - 1 
        c3 = e + i - 1 
!
        if(c1*fbd.ne.c2*fcg.or.c2*fcg.ne.c3*ffh.or.c1*fbd.ne.c3*ffh)
     $  goto 100

        call getdiv(fbd,n1,bd)
        call getdiv(fcg,n2,cg)
        call getdiv(ffh,n3,fh)
        
        do 200 i1 = 1 , n1
!
           b = bd(1,i1)
           d = bd(2,i1)
!
        do 200 i2 = 1 , n2 

           c = cg(1,i2)
           g = cg(2,i2)

        do 200 i3 = 1 , n3

           f = fh(1,i3)
           h = fh(2,i3)

        call versol(a,e,i,f,h,b,d,c,g,total)



200     continue 
100     continue 
                
         write(*,1800) total

1800    format('Euler 572 =',I10)

        return
        end 
        


        subroutine getdiv(val,nd,in)
        implicit none
        integer  in(2,*),nd,val
        integer  i1,i2
        integer  idiv
!
        nd = 0 

        do i1 = -MAX_LOOP,MAX_LOOP
        do i2 = -MAX_LOOP,MAX_LOOP
          if(i1*i2.eq.val) then
          nd = nd + 1 
           if(nd.gt.MAX_DIVISORS) then 
            print *, 'error'    
            stop
           endif 
          in(1,nd) = i1
          in(2,nd) = i2 
          endif 
        enddo
        enddo
        return

        return
        end 

        subroutine versol(a,e,i,f,h,b,d,c,g,total)
        integer total
        logical veridp
        integer a,b,c,d,e,f,g,h,i
!
        veridp =            (a*(a-1) + b*d + c*g      .eq.0) !11
        veridp = veridp.and.(e*(e-1) + b*d       + f*h.eq.0) !22
        veridp = veridp.and.(i*(i-1)       + c*g + f*h.eq.0) !33
!
        veridp = veridp.and.(b*(a+e-1)+c*h.eq.0) ! 12
        veridp = veridp.and.(d*(a+e-1)+f*g.eq.0) ! 21 
!        
        veridp = veridp.and.(g*(a+i-1)+d*h.eq.0) ! 31
        veridp = veridp.and.(c*(a+i-1)+b*f.eq.0) ! 13

        veridp = veridp.and.(h*(e+i-1)+b*g.eq.0) ! 32
        veridp = veridp.and.(f*(e+i-1)+c*d.eq.0) ! 23

        if(veridp) total = total + 1 

        return
        end 

         
        

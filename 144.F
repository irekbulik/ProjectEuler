! Answer    354
!
!real    0m0.003s
!user    0m0.001s
!sys     0m0.002s
!
        program Eu144
        implicit none 
        real*8   x0,y0,a0,b0
        real*8   x1,y1,a1,b1
        integer  k , ref
! THIS IS THE POINT WHERE WE ACTUALLY CROSS THE PARABOLA
        x0 =  7.10731694996586272D-003
        y0 =   9.9999898972040508d0

        a0 = -14.0714285714286d0
        b0 = 10.1d0
!
        ref = 0 
!
10     ref = ref + 1   
          call reflect(x0,y0,a0,b0,x1,y1,a1,b1)
!          print *, ref, x1,y1
          x0 = x1
          y0 = y1
          a0 = a1 
          b0 = b1 
! CHECK IF WE EXIT 
          if(abs(x0).le.0.01d0.and.y0.gt.0.0d0) goto 20
          goto 10 
! THE LAST REFLECTION IS OUTSIDE THE ELLIPSE ! 
20      write(*,1800)  ref-1 
1800    format('Answer  'I5)

        return
        end 


        subroutine reflect(x0,y0,a0,b0,x1,y1,a1,b1)
! GIVEN SLOPE OF RAY AND ITS DIRECTION AND START POINT, DETERMINES 
! THE SLOPE, REFLECTION POINT AND DIRECTION INFORMATION, RETURNS THE
! SAME DATA AFTER REFLECTION. 
        real*8   x0,y0,a0,b0
        real*8   x1,y1,a1,b1
        real*8   m
! SOLVE FOR NEW REFLECTION POINT
        call solve(a0,b0,x0,y0,x1,y1)
! SOLVE FOR NEW SLOPE 
        m = -4.0d0*x1/y1
! THE PREPENDICULAR SLOPE IS 
        m = -1.0d0/m
! FIND THE NEW SLOPE 
        call fndslp(m,x0,y0,a0,b0,x1,y1,a1,b1)
        return
        end


        subroutine fndslp(m,x0,y0,a0,b0,x1,y1,a1,b1)
        implicit none
        real*8   m,x0,y0,x1,y1,a0,a1,b0,b1
        real*8   n,x,y,t,s,z,xp1,xp2,yp1,yp2,nrm1,nrm2
        real*8   ang,ang1,ang2
        real*8   v1(2),v2(2)
!
! SOLVE FOR B OF PERPENDICULAR 
!
        v1(1) = x0-x1
        v1(2) = y0-y1

        n = y1-m*x1
        if(x1.gt.0.0d0) then 
        x = x1-0.1d0 
        else 
        x = x1+0.1d0 
        endif 
        y = m*x+n
        v2(1) = x-x1
        v2(2) = y-y1
!        
        nrm1 = sqrt(v1(1)*v1(1)+v1(2)*v1(2))
        nrm2 = sqrt(v2(1)*v2(1)+v2(2)*v2(2))
!
        v1(1) = v1(1)/nrm1
        v1(2) = v1(2)/nrm1
!
        v2(1) = v2(1)/nrm2
        v2(2) = v2(2)/nrm2
! JUST ONE SOLUTION SHOULD GIVE ME VECTOR INSIDE THE ELLIPSE 
        ang = acos(v1(1)*v2(1)+v1(2)*v2(2))
        t = cos(2*ang) 
        x = v1(1)
        y = v1(2)
!
        s = x*x*(x*x+y*y-t*t)

        s = sqrt(s)

        xp1 = (t*x*x-y*s)/(x*(x*x+y*y))
        yp1 = (t*y  +  s)/(  (x*x+y*y))

        xp2 = (t*x*x+y*s)/(x*(x*x+y*y))
        yp2 = (t*y  -  s)/(  (x*x+y*y))
!
! CHOOSE WHICH ONE WE WANT.
!
        ang1 = acos(xp1*v2(1)+yp1*v2(2))
        ang2 = acos(xp2*v2(1)+yp2*v2(2))
 
        if    (abs(ang1-ang).lt.1.0d-10) then 
          a1 = yp1/xp1    
        elseif(abs(ang2-ang).lt.1.0d-10) then       
          a1 = yp2/xp2
        else 
          print*,"error"
          stop
        endif 
        b1 = y1 - a1*x1
        return
        end 



        subroutine solve(a,b,xs,ys,xe,ye)
        implicit none 
        real*8   a,b,xs,ys,xe,ye
        real*8   x1,y1,x2,y2,t
        real*8   tol
        data     tol/1.0d-10/
        
        t  = sqrt(25.0d0*(4.0d0+a*a)-b*b)
        y1 = (4.0d0*b-2.0d0*a*t)/(4.0d0+a*a)
        x1 = -(a*b+2.0d0*t)/(4.0d0+a*a)

        y2 = (4.0d0*b+2.0d0*a*t)/(4.0d0+a*a)
        x2 = (-a*b+2.0d0*t)/(4.0d0+a*a)
! ONE OF THE POINTS IS THE START. 
        if    (abs(x1-xs)+abs(y1-ys).lt.tol) then
              xe=x2
              ye=y2
        elseif(abs(x2-xs)+abs(y2-ys).lt.tol) then
              xe=x1
              ye=y1
        else 
           print *, 'fail in solve'
           stop
        endif 
        return
        end 















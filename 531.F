#define __MAX__  1005000
! [0] gfortra gfortran -O4 -fdefault-integer-8 -ffast-math 531.F -o 531.x ; time ./531.x 
!Answer 531 4515432351156203105
!
!real    0m2.600s
!user    0m2.589s
!sys     0m0.005s

         program Eu531 
         implicit none
         integer  phi(__MAX__)
         integer  maxi,n,m,crtgen,tot
         
 
         tot  = 0 
         maxi = __MAX__
         call sivphi(maxi,phi)
         do 10 n = 1000000,1005000-1
         do 10 m = n+1,1005000-1
           tot = tot + crtgen(phi(n),phi(m),n,m)
10       continue 
         write(*,1800) tot 
1800     format('Answer 531 ',I19)

         return
         end 


         integer function crtgen(ia,ib,in,im)  
! CRT:
! x  =  a  mod(n) 
! x  =  b  mod(m) 
! d  =  gcd(m,n)   n' = n/d m' = m/d
! 
!   we have : x = a + t * n 
!             x = b + s * m 
! 
!             0 = (b-a) + d * (m's-n't) : d | (b-a)
!
!  we find numbers  p q : p * n   +   q * m  = d 
!                         p * n'  +   q * m' = 1 
!  
!             b = a + i * d   
!             
!             x - a = 0      mod n   ! d | x - a  
!             x - a = i * d  mod m   
! 
!         z =  (x-a)/d = 0 mod n'
!         z =  (x-a)/d = i mod m'
! 
! NOW, WE CAN SOLVE CRT FOR Z 
!
!         We know that (m',n') = 1 and p n' + q m' = 1     
!         p n' = 1 mod m' 
!         q m' = 1 mod n'     
!                       
!         z = p n'i mod(m'n')  satisfies z = 0 mod n' and i mod m'
!
!         (x-a)/d = p  n' i mod(m'n')
!          x = a + p n' d i = a + p n i  mod(n'm'd)
!
!          i = (b-a)/d 
!     
!          x = a + p n (b-a)/d       mod (n*m')
! 
         implicit none 
         integer ia,ib,a,b,in,im,n,m
         integer x,y,gcd,lcm
         integer crtsol 
!
       n = in
       m = im
       a = ia
       b = ib
       call gcdabi(n,m,x,y,gcd) 
! CHECK IF THE SOLUTION EXISTS 
       if(mod(b-a,gcd).ne.0) then 
         crtgen = 0 
         return
       endif 
       lcm    =  n*m/gcd
       crtgen =  (b-a)/gcd 
       crtgen =  mod(crtgen*n*x+a,lcm)
       if(crtgen.lt.0) then 
          crtgen = crtgen + (int(-crtgen/lcm)+1)*lcm
       endif 
!
       if(mod(crtgen,n).ne.a.and.mod(crtgen,m).ne.b) then
        print *, crtgen,a,in,b,im
        stop('fail')
       endif 

       return
       end  

!
! extended euclid algorithm that solves the problem
!
!  a . x + b . y = gcd(a,b) 
!         
        subroutine gcdabi(a,b,x,y,gcd)
        implicit none
        integer  a,b,x,y,gcd
        integer  r0,r1,r2,s0,s1,s2,t0,t1,t2
        integer  q 

        r0 = a 
        r1 = b
        if(a.lt.b) then 
         r0 = b 
         r1 = a 
        endif 
        s0 = 1 
        s1 = 0
        t0 = 0
        t1 = 1 

100     continue 
!         
         q  = r0/r1
!
         r2 = r0 - q * r1 
         s2 = s0 - q * s1
         t2 = t0 - q * t1 
         
         if(r2.eq.0) goto 200
         r0 = r1
         s0 = s1
         t0 = t1 
        
         r1 = r2
         s1 = s2
         t1 = t2 

         goto 100 

200      continue 

        gcd = r1 
        x   = s1
        y   = t1 
        
          if(a.lt.b) then
          x = t1
          y = s1 
          endif 

        return
        end 
        subroutine sivphi(n,phi)
        implicit none
        integer  n,phi(*)
        integer  j,k
        do 10 j = 1 , n 
10        phi(j) = j 
        do 20 j = 2 , n 
           if(phi(j).eq.j) then 
             phi(j) = j - 1 
             k = 1
30           k = k + 1 
             if(k*j.gt.n) goto 40
             phi(k*j) = phi(j*k)-phi(j*k)/j
             goto 30 
40           continue
           endif 
20      continue
        return
        end 
        

!            145159332
!
!real    0m34.294s
!user    0m34.105s
!sys     0m0.032s

! We follow notation from 
! 
!   https://en.wikipedia.org/wiki/Square_root_of_a_2_by_2_matrix  
! 
! we know that necessary condition for matrix to have 2 different decomposition is to have 
!
!     trace +/- det^2 being both perfect squares. In the notation of wiki, 
!
!     k^2 = trace + 2 s
!     l^2 = trace - 2 s  
!    
!         k^2 + l^2 = 2 trace 
!         k^2 - l^2 = 4 s 
! 
!  we generate valid paris of k,l that give us correct rage of trace. 
!
!  we now look for a,b,c,d that produce integer matrices.
!         ( a + s    b     )
!  1/k *  (                )
!         ( c        d + s )  
!
!         ( a - s    b     )
!  1/l *  (                )
!         ( c        d - s )  
!   
!  s^2 = a * d - b * c   
! 
!  1) we see that b and c are multiples of lcm(k,l) 
!  2) given k and l we know tau. we know that d = tau - a. Then a - s > 0 so a > s 
!     but d = tau - a so d - s > 0  tau - a > s  so a < tau - s 
!
! 
!        case                convention      symmetry factor  
!
!        a  = d and c  = b :               : x 1 
!        a != d and c  = b :         a > d : x 2 
!        a  = d and c != b : b > c         : x 2 
!        a != d and c != b : b > c & a > d : x 4 
!
! Kind of funny, but the forum seems to imply that we do not need to care for transposition ?
! 
! let us say that a = d = tau/2 then from equation for s^2 we have b*c = 1/4*k*k*l*l. 
! we know that b,c are p*lcm(k,l) and q*lcm(k,l) so p*q = 1/4*k^2*l^2/lcm(k,l)^2    
! so we can count the solutions by counting the number of divisors of 1/4*k^2*l^2/lcm(k,l)^2
!                  

        program Eu420
        implicit none
        integer , parameter  :: lim=10**7
        integer              :: sigma0(lim) 
        integer              :: k , l , tau, s, tmp, tot, p, a 

        call form_divisor_sigma0(lim,sigma0)
        tot = 0 
        do k = 1 , int(sqrt(real(2*lim*100))) 
        do l = 1 , k - 1 
           if(k*k+l*l.ge.2*lim) exit 
           if(mod(k*k+l*l,2).eq.0.and.mod(k*k-l*l,4).eq.0) then
             tau = (k*k+l*l)/2
             s   = (k*k-l*l)/4
! we work with equation for s^2 = a*d - b*c. We assume a >= d and b>=c 
!    a =  p * k - s%k = q * l + s%l 
!    a > s a < tau - s 
!    b, c ~ lcm(k,l) 
!
             p = 1 + (s+mod(s,k))/k
             do while(p*k-mod(s,k).lt.tau-s) 
                   a = p*k-mod(s,k)
                if(mod(a-mod(s,l),l).eq.0) then
                   tmp = a*tau-a*a-s*s
                   if(mod(tmp,lcm(k,l)**2).eq.0) then 
                      tmp = tmp/lcm(k,l)**2
                      if(tmp.lt.1.or.tmp.gt.lim) then 
                         print *, 'fail'
                         stop
                      endif 
                   tot = tot + sigma0(tmp)
                   endif 
                endif 
                   p = p + 1 
             enddo

           endif 

        enddo
        enddo 

        print *, tot 


        return

        contains 

        elemental integer function lcm(a,b)
        integer, intent (in) ::  a,b
        integer              ::  ia,ib

        ia = a 
        ib = b 
        
200       continue
        if(ia.eq.ib) goto 100 
          if(ia.gt.ib) then 
             ia = ia - ib 
          else 
             ib = ib - ia 
          endif 
          goto 200 
100     continue 

        lcm = a/ia*b         

        return
        end function 
!
        subroutine form_divisor_sigma0(maxd,sigma0)
        implicit none 
        integer       :: maxd, sigma0(maxd)
        integer       :: i , j
        sigma0    = 2
        sigma0(1) = 1  
        do j = 2 , maxd/2
           do i = 2*j,maxd,j
              sigma0(i) = sigma0(i) + 1 
           enddo 
        enddo  

        return
        end subroutine


        end program

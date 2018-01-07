! Because we need total trace, we can make the scaling better, linear I presume.
! Clearly, trace of sigma1 is much cheaper when you don't build the array. 
! The current solution, is runnable, and the problem is not that interesting, so I shall leave it.
!
!gfortran -fdefault-integer-8 -O3 153.f08 -o 153.x ; time ./153.x
! Answer 153 =     17971254122360635
!
!real	9m47.935s
!user	9m28.046s
!sys	0m4.696s
!

        program Eu153
        integer,parameter  ::  max_n=10**8
        integer            ::  sigma1(max_n)
        integer            ::  sigma1_gi(max_n)
        integer            ::  res=0

        call form_divisor_sigma1(max_n,sigma1)
        call form_divisor_sigma1_gi(max_n,sigma1_gi)

        print *, "Answer 153 = ", sum(sigma1+2*sigma1_gi)

        contains

       



        subroutine form_divisor_sigma1_gi(maxd,sigma1)
        implicit none
        integer,intent(in) :: maxd
        integer            :: sigma1(maxd)

        integer            :: a,b,c,d,k,g,n
        integer            :: lim
              
        lim=sqrt(real(maxd,8))
        sigma1 = 0
        do a = 1 , lim
        do b = 1 , lim 
           if(a*a+b*b>maxd) exit
           k = a*a+b*b
           g = gcd(a,b)
           k = k/g
           n = k 
           do while(n<=maxd)
             c = n*a/k/g
             d = n*b/k/g
             if(a*a+b*b<=d*d+c*c)   sigma1(n) = sigma1(n) + a   
             if(a*a+b*b<d*d+c*c )   sigma1(n) = sigma1(n) + d 
             n = n + k 
           enddo       
        enddo
        enddo 

        return
        end  





        elemental integer function gcd(a,b)
        implicit none
        integer,intent(in) :: a,b
        integer            :: ia,ib

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

        gcd = ia         

        return
        end function
                
!
        subroutine form_divisor_sigma1(maxd,sigma1)
        implicit none 
        integer       :: maxd, sigma1(maxd)
        integer       :: i , j
        sigma1    = 1
        do j = 2 , maxd
           do i = j,maxd,j
              sigma1(i) = sigma1(i) + j
           enddo 
        enddo  

        return
        end subroutine


        end program

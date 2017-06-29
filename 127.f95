! well that is quite easy problem if you use the fact that radical is multiplicative. 
! of course it means it only works for coprime numbers but that what we test latter. 
! now, there is the massive pain of shortcircut logic, but well, I am not splitting the if.
!
! Answer 127 =             18407904
!
!real    0m4.031s
!user    0m4.010s
!sys     0m0.008s
!
        program Eu127
        implicit none
        integer,parameter :: max_val = 120000-1
        integer           :: radicals(max_val)               
        integer           :: i,a,b,c,tot 
        
        do i = 1 , max_val 
          radicals(i) = rad(i)
        enddo

        tot = 0 

        do a = 1  , max_val
        do b = a+1, max_val-a
           c = a + b 
           if(c.lt.max_val) then 
           if(radicals(a)*radicals(b)*radicals(c).lt.c) then 
             if(gcd(a,b).eq.1.and.gcd(a,c).eq.1.and.gcd(b,c).eq.1) then
               tot = tot + c
             endif 
           endif  
           endif 
        enddo
        enddo

        print *, "Answer 127 =", tot


        return



        contains

        elemental function rad(n)
        implicit none
        integer, intent(in) ::  n
        integer             ::  rad,m,i
        
        rad = 1
        m = n  
        if(mod(m,2).eq.0) rad = rad * 2  
        do while(mod(m,2).eq.0) ;  m = m/2  ;  enddo
        
        i = 3 
        
        do while(i*i.le.m) 
        if(mod(m,i).eq.0) rad = rad * i 
        do while(mod(m,i).eq.0) ; m = m/i ; enddo
        i = i + 2 
        enddo 
        if(m.ne.1) rad = rad * m



        return
        end function

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

        end program 

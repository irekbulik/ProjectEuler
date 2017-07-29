!We are having situation 
!
!k*k = d * q + r 
!
!First, we note that we can choose d < k and then q > d (they cannot be equal,because 
!then r = 0). We also know that r < d. Then, from 3! ways to form geometric nondecreasing series
!
!r q d - 
!r d q +
!q r d -
!q d r -
!d r q -
!d q r - 
!
!Only the r,d,q is possible (of course, we could switch q and d but then r would be smaller than q, giving the 
!series r q d, but that does not matter). 
!
!So we have two equations:
!		k * k  =  d * q + r 
!		r * q  =  d * d   
!
!Let us now see two cases 
!	1) r and q are coprime. Then r and q are perfect squares. 
!	2) r and q are not coprime. Then r = r' * p, q = q' * p then r' * q' * p^2 = d^2. 
!	   so r' and q' are coprime perfect squares.
!
!	 k * k = p * a * b * p * a * a + p * b * b with a and b coprime.
!
!	 k * k = p^2 * a ^3 * b + p * b * b = p * b ( p * a^3 + b )
!
! the problem is now to arrange loops well. like below.
!
! Answer 141 =          878454337159
!
!real    0m0.144s
!user    0m0.141s
!sys     0m0.001s
!
!
        program Eu141
        implicit none
        integer   a,b,p,k2,t
        integer   maxval
        integer   gcd
        logical  is_sq

        maxval = 10**12

        t = 0 
          do 10 a = 2 , floor((1.0d0*maxval)**(1.0d0/3.0d0)+1.0d0)      
          do 20 b = 1 , a - 1 
          p = 0
30        p = p + 1           
          k2 = p*b*(p*a**3+b)
          if(k2.gt.maxval) goto 20 
          if(is_sq(k2)) then 
            if(gcd(a,b).eq.1) then 
              t = t + k2 
            endif 
          endif 
          goto 30 
20        continue
10        continue 

        print *, 'Answer 141 = ',  t 


        return
        end 

              




        integer  function gcd(a,b)
        integer  a,b
        integer  ia,ib

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
        end 

        function is_sq(n)
        implicit none
        integer        n 
        logical       is_sq
          if(n.lt.0) stop
          is_sq = int(sqrt(real(n)))**2.eq.n
        return
        end 

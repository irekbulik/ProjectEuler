!Relevant math:  
!  http://mathworld.wolfram.com/vonStaudt-ClausenTheorem.html
!  http://mathworld.wolfram.com/FaulhabersFormula.html
!
! [0] gfortran -O3 545.f95  -o 545 ; time ./545 
! Answer =    921107572
!
! real    0m31.657s
! user    0m31.024s
! sys     0m0.176s
!
        program Eu545 
        implicit none
        integer, parameter :: max_k = 3*10**6,max_p=max_k*308+1
        integer         :: k(max_k)
        integer(kind=1) :: p(max_p) 
        integer         :: k_stride,stride,i_prime,tot,val

        call sieve(p,max_p)
        k = 0
        p(2)  = 0
        p(3)  = 0
        p(5)  = 0
        p(23) = 0 
        p(29) = 0

        do i_prime = 1 , max_p
           if(p(i_prime)/=1) cycle 
           val = i_prime - 1
           if(mod(val,2 )==0) val = val/2
           if(mod(val,2 )==0) val = val/2
           if(mod(val,7 )==0) val = val/7
           if(mod(val,11)==0) val = val/11 
           stride   = val 
           k_stride = stride 
           if(k_stride<=max_k) then 
              if(k(k_stride)==0) then 
               do while(k_stride<=max_k) 
                k(k_stride)=1
                k_stride = k_stride+stride
               enddo
              endif 
           endif 
        enddo 
        
        tot = 0
        do k_stride = 1 , max_k
           if(k(k_stride)==0) tot = tot + 1 
           if(tot==10**5) exit 
        enddo 
        
           print *, 'Answer = ', k_stride*308 

        contains


        subroutine sieve(res,maxval)
        implicit none 
        integer  :: maxval
        integer(kind=1)  :: res(maxval)
        integer  :: val, loop
!
        res(1) = -1 
        do 100 val = 2 , maxval 
100        res(val) = 1 
!
        val = 2

50         loop = val        
20         loop = loop + val 
           if(loop.gt.maxval) goto 10 
           res(loop) = -1 
           goto 20
10      continue 
! FIND NEXT PRIME
30      val = val + 1 
        if(val.gt.maxval)    goto 40 
        if(res(val).eq.-1)   goto 30
        goto 50
40      continue 
        return
        end subroutine     

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

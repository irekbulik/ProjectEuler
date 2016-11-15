! gfortran -fdefault-real-8 -fdefault-integer-8  -O3 071.F
! Answer 71 =    428570
!
! real    0m0.029s
! user    0m0.028s
! sys     0m0.001s
!
#define MAX_DEN 1000000
        program Eu071
        implicit none
        integer  d,n 
        integer  gcd
        integer  db,nb
        real*8   best 
        
        best = 0.0d0

        do 200 d = MAX_DEN , 2 , - 1 
           if(mod(d*3,7).eq.0) then 
             n = d*3/7-1
           else 
             n = int(real(d*3)/real(7))
           endif  
          if(n.eq.0) goto 200
100       continue  
          if(real(n)/real(d).le.best) goto 200 
             if(gcd(n,d).eq.1) then        
                  nb   = n 
                  db   = d 
                  best = real(n)/real(d)  
                goto 200 
             else 
               n = n - 1 
               goto 100 
             endif 
200     continue 

        write(*,1800) nb

1800    format('Answer 71 = ',I9)
        return
        end 



        integer function gcd(a,b)
        implicit none
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

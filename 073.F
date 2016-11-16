!Answer 73 =   7295372
!
!real    0m1.304s
!user    0m1.299s
!sys     0m0.003s

#define MAX_DEN 12000
        program Eu073
        implicit none
        integer  d,n,nmin,nmax,tot
        integer  gcd
        
        tot = 0 

        do 200 d = 2, MAX_DEN 
           if(mod(d,3).eq.0) then
              nmin = d/3 + 1 
           else 
              nmin = 1 + int(real(d)/real(3))
           endif 
!
           if(mod(d,2).eq.0) then
              nmax = d/2 - 1 
           else 
              nmax =  int(real(d)/real(2))
           endif 

           do 300 n = nmin,nmax

              if(gcd(n,d).eq.1) then 
                tot = tot + 1           
              endif 

300        continue            

200     continue 

        write(*,1800) tot 

1800    format('Answer 73 = ',I9)
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

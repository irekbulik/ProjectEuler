!Answer 518 =  100315739184392
!
!real    7m10.547s
!user    7m8.772s
!sys     0m0.473s
! Quite Slow. 
!
! Well, learned nice stuff in forum and the answer clear. Let us say I
! did not have coffee when I was solving first time. 
!
#define __maxval__ 10**8
        program Eu518 
        implicit none
        integer  p(__maxval__)
        integer  m,k,a,b,c,n
        integer  tot 
        integer  gcd 
        real*8   r
        call sieve(p,__maxval__)

        tot = 0 
!        
         do 10 m = 1 , int(sqrt(real(__maxval__)))
! LOOP OVER ALL MULTIPLES OF M 
            do 20 a = m*m,__maxval__,m*m
! CHECK IF THAT NUMBER IS PRIME + 1 
               if(p(a-1).eq.1) then 
! LOOP OVER COPRIME NUMBERS TO M
               k = a/(m*m)
               k = int(sqrt(real(__maxval__+1)/k))
               do 30 n = m + 1 , k
                     b = a/m
                     b = b*n
                     c = a/m/m          
                     c = c*n*n
                     if(p(b-1).eq.1.and.p(c-1).eq.1) then
                        if(gcd(n,m).eq.1) tot = tot+a+b+c-3
                  endif
30             continue 
               endif         

20          continue        
!
10       continue 

        write(*,1800) tot 

1800    format('Answer 518 = ',I16)

        return
        end 



        subroutine sieve(res,maxval)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  val, loop
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

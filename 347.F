#define __maxval__ 10000000
#define __maxnum__  5000000
#define __maxpri__   500000
! compile i8 and r8 
!Answer 347 =  11109800204052
!
!real    0m0.498s
!user    0m0.484s
!sys     0m0.010s
!
        program eu347
        implicit none
        integer  i, j
        integer  nums(__maxnum__)
        integer  prims(__maxpri__)
        integer  nprime 
        integer  total 
        integer  m 

        total = 0 

        call sieve(nums,__maxnum__,prims,__maxpri__,nprime)

        do 10 i = 1 , nprime 
        
           do 20 j = i + 1 , nprime 
           if(prims(i)*prims(j).gt.__maxval__) goto 10      
           call  maxn(prims(i),prims(j),__maxval__,m)
           total = total + m 
20         continue                

10      continue 

        write(*,1800)  total

1800    format("Answer 347 = ", I15)

        return
        end 


        subroutine sieve(res,maxval,p,maxp,nprime)
        implicit none 
        integer  maxval,maxp,nprime
        integer  res(maxval),p(*)
        integer  val, loop

        do 1 val = 1 , maxval
           res(val) = 1
1       continue         

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

        if(maxp.lt.2) stop
        nprime = 2
        p(1) = 2 
        p(2) = 3 
         
        do 100 val = 5 , maxval , 2 
           if(res(val).eq.1) then
              nprime = nprime + 1 
              if(nprime.gt.maxp) stop
              p(nprime) = val
           endif 
100     continue                

        return
        end       



        subroutine maxn(p1,p2,n,nmax)
        implicit none
        integer  p1,p2,n,m
        integer  nmax
        integer  exp1,exp2
        integer  ip1,ip2 

        exp1 = int((log(real(n))-log(real(p2)))/log(real(p1)))
        exp2 = int((log(real(n))-log(real(p1)))/log(real(p2)))
        
          nmax = 0 
        if(p1*p2.le.n) then     
          do 10 ip1 = 1 , exp1 
          do 20 ip2 = 1 , exp2  
             m = p1**ip1*p2**ip2
             if(m.gt.n) goto 10 
20           nmax = max(nmax,m)
10        continue         
        endif 
        return
        end 
       

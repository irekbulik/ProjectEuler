#define __limit__ 12
!gfortran -fdefault-integer-8 -fdefault-real-8 -fdefault-double-8 -O3 516.F -o 516 ; time ./516 
!Answer 516 =        939087315
!
!real    0m2.115s
!user    0m2.085s
!sys     0m0.024s
        program Eu516
        implicit none
        logical isprim
        integer ngp
        integer gp(1000)
        integer choice(10,10**6)
        integer temp  (10,10**6) 
        integer ic,val,tot,nvals,ival,k
        real*8  tes
!
        call filgp(10**__limit__,gp,ngp,1000)
        call sort(gp,ngp)
! OK, SO OUR VALUES ARE 2^I*3^J*5^K*GOOD PRIMES WITH POWER UP TO 1        
! WE CAN CHOOSE AT MOST 9 OF THOSE OR 0. THE REST WE PATCH WITH THE
! 2,3,5
        tot = 0  
        do 100 ic = 0,10
           if(ic.eq.0) then 
              choice(1,1) = 1 
              nvals       = 1 
           else 
! ADD SINGLES, PAIRS, DOUBLES, TRIPLES .... 
              call makec(ic,choice,nvals,temp,gp,ngp,__limit__,10**6)
           endif 
! MAKE THE PRODUCT WITH THE CHOICE, AND THE PATCH IT WITH 2,3,5        
           do 200 ival = 1 , nvals 
           val = 1 
             do 210 k = 1 , ic 
210             val = val*choice(k,ival)
           call update(val,__limit__,tot)
200        continue 
100     continue         
           
        write(*,1800)  tot 

1800    format ("Answer 516 = ",I16)

        return
        end 
        
        subroutine update(val,lim,tot) 
        implicit none 
        integer  val,lim,tot 
        integer  mode
        integer  l2,l3,l5
        integer  i2,i3,i5,fac2,fac3,fac5
        integer  myexp
        real*8   lg2,lg3,lg5,lgv,rlim
        data     mode/4294967296/
        save     mode
        
        rlim = real(lim)
        lgv = log10(real(val))
        lg2 = log10(2.0d0)
        lg3 = log10(3.0d0)
        lg5 = log10(5.0d0)
        
        l2 = int((real(lim)-lgv)/lg2)
        l3 = int((real(lim)-lgv)/lg3)
        l5 = int((real(lim)-lgv)/lg5)
        
          do 10 i2 = 0 , l2  
             fac2 =  myexp(2,i2)     
          do 20 i3 = 0 , l3 
                if(i2*lg2+i3*lg3+lgv.gt.rlim) goto 10 
             fac3 =  myexp(3,i3)
          do 30 i5 = 0 , l5 
                if(i2*lg2+i3*lg3+i5*lg5+lgv.gt.rlim) goto 30
             fac5 =  myexp(5,i5)
                tot = mod(tot+fac2*fac3*fac5*val,mode)
30        continue 
20        continue 
10        continue 

        return
        end 

        integer function myexp(b,p)
        implicit none 
        integer b,p
        integer a,n,r

        r = 1 
        a = b 
        n = p 

100     continue 
        
        if(mod(n,2).eq.1) r = r*a
        if(n/2.gt.0) then 
           a = a * a
           n = n / 2 
        else
           goto 200 
        endif 
        goto 100 
200     continue 

        myexp = r

        return
        end 


        subroutine makec(np,arr,len,wrk,gp,ngp,lim,maxv)
        implicit none
        integer  np,arr(10,*),len,wrk(10,*),gp(*),ngp,lim
        integer  maxv
        integer  i,k,j,val,ip
        
         do 10 i = 1 , len 
         do 10 k = 1 , np - 1       
10           wrk(k,i)=arr(k,i)
! OTHERWISE, ADD MORE            
         j = 0      
         do 20 i = 1 , len 
            val = 1 
            do 30 k = 1 , np-1
30          val = val * wrk(k,i) 
! ADD NEW LEVEL                            
            do 40 ip = 1 , ngp  
! ADD ONLY BIGGER ONE
               if(np.ge.2) then  
                 if(gp(ip).le.wrk(np-1,i)) goto 40 
               endif 
! MAKE SURE IT FITS 
           if(log10(real(val))+log10(real(gp(ip))).gt.real(lim)) goto 20
                  j = j + 1 
                  if(j.gt.maxv) stop('allocate more val')
                  do 46 k = 1 , np - 1 
46                   arr(k, j) = wrk(k,i)
                     arr(np,j) = gp(ip)                
40          continue         
20        continue 
          len = j 
        return
        end 


        subroutine filgp(l,gp,n,m)
        implicit none
        integer  l,n,m
        integer  gp(*)
        integer  i,j,k
        integer  v 
        logical  isprim
        real*8   ll 
        
        ll = log(real(l))
                
          n = 0 
          do 10 i = 0 , Int(Log(real(l))/Log(2.0d0))
          do 20 j = 0 , Int(Log(real(l))/Log(3.0d0))
          do 30 k = 0 , Int(Log(real(l))/Log(5.0d0))
           if(i*log(2.0d0)+j*log(3.0d0)+k*log(5.0d0).lt.ll) then 
              v = 2**i*3**j*5**k+1
              if(isprim(v).and.v.gt.5) then 
                n = n + 1 
                if(n.gt.m) stop('allocate too litte')
                gp(n) = v                 
              endif 
           else 
              goto 20 
           endif  
30       continue                 
20       continue                 
10       continue                 
        return
        end 

        logical function isprim(n)
        implicit none 
        integer  n ,d 
         isprim = .true.
         if(mod(n,2).eq.0) then 
            isprim = .false.
            return
         else 
            do 10 d = 3,int(sqrt(real(n))),2
               if(mod(n,d).eq.0) isprim = .false.
               if(.not.isprim) return
10          enddo          
         endif 
         return
         end   
        subroutine sort(a,n)
        implicit none
        integer  a(*),n
        integer  ne,nb,bc,w,b

! HIRING PART. WE START WITH N/2+1 HIRED PEOPLE AND WE EMPLOY POTENTIAL 
! N/2 BOSSED
        ne = n
        nb = n/2+1
200     continue         
        if(nb.gt.1) then 
! HIRE GUY AND THEN ASSES HIM            
           nb = nb - 1 
           bc = a(nb)
        else 
! RETIRE THE TOP GUY, TAKE THE LAST ONE AND GIVE HIM THE PLACE. THEN
! VERIFY  
           bc    = a(ne)
           a(ne) = a(1) 
           ne    = ne - 1 
             if(ne.eq.1) then 
                a(1) = bc 
                return
             endif 
!
         endif 
! SET AS THE BOSS THE ITH ELEMENTS 
        b = nb 
        w = 2 * b 
! IN THE HIRE LOOP, FOR BOSS B WE MAKE SURE THAT NO WORKER W
! SUPERVISED BY B HAS BETTER QUALIFICATIONS. THE WORKERS ARE 
! 2*B AND 2*B + 1 . IS THE SWITCH IS DONE, THEN WE NEED TO MAKE SURE 
! NO MORE LEVELS OF DEMOTION ARE NEEDED. BOOS QUALIFICATIONS ARE BC.
! THERE ARE NE PEOPLE.
100      continue 
         if(w+1.le.ne) then 
           if(a(w+1).gt.a(w)) w = w + 1 
         endif 
         if(bc.lt.a(w)) then 
            a(b) = a(w)         
! CHECK IF YOU NEED TO DEMOTE B FURTHER                    
            b    = w
            w    = 2 * b 
         else 
! YOU DON'T, SKIP TO THE LAST EMPLOYEE 
            w = ne + 1 
         endif 
         if(w.le.ne) goto 100 
! NO MORE DEMOTION IS NEEDED. PUT OLD BOSS QUALIFICATIONS INTO HIS NEW
! POSITION 
         a(b) = bc 

         goto 200 

         return
         end 

#define __maxpow__ 47
#define __maxpal__ 1000000
#define __maxcon__ 6*10**7
#define __limit__  10**10
!
!       35856681704365
!
!real    0m0.491s
!user    0m0.486s
!sys     0m0.003s
!
! Ok, I hate that. Looking at the few limits, it seems that we always
! have a new numer 100100.... + previous palindromes. The number happens 
! to be correct. No cool.
!
        program phinar
        implicit none
        logical   ispal 
        integer*8   i,k,np,ap
        integer*8   binom
        integer*8   pos(__maxpow__+1)
        integer*8   pal(__maxpal__)
        integer     tot
        integer*8   ans(2)
        integer*8   maxpow                
        integer     gpow
        maxpow = __maxpow__
        pal(1) = 0 
        pal(2) = 2 
        pal(3) = 14
        np     = 3
        do i = 7 , maxpow + 1
           do k = 1 , maxpow+1
           pos(k) = 0 
           enddo 
           pos(i)  = 1 
           pos(i-3)= 1 
           call phi2dc(pos,i+1,pos,i,ans)
           if(ans(2).eq.0) then 
              ap = 0 
              do k = 1 , np
              call phiprt(pal(k)+ans(1),ispal,.false.)
                if(ispal) then 
!                   print *, i,pal(k)+ans(1)
                   ap = ap + 1 
                   pal(np+ap) = pal(k) + ans(1)
                endif        
              enddo
              np = np + ap 
           endif
        enddo
!            call sort(pal,np) 
!            call uniq(pal,np)   
            i = 0       
            do k = 1 , np 
               if(pal(k).lt.__limit__) then     
                i = i + pal(k)
               endif
            enddo 
        print *, i +1 
        return
        end 
        
        subroutine uniq(a,n)
        integer    n,i,m
        integer    a(*)
         m = 1
         do 10 i = 2 , n 
            if(a(i).ne.a(i-1)) then
              m = m+1
              a(m) = a(i)
            endif 
10      continue 
        n = m 
        return
        end 

        subroutine phi2dc(pos,np,neg,nn,ans)
        implicit none
! pos: coefficients for 0,1,2,3,...np-1
! neg: coefficients for -1,-2,-3,....,-nn
! WE EXPAND POLYNOMIALS USING 1/PHI = (PHI-1)          
! ANS = ANS(1) + ANS(2) * PHI
        integer*8   ans(2)
        integer*8   np,nn
        integer*8   pos(0:np-1),neg(1:nn)
        integer*8   maxpoly
        parameter (maxpoly=100)
        integer*8   poly(0:maxpoly)        
        integer*8   maxp,maxn
        integer*8   i,j
        integer*8   binom
! DETERMINE THE SIZE OF THE POLYNOMIAL 
           i    = np 
           maxp = 0 
10         i = i - 1         
           if(i.lt.0) then 
              maxp = 0
              goto 11
           endif 
           if(pos(i).eq.1) maxp = i
           if(maxp.eq.0) goto 10  
11         continue
           i    = nn+1
           maxn = 0 
20         i = i - 1        
           if(i.lt.1) then
              maxn = 0
              goto 21               
           endif 
           if(neg(i).eq.1) maxn = i
           if(maxn.eq.0) goto 20
21         continue 
!
!
        do 30 i = 0 ,maxp 
30        poly(i) = pos(i) 
!
        do 31 i = maxp + 1 , maxn                           
31        poly(i) = 0 
!
!
        do 32 i = 1 , maxn 
           if(neg(i).eq.1) then 
              do 33 j = 0, i
33            poly(i-j) = poly(i-j) + binom(i,j)*(-1)**j
           endif 
32      continue        

        do 40 i = max(maxp,maxn) -1, 1, -1
        poly(i  ) = poly(i+1) + poly(i  )
        poly(i-1) = poly(i+1) + poly(i-1) 
        poly(i+1) = 0
40      continue
        ans(1) = poly(0)
        ans(2) = poly(1)
        
        return
        end 
        

        integer*8 function binom(n,m)
        if(n.lt.m.or.m.lt.0) stop('bad input in binom')
        binom = 1 
        do 10 i = 1 , m 
10        binom = binom*(n-i+1)/i
        return
        end 

        subroutine iclear(n,a)
        logical*1 a(*)
        do 10 i = 1 , n 
10         a(i) = .false.
        return
        end 
                      
        subroutine imove(n,a,b)
        logical*1 a(*),b(*)
        do 10 i = 1 , n 
10         b(i) = a(i)
        return
        end 
        subroutine move(n,a,b)
        logical*1 a(*)
        integer*8   b(*)
        do 10 i = 1 , n 
           b(i) = 0
10         if(a(i)) b(i) = 1 
        return
        end 
        subroutine phiprt(n,pal,print)
        integer*8   maxlen
        parameter (maxlen=200)
        integer*8   n 
        logical   pal,print
        integer*8   rep(maxlen)
        integer*8   ren(maxlen)
        character answer(-maxlen:maxlen)
        integer*8   zero(2) 
        integer*8   n1(2),n2(2),n3(3),in,ip,len,i
        real*16    phi
        parameter (phi=(1.0d0+sqrt(5.0d0))/2)
        integer*8   comphi
        data      zero/0,0/
!
        if(n.le.0) stop

        n1(1) = n
        n1(2) = 0
        in = 0
        ip = 0 
        
100        i     = maxpow(n1) 
! see the damn stabilty          
           call powphi(n2,i)
           if(i.ge.0) then 
             ip = ip + 1 
             if(ip.gt.maxlen) stop('too large ip')
             rep(ip) = i
           else 
             in = in + 1 
             if(in.gt.maxlen) then
                pal = .false.
                return
             endif 
             ren(in) = i
           endif 
           call opphi('-',n1,n2,n1)
           if((n1(1).eq.0.and.n1(2).eq.0)) goto 200 
           goto 100 
200     continue 
! WRITE THE ANSWER 
        if(ip.eq.0.and.in.eq.0) stop('number is 0')
! CHECK IF FITS 
         if(rep(1).gt.maxlen) stop 
           if(in.gt.0) then 
             if(ren(in).le.-maxlen) stop
           endif 
         do 10 i = rep(1) , 0 , -1 
10          answer(i)='0'
         if(in.gt.0) then 
         do 11 i = ren(in) , -1 
11          answer(i)='0'
         endif 
         do 20 i = 1 , ip 
20          answer(rep(i)) = '1'       
         answer(-1) = '.'
         do 30 i = 1 , in 
30          answer(ren(i)-1) = '1'             
!
         if(in.gt.0) then 
         pal = rep(1)+1.eq.abs(ren(in))
         if(pal) then 
             do 40 i = rep(1),0,-1
             pal = pal.and.answer(i-1).eq.answer(-i-1)
40           if(.not.pal) goto 50
50         continue  
         endif        
         else 

         endif 

!
         if(print) then 
           if(in.gt.0) then 
            write(*,*) (answer(i),i=rep(1),ren(in)-1,-1),pal
           else 
            write(*,*) (answer(i),i=rep(1),0,-1),pal              
           endif 
         endif           

         
        return
        end                

        subroutine powphi(n,m)
        integer*8    m,n(2)
        integer*8    k(2),l(2)
        integer*8    i 
        data       k/-1,1/,l/0,1/

        if(m.lt.0) then 
        n(1) = k(1)        
        n(2) = k(2)        
        do 10 i = 2 , -m  
10           call opphi('*',k,n,n)
        elseif(m.gt.0) then 
        n(1) = l(1)        
        n(2) = l(2)        
        do 20 i = 2 , m  
20           call opphi('*',l,n,n)
        else
         n(1) = 1
         n(2) = 0
        endif   

        return
        end              

        subroutine opphi(key,n1,n2,n3)
        implicit none
        integer*8   n1(2),n2(2),n3(2)
        integer*8   a,b
        character key 

        if    (key.eq.'+') then
          n3(1) = n1(1) + n2(1)
          n3(2) = n1(2) + n2(2)
        elseif(key.eq.'-') then
          n3(1) = n1(1) - n2(1)
          n3(2) = n1(2) - n2(2)
        elseif(key.eq.'*') then 
          a = n1(1)*n2(1)+n1(2)*n2(2)
          b = n1(1)*n2(2)+n1(2)*n2(1)+n1(2)*n2(2)       
          n3(1) = a 
          n3(2) = b
        else
          print *, 'not implemented'
          stop
        endif 
        return
        end 


        integer*8 function maxpow(n) 
        implicit none
        integer*8  n(2)
        integer*8  comphi
        integer*8  c 
        integer*8  phi(2),m(2),iphi(2)
        data     phi/0,1/,iphi/-1,1/       
!        
        if(n(1).eq.1.and.n(2).eq.0) then
          maxpow = 0
          return
        endif   

        c = comphi(n,phi) 
!
        if(c.eq.0) then 
           maxpow = 0 
        elseif(c.eq.1) then
           maxpow = 0
           m(1) = 1
           m(2) = 0
10         maxpow = maxpow + 1 
           call opphi('*',phi,m,m)
           if(comphi(n,m).eq.1) goto 10 
           maxpow = maxpow - 1 
        else         
           maxpow = 0
           m(1) = 1
           m(2) = 0
20         maxpow = maxpow - 1 
           call opphi('*',iphi,m,m)
           if(comphi(n,m).eq.2) goto 20
        endif 
        return
        end 


        integer*8 function comphi(n1,n2)
        implicit none
        integer*8   n1(2),n2(2)
        real*8    sq5,zero
        parameter (sq5=sqrt(5.0d0),zero=0.0d0)
        real*8       lhs,rhs
        integer*16   ilh,irh
 
        if(n1(1).eq.n2(1).and.n1(2).eq.n2(2)) then 
          comphi = 0
          return
        endif 
       
        if(n2(2).eq.n1(2)) then
             if(n1(2).gt.n2(1)) then 
               comphi = 1 
             else 
               comphi = 2
             endif      
          return
        endif 
        if(n2(1).eq.n1(1)) then
             if(n1(2).gt.n2(1)) then 
               comphi = 1 
             else 
               comphi = 2
             endif      
          return
        endif 

        lhs = 2.0d0*(n1(1)-n2(1))-1.0d0*(n2(2)-n1(2))
        rhs = (n2(2)-n1(2))*sq5
        
         if(rhs*lhs.lt.zero) then               
            if(rhs.lt.lhs) then 
              comphi = 1 
            else 
              comphi = 2
            endif
           return
         else 
           ilh = (2*(n1(1)-n2(1))-(n2(2)-n1(2)))
           ilh = ilh*ilh
           irh =  (n2(2)-n1(2))
           irh = 5 * irh * irh
           if(ilh.lt.0.or.irh.lt.0) print *, ilh,irh
               if(irh.lt.ilh) then
                  comphi = 1  
                  if(rhs.lt.zero) comphi = 2 
               else
                  comphi = 2  
                  if(rhs.lt.zero) comphi = 1
               endif 
            return
         endif 

         return
         end 


        subroutine mkcon(nmax,maxcon,con,tot)
        implicit none
        integer*8   tot
        integer*8   nmax,maxcon
        logical*1 con(0:nmax,maxcon)
        integer*8   lnm 
        integer*8   ptot,add
        integer*8   i,j,k
!
        if(nmax.lt.1) stop('number passed less or equal 1')
! GENERATE FIRST LIST OF FOR POWERS 0 AND 1 
        call iclear(nmax+1,con(0,1))
        call iclear(nmax+1,con(0,2))
        con(1,2) = .true.
!
        ptot = 2 
        do 100 i = 2, nmax 
! AT MOST, WE GENERATE 2 * PTOT NEW CONF BUT PART OF THEM HAVE ATTACHED
! 0, SO THEY DON'T CHANGE
           if(ptot*2.gt.maxcon) stop('too little conf allocated')
           add = 0
           do 10 j = 1, ptot 
             if(.not.(con(i-1,j).or.con(i-2,j))) then
             add = add + 1 
             call imove(nmax+1,con(0,j),con(0,ptot+add))
             con(i,ptot+add) = .true.
             endif 
10         continue 
           tot  = ptot + add         
           ptot = tot 
           print *, 'generated ',i,tot
#if 0
           print *, ""
           print *, ""
           do k = 1 , tot 
           write(*,*), k,(con(j,k),j=0,nmax)        
           enddo
           print *, ""
           print *, ""
#endif
100     continue                
1800    format(20(a,2x))        

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

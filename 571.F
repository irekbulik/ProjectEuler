#define __maxel__ 20 
#define __bas__ 12
! slightly over a minute, not terrible 
!number(   1 ) =        1587937206284 sum =        1587937206284
!number(   2 ) =        1674839888205 sum =        3262777094489
!number(   3 ) =        2638904175622 sum =        5901681270111
!number(   4 ) =        2806980157234 sum =        8708661427345
!number(   5 ) =        2816957039424 sum =       11525618466769
!number(   6 ) =        3325871906940 sum =       14851490373709
!number(   7 ) =        3863090145827 sum =       18714580519536
!number(   8 ) =        3909765781284 sum =       22624346300820
!number(   9 ) =        3925260871994 sum =       26549607172814
!number(  10 ) =        3960783529164 sum =       30510390701978
         program Eu571
         implicit none 
         logical  ispan,ok
         integer  digs(12)
         integer  perm(12) 
         integer  bas 
         integer  i,fac,n,k,s,f
         save     digs
         data     digs/0,1,2,3,4,5,6,7,8,9,10,11/
         fac = 1 
         bas = __bas__
         do i = 2, bas-1 
           fac = fac*i
         enddo

         s = 0 
         f = 0
         do i = 1+fac,bas*fac
            call permut(i,bas,perm)
            n = 0 
            do k = 1 , bas-1 
              n = n + digs(perm(k))*bas**(bas-k) 
            enddo
              n = n + digs(perm(bas))
            ok = .true.
! WELL, THOSE ARE ALREADY CORRECT 
           do  k = bas-1,3,-1
              ok = ispan(n,k) 
              if(.not.ok) goto 100
           enddo
100      continue
            if(ok) then 
            s = s + n 
            f = f + 1 
            write(*,1800) f,n,s
            if(f.eq.10) goto 200 
            endif 
         enddo
200     continue 
1800    format('number( ',I3,' ) = ',I20,' sum = ', I20)        
        return
        end 

        logical function ispan(n,k)
        implicit none
        integer  n,k
        integer  wrk(0:11)
        integer  i,maxexp,nn,va,kk
        
        do i = 0 , k-1
           wrk(i) = 0 
        enddo
        maxexp = int(log(real(n))/log(real(k)))
        nn = n 
        kk = k**maxexp
        if(kk.gt.nn) kk = kk/k
        do i = maxexp , 0, -1          
           va = nn/kk
           nn = nn - va*k**i
           wrk(va) = 1  
           kk = kk/k
        enddo 
        nn = 0                 
        do i = 0 , k-1
           nn = nn + wrk(i) 
        enddo 

        ispan = nn.eq.k 
        
        return
        end 


        subroutine permut(ind,elm,res)
        implicit none
        integer  res(*)
        integer  elm,ind
        integer  tmp(__maxel__)
        integer  ipos,cval
        integer  fact 
        integer  lind 
        integer  k 
        
          fact = 1 
          lind = ind 
        do 10 ipos = 1 , elm
          fact = fact * ipos 
10        tmp(ipos) = ipos 
        if(ind.gt.fact) then 
          print *, 'out of bounds in permut'
        endif          
        do 100 ipos = 1 , elm 
           fact = fact/(elm-ipos+1)
           cval = (lind-1)/fact + 1 
           res(ipos) = tmp(cval) 
              do 110 k = 1 , cval - 1 
110           tmp(k) = tmp(k) 
              do 120 k = cval + 1 , elm - ipos + 1 
120           tmp(k-1) = tmp(k)                         
           lind = mod(lind-1,fact) + 1 
100     continue         
        return
        end 
        

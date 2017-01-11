#define __elms__ 11
#define __goal__ 2011 
!Answer 336 = CAGBIHEFJDK
!
!real    0m28.398s
!user    0m28.293s
!sys     0m0.039s
        program eu336
        implicit none
        integer  res(11) 
        integer  move(39916800) 
        integer  i,k,m, t
        integer  ichar 
        character char 

         m = 0 
         t = 0 
         do i = 1 , 39916800
         call permut(i,__elms__,res)
         call solv_p(__elms__,res,k)
         move(i) = k 
         m = max(m,k)
         enddo 
        
         do i = 1, 39916800
            if(move(i).eq.m) then 
            t = t + 1 
              if(t.eq.__goal__) then 
              call permut(i,__elms__,res)
              write(*,1800) (char(ichar('A')-1+res(k)),k=1,__elms__)     
              return
              endif 
            endif         
         enddo 
1800    format('Answer 336 = ',11(A))

        return
        end 

#define __maxel__ 20 
        subroutine solv_p(elm,per,nmove)
        implicit none
        integer  per(*),nmove,elm
        integer  tmp(__maxel__) 
        integer  ipos,jpos,i
        
          nmove = 0 
!
! CHECK IF THE PERMUTATIONS ARE IN ORDER 
!
          do 100 ipos = 1 , elm - 2
! THE LAST TWO ARE DIFFERENT   
              if(per(ipos).ne.ipos) then 
                jpos  = ipos  
20              jpos  = jpos + 1 
                if(jpos.gt.elm) then 
                  print *, 'inconsistency'      
                  stop
                endif 
                if(per(jpos).ne.ipos) goto 20 
!                print *, 'found jpos',jpos
                do 30 i = 1 , jpos - 1 
30                 tmp(i) = per(i)
                do 31 i = jpos, elm     
31                 tmp(elm-i+jpos) = per(i) 
!               print *, 'tmp',(tmp(i),i=1,elm)
                do 32 i = ipos , elm 
32                 per(i) = tmp(elm-i+ipos)         
                                nmove = nmove + 1
                if(jpos.ne.elm) nmove = nmove + 1 
!               print *, 'per',(per(i),i=1,elm)
              endif 
100      continue                 
!
          if(per(elm-1).ne.elm-1) then 
             nmove = nmove + 1                        
             if(per(elm).ne.elm-1.and.per(elm-1).ne.elm) stop
             per(elm)   = elm
             per(elm-1) = elm-1
          endif
          
!
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
!           print *, cval
!           write(*,1800), (tmp(k),k=1,elm-ipos)
           lind = mod(lind-1,fact) + 1 
100     continue         
        return
        end 
        

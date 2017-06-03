! THE PROGRAM USES THE PADDING APPROACH TO THE PROBLEM. WE NOW THAT A VALUE N
! WILL BE AN ANSWER IS WE WRITE N ANS SAY A * B * C THEN 
! WE CAN TAKE N - A - B - C AND THAT CAN BE PADDED WITH ONES AND ADDED TO A,B,C  
! Answer 088     7587457
!
!real    0m0.219s
!user    0m0.216s
!sys     0m0.003s
!
        program Eu088
        implicit none
        integer, parameter  :: max_k=12000,max_sums=1000,max_n=30
        integer      :: kvals(1:max_k)
        integer      :: sums(0:max_n,max_sums)
        integer      :: n_sums,i,k,l

        kvals = -10**8
        kvals(1) = 1
        i = 4
        do while (minval(kvals).lt.0) 
        call allowed_sums(i,sums,n_sums,max_n,max_sums)
             do k = 1 , n_sums    
                l = i - sums(1,k) + sums(0,k) 
                if(l.le.max_k) then 
                if(abs(kvals(l)).gt.i) kvals(l) = i 
                endif 
             enddo 
             i = i + 1 
        enddo   

        
        call sort(kvals,max_k) 
        k = kvals(2)
        do i = 3 , max_k 
           if(kvals(i).ne.kvals(i-1)) k = k + kvals(i)        
        enddo  

        print *, "Answer 088", k

        end 


        

        subroutine allowed_sums(n,sums,n_sums,max_n,max_sums)
        implicit none 
        integer              :: n,n_sums,max_n,max_sums,sums(0:max_n,max_sums)
        integer, parameter   :: max_d=100000
        integer              :: divs(max_d),nd  
        integer              :: tot_sum,i,j,cur_test,upper_limit,lower_limit
        logical              :: have_some 
        integer              :: posarr
! COMPUTE THE DIVISORS. 
        call divisors(n,divs,nd,max_d) 
        
        sums = 0 
        tot_sum = 0
        i       = 2  
        do while(n/divs(i).ge.divs(i)) 
           tot_sum = tot_sum +1         
           sums(0,tot_sum) = 2 
           sums(1,tot_sum) = divs(i)
           sums(2,tot_sum) = n/divs(i)
           i       = i + 1 
        enddo 
!
        have_some  = .true.
        upper_limit = 0 
        do while(have_some)
!
          have_some  = .false.
          lower_limit = upper_limit + 1 
          upper_limit = tot_sum 
!
            do i = lower_limit , upper_limit
!
               cur_test = sums(sums(0,i),i)
!
                    j  =  posarr(sums(sums(0,i)-1,i),divs,nd)
                        
                        
                    do while(divs(j).le.cur_test/divs(j))
                     if(mod(cur_test,divs(j)).eq.0.and.divs(j).ge.sums(sums(0,i)-1,i)) then 
                       tot_sum = tot_sum + 1 
                       have_some = .true.
                       if(tot_sum.gt.max_sums) then 
                          print *, 'too many sums'
                          stop
                       endif 
                       sums(0:sums(0,i),tot_sum)=sums(0:sums(0,i),i) 
                       sums(0,tot_sum)=sums(0,tot_sum)+1 
                       sums(sums(0,i)   ,tot_sum) = divs(j) 
                        if(sums(0,i)+1.gt.max_n) then 
                           print *, 'too may groups'
                           stop
                        endif 
                       sums(sums(0,i)+1 ,tot_sum) = cur_test/divs(j)
                     endif 
                     j = j + 1 
                  enddo 
!
             enddo 
!
          enddo 
 

       
        do i = 1 , tot_sum 
           cur_test = 0 
           do j = 1 , sums(0,i)
           cur_test = cur_test + sums(j,i)        
           enddo 
           sums(1,i) = cur_test 
       enddo 
                

        n_sums = tot_sum

        return 
        end subroutine 

        integer function posarr(val,arr,n)
        implicit none
        integer  :: val,n,arr(*)
!       
        integer  :: lv,rv,tv
!        
        lv = 1
        rv = n 
        tv = 1 + n/2
!
100     continue
           if(arr(tv).eq.val) then 
              posarr = tv
              return
           else 
              if(rv.eq.lv) stop('search failed')
              if(arr(tv).lt.val) then 
                lv = tv + 1 
              else
                rv = tv - 1     
              endif             
                tv = rv - lv 
                tv = tv/2
                tv = lv + tv
             endif 
         goto 100 
        return
        end 




        subroutine divisors(n,divs,nd,max_d)    
        integer             :: n,divs(*),nd,max_d 
        integer, parameter  :: max_factors=17
        integer             :: factors(2,max_factors)
        
        call factor_integer(n,factors,n_factors,max_factors) 
! CHECK HOW MANY DIVISORS THERE ARE             
           nd = 1 
        do i = 1 , n_factors
           nd = nd * (factors(1,i)+1)
        enddo 
           if(nd.gt.max_d) then 
             print *, 'too many divisors'
             stop
           endif          


        divs(1:nd) = 1 
        n_b_e = 1 
        n_b_i = nd
!
        do i = 1 , n_factors
           k = 1 
           n_b_i = n_b_i / (factors(1,i)+1)
           do i_b_e = 1 , n_b_e 
           do ip = 0 , factors(1,i) 
           do i_b_i = 1 , n_b_i 
              divs(k) = divs(k)*factors(2,i)**ip
              k = k + 1 
           enddo
           enddo 
           enddo 
           n_b_e = n_b_e * (factors(1,i)+1)
        enddo 

        call sort(divs,nd) 
        return
        end subroutine 



        subroutine factor_integer(n,factors,n_factors,max_factors) 
        implicit integer (a-z) 
        dimension :: factors(2,max_factors)
        factors   = 0  
        n_factors = 0
        m = n 
         if(mod(m,2).eq.0) then 
            n_factors = n_factors + 1 
            if(n_factors.gt.max_factors) then 
              print *, 'too many factors'
              stop
            endif 
            factors(1,n_factors) = 0
            factors(2,n_factors) = 2 
          do while(mod(m,2).eq.0) 
            m = m / 2 
            factors(1,n_factors) = factors(1,n_factors) + 1 
          enddo
         endif         

         i = 3 
         do while(m.ge.i*i) 
            if(mod(m,i).eq.0) then 
            n_factors = n_factors + 1 
            if(n_factors.gt.max_factors) then 
              print *, 'too many factors'
              stop
            endif 
             factors(1,n_factors) = 0
             factors(2,n_factors) = i
             do while(mod(m,i).eq.0) 
              m = m / i 
              factors(1,n_factors) = factors(1,n_factors) + 1 
             enddo
            else 
              i = i + 2              
            endif 
         enddo 

         if(m.ne.1) then 
             n_factors = n_factors + 1 
             if(n_factors.gt.max_factors) then 
              print *, 'too many factors'
              stop
             endif 
             factors(2,n_factors) = m
             factors(1,n_factors) = 1 
         endif 
        
         return
         end subroutine 


        subroutine sort(a,n)
        implicit none
        integer  :: a(*),n
        integer  :: ne,nb,bc,w,b
        
        ne = n
        nb = n/2+1
200     continue         
        if(nb.gt.1) then 
           nb = nb - 1 
           bc = a(nb)
        else 
           bc    = a(ne)
           a(ne) = a(1) 
           ne    = ne - 1 
             if(ne.eq.1) then 
                a(1) = bc 
                return
             endif 
!
         endif 
        b = nb 
        w = 2 * b 
100      continue 
         if(w+1.le.ne) then 
           if(a(w+1).gt.a(w)) w = w + 1 
         endif 
         if(bc.lt.a(w)) then 
            a(b) = a(w)         
            b    = w
            w    = 2 * b 
         else 
            w = ne + 1 
         endif 
         if(w.le.ne) goto 100 
         a(b) = bc 

         goto 200 

         return
         end 

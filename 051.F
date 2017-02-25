#define __maxval__ 10**6
        program Eu51
        implicit  none
        integer   p(__maxval__)
        integer   r(3)
! There is no more than that candidates 
        integer   vals(6*10**3)
        integer   idig,iv,it,ic,tot
        integer   numba,np
        integer   family
        integer   fromd
        integer   binom

! For family of 8, it is quite easy to see we must replace 3*n digits.
! Assume answer is below 10**8, then we can only replace 3 numbers. 
        call sieve(p,__maxval__)
! generate possible paddings. they have to add to nondivisible by 3.
! also, we can never replace the last digits because there are no 8 odd
! values 

        do idig = 6,6
        call genval(vals,tot,idig-3)
! make substitution pattern                             
          do ic = 1 , binom(idig-1,3) 
          call choose(ic,idig-1,3,r)  
          do it = 1 , tot
             family = 0
             np     = 0 
          do iv = 0 , 9 
             numba = fromd(idig,vals(1+(it-1)*(idig-3)),r,iv)
             if(numba.ge.10**(idig-1)) then 
                if(p(numba).eq.1) then 
                   if(family.ne.0) then 
                      np = np + 1 
                      if(np.ge.8) print *, 'family',family
                   else         
                      np     = 1
                      family = numba 
                   endif 
                endif 
             endif 
          enddo
          enddo
          enddo
        enddo


        return
        end

        integer function fromd(n,vals,r,iv)
        implicit none
        integer  n
        integer  vals(*),r(*),iv
        integer  k,donr,done,dig
        
        fromd = 0 
        donr  = 1
        done  = 1 
        do k = 1 , n
           if(k.eq.r(donr)) then       
           dig  = iv
           donr = donr + 1 
           else 
           dig  = vals(done)
           done = done + 1      
           endif 
        fromd = fromd + dig*10**(n-k)
        enddo

        return
        end 

        subroutine genval(vals,c,n)
        implicit none 
        integer  n,c
        integer  vals(n,*) 
        integer  ip,ir,lb,iv,k
        
        do ip = 1 , n
        c = 0  
        do ir = 1 , 10**(ip-1)
           lb = 10**(n-ip)
           do iv = 0,9
           do k  = 1,lb
           c = c+1
           vals(ip,c) = iv 
           enddo
           enddo
        enddo 
        enddo 
        c = 0         
        do ip = 1 , 10**n
           if(mod(vals(n,ip),2).ne.0) then 
           ir = 0
           do k = 1 , n 
           ir = ir + vals(k,ip) 
           enddo
           if(mod(ir,3).ne.0.or..true.) then 
              c = c + 1 
              do k = 1, n 
              vals(k,c) = vals(k,ip) 
              enddo
!              print *, (vals(k,c),k=1,n)
           endif 
           endif 
        enddo

        return
        end 
          




        subroutine choose(i,n,p,r)
        implicit none
        integer  i,n,p,r(*)
        integer  ii,ij,is,iv,ip
        integer  binom 
! FILL POSITIONS               
           ii = i 
           is = 1 
        do 10 ip = 1 , p 
           do 20 iv = is , n
             ii = ii - binom(n-iv,p-ip) 
             if(ii.le.0) then 
                r(ip)=iv
                is   =iv+1
                ii   =ii + binom(n-iv,p-ip)
                goto 10 
             endif 
20         continue
10       continue        


        return
        end 

        integer function binom(n,m)
        if(n.lt.m.or.m.lt.0) then 
                binom = 0
                return
        endif 
        binom = 1
        do 10 i = 1 , m
10        binom = binom*(n-i+1)/i
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

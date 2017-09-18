        program Eu609
! Answer 609 = 172023848
!
!real    0m5.508s
!user    0m5.413s
!sys     0m0.062s
        implicit none
        integer,parameter   :: limit=10**8
        integer(kind=1)     :: prime(limit)
        integer,allocatable :: k_vals(:)
        integer             :: limit_k
        integer             :: prime_pi(limit)
        integer             :: i,ik,ii,non_prime
! We could do the russian peasant one, but well, we do not have to 
        integer(kind=16)  :: res            
        call sieve(prime,limit)
        prime_pi(1) = 0 
        do i = 2 , limit
           prime_pi(i) = prime_pi(i-1) + prime(i)
        enddo

        ii      = limit
        limit_k = 0
        do while(prime_pi(ii).gt.0) 
           limit_k = limit_k + 1
           ii = prime_pi(ii)
        enddo 
        allocate(k_vals(0:limit_k))        
!prime_pi(2) = 1 
        k_vals    = 0 
        do i = 2 , limit 
! Count number of elements that are not prime 
        non_prime = 0 
        ii = i 
        if(prime(ii).eq.0) non_prime = non_prime + 1 
        ii = prime_pi(ii)
        if(prime(ii).eq.0) non_prime = non_prime + 1 
        k_vals(non_prime) = k_vals(non_prime) + 1 
        ii = prime_pi(ii) 
           do while(ii.ne.0) 
           if(prime(ii).eq.0) non_prime = non_prime + 1 
           k_vals(non_prime) = k_vals(non_prime) + 1 
           ii = prime_pi(ii)   
           enddo 
        enddo 
        res = 1 
        do i = 0, limit_k
           if(k_vals(i).ne.0) res = mod(res*k_vals(i),1000000007)
        enddo 
        print*, "Answer 609 =", res
        deallocate(k_vals)
        contains

        subroutine sieve(res,maxval)
        implicit none 
        integer  :: maxval
        integer(kind=1)  :: res(maxval)
        integer  :: val, loop
!
        res(1) = 0
        do 100 val = 2 , maxval 
100        res(val) = 1 
!
        val = 2

50         loop = val        
20         loop = loop + val 
           if(loop.gt.maxval) goto 10 
           res(loop) = 0
           goto 20
10      continue 
! FIND NEXT PRIME
30      val = val + 1 
        if(val.gt.maxval)    goto 40 
        if(res(val).eq.0)   goto 30
        goto 50
40      continue 
        return
        end subroutine     

        end 

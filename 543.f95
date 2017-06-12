! Let us assume Goldbach conjecture true. In any case, it was proven numerically way beyond what we use here. 
! 
!       Every even number greater than 2 can be written as sum of two primes. 
!
! It then follows (proven, btw), that every odd number greater than 5 can be written as sum of three primes. 
! The even numbers are then easy, you subtract 2 and have conjecture for 2 primes and even number. 
! 
! How about sum of 4 primes. Every odd number above 7 is just adding 2 to above result. For every even, add 3 to above result. 
! 8 is trivial. 
!
! The proof is kind of formalized. 
!
! S(n) is then PrimePi(n) + PrimePi_2(n) + sum(1,{k,2*i,n},{i,3,n)) 
!
! Where PrimePi_2 counts how many numbers no grater than n can be written as sum of two primes. 
!
! The last term in, the sum is (n/2)*(n-n/2)+4-2 n, with appropriate quotient in place. 
!
! Answer 543 =    199007746081234640
!
! real    0m23.863s
! user    0m23.543s
! sys     0m0.104s
!
!
        program Eu543
        implicit none
        integer        , parameter   :: max_fib = 44
        integer(kind=1), allocatable :: numbers(:)
        integer                      :: fibs(0:max_fib),max_value
        integer                      :: prime_pi, prime_pi_2 
        integer                      :: i,j
        integer                      :: res(3,max_fib)

        fibs(0) = 0
        fibs(1) = 1 
        do i = 2 , max_fib
           fibs(i) = fibs(i-1)+fibs(i-2)
        enddo 
        max_value = fibs(max_fib) 
        allocate(numbers(max_value))
!
        call sieve(numbers,max_value)
! COMPUTE PRIME PI 
!       
        res = 0 
        prime_pi = 0 
        do j = 3 , max_fib
        do i = fibs(j-1)+1, fibs(j)
           if(numbers(i).eq.1) prime_pi = prime_pi + 1 
        enddo 
           res(1,j) = prime_pi
           res(2,j) = sum_term(fibs(j))
        enddo 
!
! HERE IT IS WHEN WE ARE USING THE GOLDBACH CONJECTURE. WE ASSUME ALL EVEN NUMBERS > 2 CAN BE WRITTEN AS SUM OF TWO 
! PRIMES. THEN THE ODD ONE, WE MUST HAVE N-2 IS PRIME. OTHERWISE, IT CANNOT BE WRITTEN LIKE THAT. 
!
        prime_pi = 0 
        do j = 3 , max_fib
        do i = max(4,fibs(j-1)+1) , fibs(j)
           if(mod(i,2).eq.0.or.numbers(i-2).eq.1) prime_pi = prime_pi + 1 
        enddo 
           res(3,j) = prime_pi
        enddo 

        
        j = 0 
        do i = 3 , max_fib
           j = j + res(1,i)+res(2,i)+res(3,i)
        enddo

! There is a off by 1 error from the summation function. It should give 0 for 2 but gives 1.
        print *, "Answer 543 = ", j-1
        
        deallocate(numbers)

        contains


        integer function sum_term(n)
        implicit none
        integer    :: n 
        sum_term = n/2*(n-n/2)+4-2*n
        return
        end function 

        subroutine sieve(res,maxval)
        implicit none 
        integer  :: maxval
        integer(kind=1)  :: res(maxval)
        integer  :: val, loop
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
        end subroutine     

        end 

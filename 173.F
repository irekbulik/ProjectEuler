#define NMAX    1000000
#define MAXCPU  2
        program eu173 
        implicit none
        integer  hsdes2
        integer  total 
        integer  totals(MAXCPU)
        integer  i
        integer  getcpu, setcpu
        integer  ncpu, icpu 
        integer  maxk, k

!*********************************************************************!
!********** LAME SOLUTION THAT WAS A LEARNING TOOL *******************!
!*********************************************************************!
        total = 0 
        ncpu  = setcpu()
        do 5 i = 1 , ncpu
5       totals(i) = 0

!$omp  parallel do if(ncpu.gt.1) schedule(dynamic,1) default(shared)
!$omp+ private (icpu)
        do 10 i = 2 , NMAX
          icpu = getcpu()
          totals(icpu) = totals(icpu) + hsdes2(i)
10      continue         
        do 11 i = 1 , ncpu 
11        total = total + totals(i)
        

        write(*,1800) total 

1800    format('solution to Euler(173) =',I12)
!
! NOT LAME SOLUTION
! FOR GIVEN LAYER THICKNESS K, AND THE INNER SQUARE M, WE USE 
! 4*K*(K+M) TILES. SO IF WE WANT TO HAVE LESS THAN T TILES, WE CAN
! AFFORD TO HAVE ALL THE M FOR GIVEN THICKNESS THAT ARE GREATER THAN 1
! AND SMALLER THAN 
!  
!                       T/(4*K) - K 
!
! GIVEN T TILES, THE MAXIMUM NUMBER OF LAYERS IS K^2 + K <= T
!
!                      Sqrt(T+1) - 1 / 2  
!
        total = 0 
        maxk  = floor((sqrt((NMAX+1)*1.0d0)-1)/2)
        do 20 k = 1 , maxk
20        total = total + NMAX/(4*k) - k

        write(*,1800) total 

        return
        end 

        integer function setcpu()
        implicit none 
#if defined (_OPENMP)
        integer  omp_get_max_threads
        setcpu = min(omp_get_max_threads(),MAXCPU)
        call omp_set_num_threads(setcpu)
#else 
        setcpu = 1 
#endif 
        return
        end 


        integer function getcpu()
        implicit none
#if defined (_OPENMP)
        integer  omp_get_thread_num 
        getcpu = omp_get_thread_num() + 1 
#else 
        getcpu = 1 
#endif 
        return
        end 









        integer function hsdes2(val0)
        implicit none
        integer  val0
        integer  val 
        integer  div, comdiv, lim, i, n, m, k
!
! SOLVES DIOPHANTINE TYPE EQUATION 
!
! N^2 - M^2 = VAL 
!
!
! THE ALGORITHM IS 
!
!
! THE APPROACH: 
! FIND PAIRS OF DIVISOR OF 4 VAL AND ITS COMPLEMENT SUCH THAT THEIR SUM IS MULTIPLE OF 4
! THAT VALUE IS 4 M.  
!
! N SHOULD BE THEN THE ( DIVISOR - 2 M )/2
! 
! THEN, DUE TO SYMMETRY OF THE PROBLEM WE CHECK THAT WHEN N IS ODD M IS
! ODD. IF THAT IS THE CASE, SOLUTION IS FOUND. FUNCTION RETURNS NUMBER
! OF SOLUTIONS FOUND.
!
      hsdes2 = 0 
      val    = 4 * val0
!
! WE LOOK TO FIND DIVISORS FOR 4 * V
!
! 
      lim = floor(sqrt(val*1.0d0)) 
!
      do 10 i = 2 , lim
       if(i*i.gt.val) goto 10 
       if(mod(val,i).eq.0) then 
! BECAUSE WE LOOK FOR SOLUTIONS THAT ARE POSITIVE
! WE NEED DIV TO BE BIGGER THAN COMDIV
          div = val/i
       comdiv = i 
          if(mod(div+comdiv,4).eq.0) then 
          n = (div + comdiv)/4
          k = div - 2 * n
            if(mod(k,2).eq.0.and.k.gt.0) then 
            m = k/2 
! CHECK IF WE UPDATE THE VALUE OF NOT 
              if(n*n-m*m.ne.val/4) then 
                print *, 'epic fail'
                stop
              endif 
              if(mod(n,2).eq.mod(m,2)) hsdes2 = hsdes2 + 1 
            endif
          endif
        endif 
10     continue   
!
       return
       end
!





![0] gfortran -fdefault-real-8 -fdefault-double-8 -fdefault-integer-8 -O3 -ffast-math -funroll-all-loops -fopenmp 589.F -o 589 ; time ./589
!Answer 589 =     131776959.25
!
!real    0m1.074s
!user    0m4.078s
!sys     0m0.006s

        program Eu589
#define _MAX_ 200
#define MAXCPU 4
        implicit none
        real*8   exptim
        real*8   suma(MAXCPU)
        real*8   arr((_MAX_+1)*(_MAX_+2),MAXCPU)
        integer  n,m
        integer  getcpu,ncpu,setcpu,icpu
        
        ncpu = setcpu()
        if(ncpu.gt.MAXCPU) stop('too many cpu')
        do n = 1 , ncpu 
           suma(n) = 0.0d0
        enddo 

!$omp   parallel do if(ncpu.gt.1) default(shared) schedule(dynamic,1) 
!$omp+  private(icpu,n)
        do m = 2, 100
           icpu = getcpu()
        do n = 1, m-1 
        suma(icpu) = suma(icpu) +  exptim(5,n,m,arr(1,icpu))
        enddo
        enddo
        do n = 2 , ncpu
           suma(1) = suma(1) + suma(n)
        enddo

        write(*,1800), suma(1)
        
1800    format("Answer 589 = ", F16.2)


        return
        end 
    
        real*8 function exptim(o,n,m,arr)
        implicit none
        integer  o,n,m
        real*8   arr(0:m+o,-1:m+o)
        real*8   solve0
! o: time to rest
! n: minimal time 
! m: maximal time         
        integer  i,j,k,nd 
        real*8   one
        data     one/1.0d0/
        real*8   p,pwin
        integer  maxdel
        p = one/(m-n+1)
! maximum delta is m + o         
        maxdel = m + o 
        call aclear((1+maxdel)*(2+maxdel),arr)
        
! run over possible delta is n: 
        do 100 i = 0, maxdel 
! instant win t<delta-offset. that means that from the previous case, we
! established delta after the previous race. now before the other
! finish, we win. 
           pwin = 0.0d0
           do 200 j = n,i-o-1
           pwin = pwin + p 
200        arr(i,-1) = arr(i,-1) + p*j
! did not win instantly. both swimmers will make another lap. that 
! is just the time needed to get fished and be returned to the bridge. 
! set new races. 
           do 300 j = max(n,i-o),m 
              arr(i,-1) = arr(i,-1) + p * o
           do 300 k = n,m          
! update the delta. If it becomes below 0, we switch the winner.
              nd = i +  k - j 
              if(nd.lt.0) then 
              nd = -nd 
! correct for the faster guy. We switch the winner and add the new
! winner time. Here clearly k is faster so add k and then be correct.
! looser becomes winner and time of looser is delta + t winner 
              arr(i,-1) = arr(i,-1) + p*p*(k-j+i)
              endif 
              arr(i,nd) = arr(i,nd) + p*p 
              arr(i,-1) = arr(i,-1) + p*p*j
300        continue 
!           
100       continue 

        exptim = solve0(arr,maxdel)
        return
        end 


        real*8 function solve0(arr,n)
! SOLVE SYSTEM OF EQUATIONS FOR THE FIRST ENTRY. 
        implicit none 
        integer  n 
        real*8   arr(0:n,-1:n) 
        real*8   coe 
        integer  i,j,k
!
       coe = arr(n,n)
        if(abs(coe-1.0d0).lt.1.d-5) stop('cannot solve')
           do 40 j = -1 , n - 1 
40         arr(n,j) = arr(n,j)/(1.0d0-coe)            

        do 10 k = n  , 1 , -1 
        do 10 i = k-1, 0 , -1 
           coe = arr(i,k) 
           do 20 j = -1 , k - 1   
20         arr(i,j) = arr(i,j) + coe * arr(k,j) 
           coe = arr(i,i) 
           if(abs(coe-1.0d0).lt.1.d-5) stop('cannot solve')
           do 30 j = -1 , k - 1 
30         arr(i,j) = arr(i,j)/(1.0d0-coe)            
           arr(i,i) = 0.0d0
10       continue

        solve0 = arr(0,-1)         

        return
        end           


        subroutine aclear(n,a) 
        implicit none
        integer  n , i 
        real*8   a(*)
        do 1 i = 1 , n 
1        a(i) = 0.0d0
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

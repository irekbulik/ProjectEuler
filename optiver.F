! PROGRAM TO SOLVE THE OPTIVER QUESTION FOR 
!  I INTERSECTIONS WITH J WAND USAGES AND 
!  M SECONDS CYCLE (M GREEN, M RED)
!
! WE FOUND IT ON THEIR WEBSITE
!
#define MAXI 10
#define MAXJ 10
        program intopt
        implicit none
        real*8     wt(MAXI,MAXJ+1)
        common     /waitt/wt
        external   getwt
        real*8     res 
        integer    i,j
! SET TO SOME NEGATIVE NUMBER LOWER THAN -1
        call aset(MAXI*(MAXJ+1),wt,-1.D0)       
!
        call getwt(res,10,9,40,getwt)
         print *, res
        call getwt(res,10,9,40,getwt)
         print *,res
!
        do i = 0 , 10
        call getwt(res,10,i,40,getwt)
        enddo

        call prt(wt)
!
        return
        end 

        subroutine getwt(res,i,j,m,recur)
        implicit none
        real*8     res 
        integer    i,j,m
        real*8     wt(MAXI,0:MAXJ)
        common     /waitt/wt
        external   recur
! WAIT PER ONE 
        real*8     waitp1
        real*8     tm0,tmm,weight       
        real*8     curwt
!
! SANITY CHECK 
!
        if(i.lt.1.or.j.lt.0) then 
          write(*,*) 'incorrect input in getwt'
          stop
        endif 
!
! WAIT PER ONE INTERSECTION, EXPECTED 
! 
!  W = 1/2 * 0 + 1/2 * ( SUM(i/M) i , (i=1,M) ) 
!
!  = 1/4 * (M+1)
!
! LET US MAKE CONTINUOUS TIME APPROXIMATION 
!
        waitp1 = real(m)/real(4)
!
! IF WE SOLVED IT ALREADY, RETURN 
!
        if(wt(i,j).gt.-1.0d0) then
          res = wt(i,j)
        else
! IF WE HAVE MORE OR EQUAL USAGES OF WAND,THE RESULT IS NO WAIT TIME 
          if(j.ge.i) then 
            res = 0.0d0
! IF WE HAVE NO WAND USAGE, THE WAIT IS THE NUMBER OF INTERSECTIONS
! TIMES THE EXPECTED WAIT OF THAT INTERSECTION 
          elseif(j.eq.0) then 
            res = i * waitp1
          else 
!
! SEE IF WE USE THE WAND OR NOT. IF WE USE THE WAND, THEN THE WAIT TIME
! WILL BE T(I-1,J-1). OTHERWISE, IT WILL BE CURRENT WAIT TIME + T(I-1,J)
!
! OBTAIN T(I-1,J-1) 
          call recur(tmm,i-1,j-1,m,recur)
! OBTAIN T(I-1,J) 
          call recur(tm0,i-1,j  ,m,recur)
! THE CURRENT WAIT TIME HAS TO BE GREATER THAN T(I-1,J-1)-T(I-1,J) 
! TO JUSTIFY THE USE OF THE WAND. WE ASSUME WE CAN SEE EVERY SECOND
! ONLY. WE WILL WORK WITH CURWT BEING NOT TO USE THE WANT 
          curwt = tmm - tm0
        
!          print *, 'at ',i,' intersections with ',j,' wands',
!     $             'wait time',curwt

             if(int(curwt)+1.gt.m.or.int(curwt).lt.0) then 
              write(*,*) 'incorrect in getwt'
              stop      
             endif 
! PROBABILITY OF EVENT THAT CURWT < T(I-1,J-1) - T(I-1,J) 
! WHICH MAKES US NOT USE THE WAND 
            weight = curwt/real(2*m)
! WE NEED TO AVERAGE THE CURRENT WAITING TIME OVER ALL POSSILBLE ONES
! BELOW CUTOFF => 1/CURWT SUM{K,{K,1,CURWT}}        
            res = weight*(curwt/2 + tm0) + (real(1)-weight) * tmm

          endif 

          wt(i,j) = res 

        endif 

        return
        end 


        subroutine prt(w)
        real*8     w(MAXI,0:MAXJ)
        
         do i = 1 , 9
         write(*,1900) (max(w(i,j),0.0d0),j=0,9)
         enddo

1900    format(10(F9.4,2x))
        
        return
        end

        subroutine aset(n,a,val)
        implicit none
        integer  i,n
        real*8   a(*),val
        
            do 10 i = 1 , n 
10          a(i) = val
        return
        end            

         



  

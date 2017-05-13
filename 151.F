!
! Well, that is disappointingly simple problem that can be brute forced
! without memoization in no time. 
!
!
!Answer 151: 0.464399
!
!real    0m0.007s
!user    0m0.003s
!sys     0m0.003s

        program eu151 
        implicit none 
        external pick
!There are just a2,a3,a4 and a5 possibilities after day 1, which we
!discard. 
        integer  inish(4)
        real*8   prob 
        real*8   expt 
        
        inish(1)=1
        inish(2)=1
        inish(3)=1
        inish(4)=1
        expt    =0.0d0 
        prob    =1.0d0

        call pick(pick,inish,prob,expt)

        write(*,1800) expt

1800    format("Answer 151: ",F8.6)

        return
        end 


        subroutine pick(routine,sheet,prob,expt)
        implicit none
        external routine 
        integer  sheet(4)
        real*8   prob,expt
! Locals 
        integer  cursht(4)
        real*8   curp,curexp
        integer  tot
        integer  ipick,k
!
! IF THERE IS ONLY A5 LEFT, YOU CAN RETURN WITH NO UPDATE        
!
        if(sheet(1).eq.0.and.sheet(2).eq.0.and.sheet(3).eq.0) 
     $  return         
!
        tot = sheet(1)+sheet(2)+sheet(3)+sheet(4) 
!
!

        do ipick = 1 , 4 
         if(sheet(ipick).gt.0) then 
        curexp = 0.0d0
        curp   = prob * real(sheet(ipick))/real(tot) 
         do k = 1 , 4  
          cursht(k) = sheet(k)
         enddo 
! Make Cuts 
           if(ipick.eq.1)     then 
           cursht(1) = cursht(1)-1 
           cursht(2) = cursht(2)+1
           cursht(3) = cursht(3)+1
           cursht(4) = cursht(4)+1
           elseif(ipick.eq.2) then 
           cursht(2) = cursht(2)-1
           cursht(3) = cursht(3)+1
           cursht(4) = cursht(4)+1
           elseif(ipick.eq.3) then 
           cursht(3) = cursht(3)-1
           cursht(4) = cursht(4)+1
           else
           cursht(4) = cursht(4)-1 
           endif 
         call routine(routine,cursht,curp,curexp)
         expt = expt + curexp 
         endif 
        enddo 
!
! IF THERE IS ONLY ONE SHEET (THIS IS NOT A5, AS WE VERIFY ABOVE, 
! UPDATE THE EXPECTED VALUE AND WHAT WE CALLED ED BEFORE. 
!
        if(tot.eq.1) then
          expt = expt + prob 
        endif 

        return
        end 


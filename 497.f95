!
! [0] gfortran -std=f2008 -O3 497.f95 -o 497 ; time ./497
! Answer 497 =             684901360
!
!real    0m0.841s
!user    0m0.835s
!sys     0m0.004s

! we need to solve only the base cases, then the rest we solve by magic of dynamic programming as we sum over n. 
!
! we always need to move a to c, starting from b. that b is unneeded difficulty, which we can add at the end. we
! can now assume we start at the one where the tower is. 
! 
! then, we must move n-1 tower from a to b, then move from b to a, then move from a to c, then move from c to b, then 
! move the n-1 tower from b to c. all we have to do is to unroll recursion.
!
        program test
        implicit none  
        integer,parameter      :: int_kind=selected_int_kind(18)
        integer,parameter      :: max_tiles=10000
        integer(kind=int_kind),parameter :: mod_val=10**9
!
        integer(kind=int_kind) :: move_AtoB(max_tiles),move_BtoA(max_tiles)
        integer(kind=int_kind) :: move_BtoC(max_tiles),move_CtoB(max_tiles)
        integer(kind=int_kind) :: move_AtoC(max_tiles),move_CtoA(max_tiles)
        integer(kind=int_kind) :: e_AtoB,e_BtoA,e_AtoC,e_CtoA,e_BtoC,e_CtoB
        integer(kind=int_kind) :: a,b,c,siz,e,i_tile,n
        integer(kind=int_kind) :: answer
!
        answer = 0 
! rather brute force but that is ok 
        do n = 1 , 10000

        siz = 10
        a   = 3
        b   = 6 
        c   = 9
        e   = n 

        e_AtoB = expected_moves(a,b,siz,e) 
        e_BtoA = expected_moves(b,a,siz,e) 
        
        e_AtoC = expected_moves(a,c,siz,e) 
        e_CtoA = expected_moves(c,a,siz,e) 

        e_BtoC = expected_moves(b,c,siz,e) 
        e_CtoB = expected_moves(c,b,siz,e) 
         

        move_AtoB(1) = e_AtoB         
        move_BtoA(1) = e_BtoA
        move_AtoC(1) = e_AtoC
        move_CtoA(1) = e_CtoA
        move_BtoC(1) = e_BtoC
        move_CtoB(1) = e_CtoB
!
        do i_tile = 2 , n
! move A to B.  
        call move(i_tile,move_AtoB,move_AtoC,move_CtoB,e_AtoB,e_BtoC,e_CtoA) 
! move B to A.  
        call move(i_tile,move_BtoA,move_BtoC,move_CtoA,e_BtoA,e_AtoC,e_CtoB) 
! move A to C 
        call move(i_tile,move_AtoC,move_AtoB,move_BtoC,e_AtoC,e_CtoB,e_BtoA) 
! move C to A 
        call move(i_tile,move_CtoA,move_CtoB,move_BtoA,e_CtoA,e_AtoB,e_BtoC) 
! move B to C 
        call move(i_tile,move_BtoC,move_BtoA,move_AtoC,e_BtoC,e_CtoA,e_AtoB) 
! move C to B
        call move(i_tile,move_CtoB,move_CtoA,move_AtoB,e_CtoB,e_BtoA,e_AtoC) 
        enddo 
        answer =  mod(answer+move_AtoC(n)+e_BtoA,mod_val)
        enddo 
        print *, 'Answer 497 = ', answer
        contains 

        pure subroutine move(i_tile,e_s2t,e_s2a,e_a2t,s2t,t2a,a2s) 
        implicit none
        integer(kind=int_kind),intent(in)    :: i_tile
        integer(kind=int_kind),intent(inout) :: e_s2t(:),e_s2a(:),e_a2t(:)
        integer(kind=int_kind),intent(in)    :: a2s,s2t,t2a
        e_s2t(i_tile) = mod(e_s2a(i_tile-1) + a2s + s2t + t2a + e_a2t(i_tile-1),mod_val)
        return
        end 

        function expected_moves(ini,fin,siz,e) result(moves) 
        implicit none 
        integer(kind=int_kind), intent(in)  :: ini,fin,siz,e
        integer(kind=int_kind)              :: moves  
! simple things: lets us move randomly to the right. clearly, the move from tile 
!        (n-1) --> n takes 2n-3 steps. 
! here we have lowest n of 2. Then the move from 1 to n takes (n-1)^2 steps. 
! those should be inverter to move to the left, as we always start from the edges. 
! determine if we move to the left or to the right. the expected values 
            moves = 0 
            if(ini.lt.fin) then 
               moves =(mod_exp(fin,e,mod_val)**2-2*mod_exp(fin,e,mod_val)+1)&
                     -(mod_exp(ini,e,mod_val)**2-2*mod_exp(ini,e,mod_val)+1)
            else 
               moves = mod(2*mod_exp(siz,e,mod_val)-mod_exp(fin,e,mod_val)-mod_exp(ini,e,mod_val),mod_val)&
                     * mod(mod_exp(ini,e,mod_val)-mod_exp(fin,e,mod_val),mod_val)
            endif 
            moves=mod(moves,mod_val)
            if(moves.lt.0) moves = moves+mod_val
         return
         end 
 
        function mod_exp(b,p,m) result(res)
        integer(kind=int_kind),intent(in)    :: m,p,b
        integer(kind=int_kind)               :: lp,lb
        integer(kind=int_kind)               :: res 
        integer(kind=int_kind),parameter     :: two=2

        res = 1
        lp  = p 
        lb  = b 
100     continue
        do while(lp.gt.0) 
        if(mod(lp,two).eq.1) res = mod(res*lb,m)
           lb = mod(lb*lb,m)
           lp = lp / 2
        enddo
        res = mod(res,m)
        return
        end

        

        end 

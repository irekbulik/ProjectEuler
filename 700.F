! We know that the last coin is 0 for n = m because seed and m are
! coprime. We can look at the big coins first, then we just check
! explicitly whether a given coin is possible
! [0] gfortran -fdefault-integer-8 -O3 700.F -o 700 ; time ./700
! Answer 700 =      1517926517777556
!
!real	0m0.406s
!user	0m0.398s
!sys	0m0.003s

       program eu700
       implicit none
       integer seed, m, m_inv
       integer val, l_val, l_coin, coin, total
       integer i
       parameter (seed  = 1504170715041707, m = 4503599627370517,
     $            m_inv = 3451657199285664)
 
       coin  = seed + 1 
       total = 0
       val   = 0
       do i = 1,  10**8 ! will avoid overflow for l_coin * m_inv
          val = mod( val + seed, m )
          if( val .lt. coin ) then
              coin = val
              total = total + coin
          endif
       enddo
       l_coin = coin
       l_val  = m
       do coin = 1, l_coin - 1
          val = mod(coin * m_inv, m)
          if( val .lt. l_val) then
              l_val = val
              total = total + coin
          endif
       enddo
       print*, "Answer 700 = ", total 
       return
       end 

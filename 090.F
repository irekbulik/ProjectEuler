       program Eu090
       implicit none
       integer   die1(6),die2(6)
       integer   id1,id2
       logical   is_all 
       integer   binom 
       integer   tot 

        tot = 0 

        do id1 = 1 , binom(10,6)
        call choose(id1,10,6,die1)
        do id2 = 1 , binom(10,6)
        call choose(id2,10,6,die2)
           if(is_all(die1,die2)) then 
           tot = tot + 1 
           endif 

        enddo
        enddo 
! Dies are indistinguishable
        print *, tot/2

1800   format(6(i1),2x,6(i1))
       return
       end 



      logical function is_all(d1,d2)
      implicit none
      integer  d1(6),d2(6)
      logical  is_possible
      
      is_all = is_possible(d1,d2,0,1)
      is_all = is_possible(d1,d2,0,4).and.is_all
      is_all = (is_possible(d1,d2,0,9).or.is_possible(d1,d2,0,6))
     $         .and.is_all
      is_all = (is_possible(d1,d2,1,6).or.is_possible(d1,d2,1,9))
     $         .and.is_all
      is_all = is_possible(d1,d2,2,5).and.is_all
      is_all = (is_possible(d1,d2,3,6).or.is_possible(d1,d2,3,9))
     $         .and.is_all
      is_all = (is_possible(d1,d2,4,9).or.is_possible(d1,d2,4,6))
     $         .and.is_all
      is_all = (is_possible(d1,d2,6,4).or.is_possible(d1,d2,9,4))
     $         .and.is_all
      is_all = is_possible(d1,d2,8,1).and.is_all

      return
      end 



      logical function is_possible(d1,d2,v1,v2)
      implicit none
      integer  d1(6),d2(6),v1,v2
      integer  i 
      logical  v1ind1,v1ind2,v2ind1,v2ind2 
      
      v1ind1 = .false.       
      v2ind1 = .false.       
      v1ind2 = .false.       
      v2ind2 = .false.       

      do i = 1 , 6 
         v1ind1 = v1ind1.or.d1(i).eq.v1
         v1ind2 = v1ind2.or.d2(i).eq.v1
         v2ind1 = v2ind1.or.d1(i).eq.v2
         v2ind2 = v2ind2.or.d2(i).eq.v2
      enddo 
        
      is_possible = v1ind1.and.v2ind2 
      is_possible = v1ind2.and.v2ind1.or.is_possible 

      return
      end 




      Integer Function BINOM(M,N)
!          ( M )
! COMPUTES (   ) returns 0 if n > m 
!          ( N ) 
!
      Implicit None
      Integer  M, N
      Integer  P,I,N1,R
      integer  memo(0:40,0:40)
      logical  ini
      save     ini,memo
      data     ini/.true./
      
      IF(INI) THEN              
        DO 1 , P = 0 , 40
        DO 1 , I = 0 , 40
1         MEMO(I,P) = 0  
        INI = .FALSE.
      ENDIF 
!      If(M.lt.0.or.N.lt.0.or.n.gt.m) THEN
!        STOP("Incorrect Input For Binom")
!      EndIF
!
      if(MEMO(M,N).ne.0) then
        binom = MEMO(M,N)
      else
      N1 = N
      P  = M - N1
      If(N1.ge.P) goto 10
      P  = N1
      N1 = M - P
10    R = N1  + 1
      If(P.eq.0) R = 1
      If(P.LT.2) goto 30
      Do 20 I = 2 , P
      R = (R*(N1+I))/I
20    Continue
30    Binom = R
      MEMO(M,N) = BINOM
      endif
      Return
      End
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
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
        do ip = 1 , p 
           r(ip) = r(ip)-1
        enddo
        return
        end 
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!

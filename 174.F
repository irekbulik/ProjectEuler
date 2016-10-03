!              I = J + 2 K  
!      +--------------+
!      |       J    K |
!      |   +------+   |
!      |   |      |   |
!      |   |      |   |
!      |   +------+   |
!      |              |
!      +--------------+
! NUMBER OF TILES T = I^2-J^2 = 4K**2+2JK
! FOR GIVEN THICKNESS, WE CAN MAKE LAMINAE 
!      
!     J <= (T/4-K**2)/K
!
! WITH THE 10^6 TILES, THE LARGEST VALUE OF J WE CAN ATTAIN 
! IS 249 999.  FOR GIVEN POSSIBLE J WE LOOP INCREASING K UNTIL WE OUT OF
! TILE AND UPDATE THE TOTAL NUMBER GIVEN VALUE OF TILES WAS VISITED.
! QUITE LAME, BUT RUNS FAST 
!
!
!Answer 174 =  209566
!
!real    0m0.026s
!user    0m0.020s
!sys     0m0.006s
!
! WITH NMAX = 10**8 
!Answer 174 = 18898249
!
!real    0m3.618s
!user    0m3.522s
!sys     0m0.066s
!
#define NMAX 1000000
        program e174
        implicit none
        integer  t(NMAX)
        integer  ij,ik,it
        
        do 10 ij = 1 , NMAX
10        t(ij) = 0

        do 100 ij = 1 , NMAX/4-1
!        
           ik = 0 
!
200        ik = ik + 1 
                
           it = 4 * ik * ( ik + ij )
           if(it.lt.NMAX) then 
           t(it) = t(it) + 1 
           goto 200 
           else 
           goto 100
           endif 

100        continue


         ik = 0 
         ij = 0 

         do 300 it = 1 , NMAX
300        if(t(it).ge.1.and.t(it).le.10) ik = ik + 1 
         
         write(*,1800) ik
         
1800     format("Answer 174 = ", I8)

         return 
         end 

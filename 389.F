#define s_t 4
#define s_c 6 
#define s_o 8
#define s_d 12 
#define s_i 20
!Answer 389 =2406376.3623
!
!real    0m1.610s
!user    0m1.603s
!sys     0m0.003s
        program eu389
        implicit none
        real*8  t(s_t)
        real*8  c(s_t*s_c)
        real*8  o(s_t*s_c*s_o)
        real*8  d(s_t*s_c*s_o*s_d)
        real*8  i(s_t*s_c*s_o*s_d*s_i)
        real*8  w(s_t*s_c*s_o*s_d*s_i)
        real*8  v(s_t*s_c*s_o*s_d*s_i)
        integer j
        real*8  mean,var

         call set(t,s_t,1.0d0/s_t)
!
         call update(s_t            ,t,c,s_c,w,v)
         call update(s_t*s_c        ,c,o,s_o,w,v)
         call update(s_t*s_c*s_o    ,o,d,s_d,w,v)
         call update(s_t*s_c*s_o*s_d,d,i,s_i,w,v)

         mean = 0.0d0 
         do j = 1, s_t*s_c*s_o*s_d*s_i
            mean = mean + i(j)*j
         enddo         
         var = 0.0d0
         do j = 1, s_t*s_c*s_o*s_d*s_i
            var = var + i(j)*(1.0d0*j-mean)**2
         enddo         

        write(*,1800) var
1800    format("Answer 389 =",F12.4)

        return
        end 
        

        subroutine update(np,vp,vc,sc,nm1,nm0)
        implicit none 
        integer  np,nc,sc
        real*8   vp(*),vc(*),nm1(*),nm0(*)
        real*8   pc 
        integer  ipt,iv,ipv,tm1

        pc   = 1.0d0/sc
        nc   = sc*np
        tm1  = 0
        call set(vc,nc,0.0d0)

        do 100 ipt = 1 , np 
!
! FOR EACH NUMBER OF THROWS BEFORE WITH GIVEN DIE, WE ADD CURRENT THROW          
           if(ipt.eq.1) then 
           do 200 iv  = 1 , sc 
200           nm0(iv) = pc 
           else 
           do 210 iv  = 1 , sc 
           do 210 ipv = 1 , tm1 
210          nm0(iv+ipv) = nm0(iv+ipv)+nm1(ipv)*pc 
! nm0 now has the probabilities for the case of ipt throws. 
! we update the output array 
           endif 
           call adds(nc,vc,nm0,vp(ipt))
           call amove(nc,nm1,nm0)       
           call set(nm0,nc,0.0d0)
           tm1 = tm1 + sc
100     continue        

        return
        end 

        subroutine amove(n,a,b)
        implicit none
        integer  n,i
        real*8   a(*),b(*)
        do 10 i = 1 , n 
 10     a(i) = b(i)
        return
        end 

        subroutine adds(n,a,b,s)
        implicit none 
        real*8  a(*),b(*),s 
        integer n,i
        do 10 i = 1 , n
10         a(i) = a(i) + s*b(i) 
        return
        end 

        subroutine set(a,n,v)
        implicit none 
        real*8  a(*),v 
        integer n,i
        do 10 i = 1 , n
10         a(i) = v
        return
        end 

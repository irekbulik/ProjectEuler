!Answer 286 =   52.6494571953
!
!real    0m0.009s
!user    0m0.006s
!sys     0m0.003s
        program Eu286
        implicit none
        integer  i
        real*8   p 
        real*8   ql,qu,qt,pt

         ql = 50.01d0
         qu = 100.0d0 

100      qt = (ql + qu)/2
         pt = p(qt)
         if(pt.gt.0.02d0) then 
            ql = qt 
         else 
            qu = qt 
         endif 
            if(abs(qu-ql).gt.1d-14) goto 100
         write(*,1800) qt 
1800     format("Answer 286 =", F16.10)          

         return
         end 
          
         



        real*8 function p(q)
        implicit none
        integer  i,j,n
        real*8   pa(0:20,0:50) 
        real*8   q 
        real*8   pdnn
        real*8   one,zero
        save     one,zero
        data     one/1.0d0/,zero/0.0d0/  
!
        call aclear(51*21,pa)
!
! TAKE CARE OF M=0 CASE 
           pa(0,0) = one
        do i = 1 , 50
           pa(i,0) = zero 
        enddo 
! TAKE CARE OF N=0 CASE 
           pa(0,1) = 1/q
        do i = 2 , 50
           pa(0,i) = pa(0,i-1)*i/q
        enddo 
        
        do n = 1 , 20 
        do i = 1 , 50 
           pa(n,i) = pa(n,i) + pa(n-1,i-1)*(one-i/q) 
     $             +           pa(n  ,i-1)*i/q  
        enddo         
        enddo   

        p = pa(20,50)


        return
        end 

        subroutine aclear(n,a)
        implicit none
        integer  n,i
        real*8   a(*)           
        do 10 i = 1 , n
10      a(i) = 0.0d0
        return
        end 

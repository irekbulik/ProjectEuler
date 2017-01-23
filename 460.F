#define _NX_ 10000
#define _NY_ 5000
#define __BIG__ 1.0D20
!
!Anwer 460 =     18.420738199
!
!real    1m9.554s
!user    1m8.882s
!sys     0m0.173s
!
! THE PROGRAM ALSO PRINTS THE PATH. YOU CAN SEE THAT IT IS LIKE A
! CIRCLE. THAT IS HOW THE GRID WAS OPTIMIZED. WALL, WE CAN MAKE PRUNING
! BETTER BUT I DON'T THINK IT IS SO COOL. 
!
        program test
        implicit none
        integer path(2,0:_NX_,_NY_)
        real*8  t(0:_NX_,_NY_)
        real*8  tmp(_NY_)
        real*8  gtime,time,mint
        integer ix,iy,jx,jy
        integer minx,miny
        integer nx,ny
        integer apiy,apjy
        integer limiy,limjy
        integer tol,toly

        
        nx = _NX_       
        ny = _NY_
        tol = 70
        toly = 40
 
!        tol = 70
!        toly = 40
!
        do iy = 1 , ny 
        path(1,nx,iy) = nx
        path(2,nx,iy) = 1
        enddo
        do 2 ix = 0 , nx
        do 2 iy = 1 , ny
2        t(ix,iy) = __BIG__
!
! FILL THE TIME NEEDED TO ARRIVE AT THE POINT (NX,1) FROM 
! LAST COLUMN. 
!
        do 1 iy = 1 , ny
1        t(nx,iy) = gtime(nx,iy,nx,1)
!
! Here, we enter the main loop. 

        do 100 ix = nx - 1 , nx/2 , -1         
!
! COMPUTE THE EXPECTED Y POSITION 
!
        apiy = int(sqrt(Real(NY*NY-(ix-ny)**2)))
!
! From here, we should do the following. We can either move forward or
! move in vertical direction. 
           do 10 iy = max(1,apiy-toly), min(ny,apiy+toly)
           mint = __BIG__
           minx = ix + 1 
           miny = iy
           do 20 jx = ix + 1 , min(nx,ix+tol)
              apjy = int(sqrt(Real(ny*ny-(jx-ny)**2)))
           do 21 jy = max(1,apjy-toly), min(iy,apjy+toly)
              time = t(jx,jy) + gtime(ix,iy,jx,jy)
              if(time.lt.mint) then
                mint = time 
                minx = jx
                miny = jy
              endif    
21          continue 
20       continue     
!
         path(1,ix,iy) = minx
         path(2,ix,iy) = miny
10       t(ix,iy) = mint                
!
! Now, we need to check whether we move up/down in the coulmn             
! for that, we need a copy of current coulmn         
          do 30 jy =  1,ny
30          tmp(jy) = t(ix,jy)
!
! Now, we check if we move within given column: 
!
          do 40 iy = max(1,apiy-toly), min(ny,apiy+toly)
!          do 40 iy = 1, ny
             mint = t(ix,iy)
             miny = iy 
          do 50 jy = iy - 1 , max(1,iy-toly),-1
!          do 50 jy = iy - 1 , 1,-1
             if(gtime(ix,iy,ix,jy)+tmp(jy).lt.mint) then
                miny = jy 
                mint = gtime(ix,iy,ix,jy)
             endif 
50        continue 
             t(ix,iy) = mint  
             if(iy.ne.miny) path(2,ix,iy) = miny
40        continue 


100      continue 
#if 1
        print *, '===========path============='
        jx = nx/2
        jy = ny
200     continue
        ix = path(1,jx,jy)
        iy = path(2,jx,jy) 
        print *, 'from',jx,jy,'to',ix,iy
        if(ix.eq.nx.and.iy.eq.1) goto 300
        jx = ix
        jy = iy
        goto 200
300     continue 
#endif
        write(*,1800) 2*t(nx/2,ny)        



1800   format('Anwer 460 = ',F16.9)

        return
        end 






        real*8 function gtime(x1,y1,x2,y2)
        implicit none
        integer  x1,y1,x2,y2
        real*8   rx1,rx2,ry1,ry2
        real*8   dist
        real*8   sped 

        rx1 = real(x1)
        rx2 = real(x2)
        ry1 = real(y1)
        ry2 = real(y2)

        if(y1.eq.y2) then
           sped =  ry1 
        else
           sped = (ry2-ry1)/Log(ry2/ry1)
        endif 
        
        dist = (rx1-rx2)**2+(ry1-ry2)**2
        dist = sqrt(dist)
        gtime = dist/sped 
        return
        end 

        subroutine amove(n,a,b)
        implicit none
        real*8   a(*),b(*)
        integer  i,n

        do 10 i = 1, n 
10      b(i) = a(i)
        return  
        end         

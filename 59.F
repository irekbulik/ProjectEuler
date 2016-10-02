#define MAXSIZE 1201
        program test
        implicit none 
        integer   cipher(MAXSIZE)
        integer   key(3)
        character text(MAXSIZE)
        character char
        integer   iachar
        logical   found
        integer   i,j,k,l
        integer   suma

        open (file='p059',unit=11,status='old')
        do i = 1 , MAXSIZE
        read(11,*), cipher(i)
        enddo
        close(11)

!         cipher(1) = xor(iachar('u'),29)
!         cipher(2) = xor(iachar('l'),29)
!         cipher(3) = xor(iachar('e'),29)
!         cipher(4) = xor(iachar('r'),29)
!
!         print *, (char(cipher(i)),i=1,4)
!               
!         cipher(1) = xor(cipher(1),29)
!         cipher(2) = xor(cipher(2),29)
!         cipher(3) = xor(cipher(3),29)
!         cipher(4) = xor(cipher(4),29)
!
!         print *, (char(cipher(i)),i=1,4)

         do i = 97 , 26 + 96
          key(1) = i 
         do j = 97 , 26 + 96
          key(2) = j
         do k = 97 , 26 + 96
          key(3) = k 

          do l = 1 , MAXSIZE 
          text(l) = char(xor(cipher(l),key(mod(l-1,3)+1)))
          enddo

          call scant(text,found)
          if(found) then
            print *, 'key ', char(key(1)),char(key(2)),char(key(3))
            write(*,1800) (text(l),l=1,MAXSIZE)
          endif 

         enddo
         enddo
         enddo 

        key(1) = ichar('g')
        key(2) = ichar('o')
        key(3) = ichar('d')

        suma = 0         

        do l = 1 , MAXSIZE
          text(l) = char(xor(cipher(l),key(mod(l-1,3)+1)))
          suma    = suma + ichar(text(l))
        enddo  
            write(*,1800) (text(l),l=1,MAXSIZE) 
            print *, suma



1800    format(100(A))

        return
        end 

        subroutine scant(text,found)
        implicit none
        integer    loop 
        logical    found
        character  text(*)
        character  the(4)
        logical    comp

        the(1) = 'a'
        the(2) = 'n'
        the(3) = 'd'
!        the(4) = 't'
        
        do loop = 1 , MAXSIZE - 4
           found = comp(3,the,text(loop))
           if(found) goto 100 
        enddo 

100     continue


        return
        end 



        logical function comp(n,st1,st2)
        implicit none
        integer n
        character st1(*), st2(*)        
        integer i
          comp = .true.
          do i = 1 , n 
           comp = comp.and.st1(i).eq.st2(i)
          enddo
        return
        end 
        

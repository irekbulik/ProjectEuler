#define __limit__ 10**7
#define __limit2__ 10**6
#define __maxdig__ 20 
!
! Output: takes ~5 min. I hated it. Probally, it makes sense to prune
! overlays of search. I did not like the problem much tho.
!
!       3203324994356        3156845497032          46479497324
        program Eu425 
        implicit none
        integer  nums(__limit__)
        integer  lev(__limit2__)
        integer  limit,limit2
        integer  add,ip,inum    
        integer  nprim,lmax,newmax
! OK, THE PRIME IS EITHER CONNECTED DIRECTLY TO TWO, OR IT IS 

        limit  = __limit__
        limit2 = __limit2__ 
        call sieve(nums,limit)
        nprim = 0 
        do 20 ip = 2 , limit
20        if(nums(ip).eq.1) nprim =  nprim + ip
       
        nums(2) = 2 
!
! WE ARE GOING TO LOOP LEVELS OF CONNECTNESS TO TWO. WE END WHERE NO
! NEED NUMBER IS ADDED 
        do 100 inum = 2 , limit 
        lev(1) = inum
        lmax   = 1 
           if(nums(inum).lt.1) goto 100 
400     continue 
        print *, inum,lmax
! WELL, HERE WE NEED TO GO WHEN WE DID NOT FIND THE CORRECT MERGE
        add  = 0 
        do 300 ip = 1 , lmax
        call addcon(lev(lmax+1),limit2-lmax,add,lev(ip),nums,limit,
     $              inum)
300     continue
!        do ip = 1 , add
!         print *, 'adding ',lev(lmax+ip)
!        enddo
! NOW, THE NUMBER CAN BE EITHER CONNECTED TO A NUMBER THAT IS ALREADY
! CONNECTED, THEN IT CAN BE CONNECTED. IT CAN BE NOT CONNECTED TO ANY
! PRIME NUMBER LOWER THAN ITSELF, OR IT CAN BE CONNECTED TO SMALLER
! PRIMES THAT WE DO NOT YET KNOW IF THEY ARE CONNECTED.
!
        if(add.gt.0) then 
!
           do 200 ip = 1 , add 
            if(nums(lev(ip+lmax)).eq.2) then 
!              print *, 'connected to ', nums(lev(ip+lmax))
              nums(inum) = 2 
!              print *, 'found it is connected'
              goto 100 
            endif 
200        continue
!
! WE HAD NONZERO ADD BUT IF WE DID NOT CONNECT, WE NEED TO RETURN.        
!           print *, 'no connection',add
!           do 250 ip = 1 , add!add + lmax
!250          lev(ip) = lev(ip+lmax)
           add = add + lmax 
           call sort(lev,add)
           call getunq(lev,add,newmax)
! DID I ADD SOMETHING? 
           if(newmax.eq.lmax) goto 100 
           lmax=newmax
!           print *, 'connection not found, repeat with',lmax
!              do ip = 1 , lmax
!                 print *, 'testing',lev(ip)
!              enddo
           goto 400 
!
        endif 
! ADD = 0 
100      continue 
        lmax = 0 
        do inum = 1 , limit
           if(nums(inum).eq.2) then
             lmax = lmax + inum
             print *, 'connected',inum
           endif 
        enddo
        print *, nprim,lmax,nprim-lmax

        return
        end 


        subroutine getunq(a,n,m)
        implicit none
        integer  a(*),n,m
        integer  i 
        m = 1 
        do i = 2 , n 
           if(a(i).ne.a(i-1)) then 
             m = m + 1 
             a(m) = a(i)
           endif 
        enddo 
        return
        end 

        subroutine addcon(out,maxadd,add,n,primes,nmax,maxim)
        implicit none
        integer  out(*),primes(*)
        integer  add,maxadd,n,nmax,maxim
        integer  ndig,ndig2
        logical  valid
        integer  ip,k,tmp,tmpval,num,dig
        ndig = log10(real(n))+1
! REPLACE ALL NUMBERS INCLUDING THE FRIST ONE TO REMOVE A DIGIT.
        num = n 
        do ip = ndig-1,0,-1
           dig = num/10**ip
           num = num - dig*10**(ip)
           do k = 0,9
           tmpval = n + (k-dig)*10**ip
           if(tmpval.eq.0) cycle
           ndig2 = log10(real(tmpval))+1
                     valid = ndig2.eq.ndig
                     valid = valid.or.(ndig2+1.eq.ndig.and.ip.eq.ndig-1)
                     valid = valid.and.tmpval.ne.n.and.tmpval.lt.maxim
           if(valid) valid = primes(tmpval).gt.0
             if(valid) then 
                add = add+1
                if(add.gt.maxadd) stop('fail 1 ')
                out(add) = tmpval
             endif 
           enddo  
        enddo   
! ADD A NUMBER IN FRONT
        num = n 
        do k = 1 , 9 
           tmpval = num + k*10**(ndig)
                      valid = tmpval.ne.n.and.tmpval.lt.maxim
           if(valid)  valid=primes(tmpval).gt.0
           if(valid) then 
                add = add+1
                if(add.gt.maxadd) stop('fail 3 ')
                out(add) = tmpval
           endif 
        enddo
        return
        end 







        subroutine sieve(res,maxval)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  val, loop
!
        res(1) = -1 
        do 100 val = 2 , maxval 
100        res(val) = 1
!
        val = 2

50         loop = val        
20         loop = loop + val 
           if(loop.gt.maxval) goto 10 
           res(loop) = -1 
           goto 20
10      continue 
! FIND NEXT PRIME
30      val = val + 1 
        if(val.gt.maxval)    goto 40 
        if(res(val).eq.-1)   goto 30
        goto 50
40      continue 


        return
        end       



        subroutine sort(a,n)
        implicit none
        integer  a(*),n
        integer  ne,nb,bc,w,b

! HIRING PART. WE START WITH N/2+1 HIRED PEOPLE AND WE EMPLOY POTENTIAL 
! N/2 BOSSED
        if(n.eq.1) return
        ne = n
        nb = n/2+1
200     continue         
        if(nb.gt.1) then 
! HIRE GUY AND THEN ASSES HIM            
           nb = nb - 1 
           bc = a(nb)
        else 
! RETIRE THE TOP GUY, TAKE THE LAST ONE AND GIVE HIM THE PLACE. THEN
! VERIFY  
           bc    = a(ne)
           a(ne) = a(1) 
           ne    = ne - 1 
             if(ne.eq.1) then 
                a(1) = bc 
                return
             endif 
!
         endif 
! SET AS THE BOSS THE ITH ELEMENTS 
        b = nb 
        w = 2 * b 
! IN THE HIRE LOOP, FOR BOSS B WE MAKE SURE THAT NO WORKER W
! SUPERVISED BY B HAS BETTER QUALIFICATIONS. THE WORKERS ARE 
! 2*B AND 2*B + 1 . IS THE SWITCH IS DONE, THEN WE NEED TO MAKE SURE 
! NO MORE LEVELS OF DEMOTION ARE NEEDED. BOOS QUALIFICATIONS ARE BC.
! THERE ARE NE PEOPLE.
100      continue 
         if(w+1.le.ne) then 
           if(a(w+1).gt.a(w)) w = w + 1 
         endif 
         if(bc.lt.a(w)) then 
            a(b) = a(w)         
! CHECK IF YOU NEED TO DEMOTE B FURTHER                    
            b    = w
            w    = 2 * b 
         else 
! YOU DON'T, SKIP TO THE LAST EMPLOYEE 
            w = ne + 1 
         endif 
         if(w.le.ne) goto 100 
! NO MORE DEMOTION IS NEEDED. PUT OLD BOSS QUALIFICATIONS INTO HIS NEW
! POSITION 
         a(b) = bc 

         goto 200 

         return
         end 

#define __maxrep__ 2*10**6
#define __nmax__   10**12
!  336108797689259276
!
!real    0m0.235s
!user    0m0.231s
!sys     0m0.002s
        program eu346 
        implicit none
        integer  base,nmax,reps(__maxrep__),nreps,maxrep
        integer  maxbas 
        integer  total,rep 

        maxrep = __maxrep__
        nmax   = __nmax__ 

        nreps = 0 
        
        maxbas = int(sqrt(real(nmax)))

        do 100 base = 2 , maxbas 
          call genrep(base,nmax,reps,nreps,maxrep)
100     continue         

        call sort(reps,nreps)

        total = 1 ! 1 is apparently a rep unit 
        base  = 1 
200     rep   = reps(base)
        total = total + rep
300     base = base + 1 
        if(base.gt.nreps) goto 400 
        if(reps(base).eq.rep) goto 300
        goto 200
400     print *, total
        return
        end 


        subroutine genrep(base,nmax,reps,nreps,maxrep)
        implicit none
        integer  base,nmax,reps(*),nreps,maxrep
! Consider single and double units in any basis as trivial. Single 
! unit is just 1 and any number n is repuint of size 2 in base
! n-1   
        integer  ipow,irep,rep
        
        ipow = 3 
        rep  = base**2+base+1
100     continue 
         if(rep.gt.nmax) return
        nreps = nreps + 1 
         if(nreps.gt.maxrep) then
            print *, 'allocated to little'
            stop     
         endif 
        reps(nreps) = rep 
        rep  = rep + base**ipow       
        ipow = ipow + 1 
        goto 100        

        return
        end 
          
        


! BASED ON NUMERICAL RECIPES 
        subroutine sort(a,n)
        implicit none
        integer  a(*),n
        integer  ne,nb,bc,w,b

! HIRING PART. WE START WITH N/2+1 HIRED PEOPLE AND WE EMPLOY POTENTIAL 
! N/2 BOSSED
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

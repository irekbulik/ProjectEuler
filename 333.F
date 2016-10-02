#define MAXVAL 1000000
        program e333
        implicit none
        integer    res(MAXVAL)
        integer    dim2, dim3
        parameter (dim2=floor(log(MAXVAL*1.0d0)/log(2.0d0)),
     $             dim3=floor(log(MAXVAL*1.0d0)/log(3.0d0)))
        integer    pairs(2,(dim2+1)*(dim3+1))
        integer    npairs
        integer    k
        integer    nused 
        integer    pused(150)
!
        external   updres
!

        call prepp(pairs,dim2,dim3,npairs)
!
        do 10 k = 1 , MAXVAL    
10       res(k) = 0
!  
        nused = 0
        call updres(res,1,npairs,0,MAXVAL,pused,nused,pairs,updres)
        call sieve(res,MAXVAL)        


        return
        end 



        subroutine sieve(res,maxval)
        implicit none 
        integer  maxval
        integer  res(maxval)
        integer  val, loop


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
                



        val = 0 
        do 60 loop = 2 , maxval 
60      if(res(loop).eq.1) val = val + loop
        
        print *, val

        return
        end       


        subroutine updres(res,st,np,inval,nmax,used,nused,pairs,calls)
        implicit none
        integer  nmax
        integer  res(nmax)      
        integer  pairs(2,np)
        integer  inval, st, np
        integer  nused
        integer  used(*)
!
        integer  val 
        integer  locval 
!
        external calls
!
! HERE WE NEED AN ARRAY TO STORE THE USED PATH. SAY WE NEED 100 
!  
        integer curuse(150)
        integer k 
!
        logical update
!
! CHECK WHETHER WE SHOULD RETURN BECAUSE WE WENT TOO FAR WITH VALUES 
        if(inval.gt.nmax) return
! CHECK WHETHER WE SHOULD RETURN BECAUSE WE DON'T HAVE MORE PAIRS 
        if(st.gt.np)    return 
! IF WE ARE ALLOWED, WE ARE NOT UPDATING THE ARRAY
        call calls(res,st+1,np,inval,nmax,used,nused,pairs,calls)
! SEE IF WE CAN UPDATE THE POINTER
        locval = inval + val(pairs(1,st),pairs(2,st))
        if(locval.gt.nmax) return
! SEE IF WE CAN UPDATE
        update = .true. 
        do 10 k = 1 , nused 
         update = ( ( pairs(1,st).gt.pairs(1,used(k)) ) . or .  
     $              ( pairs(2,st).gt.pairs(2,used(k)) )        ) 
     $  . and . update
        if(.not.update) goto 20 
10      continue 
20      continue         
        if(.not.update) return
! WE CAN UPDATE 
        if(inval.ne.0) then  
          res(locval) = min(res(locval) + 1,10)
        else
          res(locval) = min(res(locval) + 1,10)
        endif
!
        curuse(1)   = st 
        do 30 k = 1 , nused
30        curuse(k+1) = used(k)
! MAKE THE CALL  
        call calls(res,st+1,np,locval,nmax,curuse,nused+1,pairs,calls)

        return
        end 





        integer function val(i,j)
        implicit none 
        integer  i,j

        val = 2**i * 3**j

        return
        end 


        subroutine prepp(pairs,dim2,dim3,npairs)
        implicit none 
        integer    dim2, dim3, npairs 
        integer    pairs(2,(dim2+1)*(dim3+1))
        integer    locpa(2,(dim2+1)*(dim3+1))
        integer    k,l,i2,i3
        integer    val
        integer    maval, mapos
!
        k = 1 
        do 10 i2 = 0 , dim2
        do 10 i3 = 0 , dim3
         pairs(1,k) = i2
         pairs(2,k) = i3 
10       k = k + 1 

        


        do 25 k = 1 , (dim2+1)*(dim3+1)
         mapos = k 
         maval = val(pairs(1,k),pairs(2,k))
         do 20 l = k+1, (dim2+1)*(dim3+1)
         if(val(pairs(1,l),pairs(2,l)).lt.maval) then
         maval = val(pairs(1,l),pairs(2,l))
         mapos = l
         endif
20       continue 
        locpa(1,k)     = pairs(1,mapos)
        locpa(2,k)     = pairs(2,mapos)
        pairs(1,mapos) = pairs(1,k)
        pairs(2,mapos) = pairs(2,k)
        pairs(1,k)     = locpa(1,k)
25      pairs(2,k)     = locpa(2,k)
        
        npairs = 0 
        do 30 k = 1 , (dim2+1)*(dim3+1)
        if(val(locpa(1,k),locpa(2,k)).le.MAXVAL) then 
          npairs = npairs + 1 
        else 
         goto 40
        endif
30      continue          
40      continue 

        do 50 k = 1 , npairs 
         pairs(1,k) = locpa(1,npairs-k+1)
50       pairs(2,k) = locpa(2,npairs-k+1)
        

        return
        end




   





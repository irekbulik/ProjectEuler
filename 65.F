!gfortran -fdefault-integer-8 -fdefault-real-8 -O4 65.F 
!
!euler 65 =       272
!
!real    0m0.006s
!user    0m0.004s
!sys     0m0.002s

!> @author IWB
!> @brief  Euler 65
#define MAX_SIZE 100
        program eu65
        implicit none
        integer  h0(MAX_SIZE+1)
        integer  h1(MAX_SIZE+1)
        integer  h2(MAX_SIZE+1)

        integer  k0(MAX_SIZE+1)
        integer  k1(MAX_SIZE+1)
        integer  k2(MAX_SIZE+1)
        
        integer  tmp(MAX_SIZE+1)
        integer  cfe 
        integer  icon


        call makein(2,h2)
        call makein(1,k2)
        
        call makein(3,h1)
        call makein(1,k1)

        do icon = 2 , 100-1
           if(mod(icon-2,3).eq.0) then
             cfe = 2*((icon-2)/3+1)
           else
             cfe = 1
           endif 
 
! NUMERATOR 
           call makein(cfe,tmp)
! CFE * H1 
           call mulint(tmp,h1 ,h0)
! CFE * H1 + H2 
           call addint(h0 ,h2 ,tmp)
           call equint(tmp,h0)
! DENOMINATOR 
           call makein(cfe,tmp)
! CFE * H1 
           call mulint(tmp,k1 ,k0)
! CFE * H1 + H2 
           call addint(k0 ,k2 ,tmp)
           call equint(tmp,k0)
 
        call equint(h1,h2)      
        call equint(h0,h1)
        
        call equint(k1,k2)      
        call equint(k0,k1)

        enddo     
        cfe = 0      
        do icon = 1 , h0(1)
        cfe = cfe + h0(icon+1)
        enddo
        
        write(*,1800) cfe

1800    format('euler 65 =',I10)


        return
        end 

        subroutine prtint(inarr)
        implicit none
        integer  inarr(0:*)
        integer  i 
!
        write(*,1800)(inarr(i),i=inarr(0),1,-1)
!
1800    format(100(I1))        
        return
        end 


        subroutine makein(inval,inarr)
        implicit none 
        integer  inval, inarr(0:*)
        integer  ndig, idig 
        integer  suma 
        
                        ndig = 1
        if(inval.gt.0)  ndig = int(Log10(Real(inval))) + 1 
        inarr(0) = ndig 
!
        do idig = 1 , MAX_SIZE
           inarr(idig) = 0 
        enddo
           suma     = 0
        do idig = 1 , ndig 
           inarr(idig) = (mod(inval,10**idig)-suma)/10**(idig-1)
           suma = suma + inarr(idig) * 10**(idig-1)
        enddo
        return
        end        
        



        subroutine addint(in1,in2,res)       
        integer    in1(0:*),in2(0:*),res(0:*)
        integer    carry,val
        logical    error 
        integer    len1, len2
        integer    loop,idig

           do idig = 1 , MAX_SIZE
              res(idig) = 0 
           enddo
!
           len1 = in1(0)
           len2 = in2(0)
!
           res(0) = max(len1,len2)         
           carry  = 0 
!
           do idig = 1 , max(len1,len2)
             val       = in1(idig) + in2(idig) + carry
             carry     = val/10
             val       = mod(val,10)
             res(idig) = val 
           enddo
!
             if(carry.ne.0) then
                if(max(len1,len2).eq.MAX_SIZE) then
                   print *, 'integer overflow'
                   stop
                endif             
              res(0)      = res(0)+1
              res(res(0)) = carry 
             endif 
          return
          end  

        subroutine mulint(in1,in2,res)
        integer    in1(0:*),in2(0:*),res(0:*)
        integer    carry,val,add
        integer    len1, len2
        integer    loop,idig
        integer    tmp(0:MAX_SIZE)
        integer    tmp1(0:MAX_SIZE)
!         
           len1   = in1(0)
           len2   = in2(0)
!
           call makein(0,res)

              do idig = 1 , min(len1,len2)
!
              call makein(0,tmp)
              ind = idig - 1
              carry = 0 
!
                do jdig = 1 , max(len1,len2)
                if(len1.lt.len2) then                   
                val   = in1(idig) * in2(jdig) + carry
                else
                val   = in1(jdig) * in2(idig) + carry
                endif 
                carry = val/10
                val   = mod(val,10)
!
                if(ind.ge.MAX_SIZE) then
                   print *, 'integer overflow'
                   stop
                endif  
!
                ind      = ind + 1 
                tmp(ind) = val
!
                enddo
!
               tmp(0) = ind
               if(carry.ne.0) then
                 if(tmp(0).ge.MAX_SIZE) then
                  print *, 'integer overflow'
                  stop
                 endif 
                 tmp(0)     = tmp(0)+1
                 tmp(tmp(0))= carry
               endif 
               call addint(res,tmp,tmp1)
               call equint(tmp1,res)
               enddo  

        return
        end 


        subroutine equint(in1,in2)
        implicit none
        integer  in1(*),in2(*)
        integer  i 
         do i = 1 , MAX_SIZE + 1 
            in2(i) = in1(i)
         enddo
         return
         end 
        








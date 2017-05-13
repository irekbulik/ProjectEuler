NB. We can parametrize the problem by tracking the longest string with a 
NB. norepeating characters. For 1 character there is 
NB. 7 possible strings with 1 nonrepeating character. 
NB.
NB. Let us say we are at state with character of length 1. There is one option to stay here
NB. and 6 options to move to 2.
NB.  
NB. Let us say we are at the state with 2 distinct characters 1 2. We can choose 1 and stay at 2, choose 2
NB. and go to 1 and 5 options to go to 3.
NB. 
NB. Let us say we are at the state with 3 distinct character 1 2 3. We can choose 1 and stay at 3, choose 2 and go to 2, choose 
NB. 3 and go to 1 and we have 4 options to move to 4. 
NB. 
NB. If we hit 7, we stay there.
NB. 
NB. 		      10^12 -1 
NB.    1 6 0 0 0 0  T          7 
NB.    1 1 5 0 0 0             0 
NB.    1 1 1 4 0 0             0
NB.    1 1 1 1 3 0             0 
NB.    1 1 1 1 1 2             0
NB.    1 1 1 1 1 1             0 
NB. 
NB. Is the answer.  
NB.
mat_pow_mod =: 3 : 0 
mm =: +/ .*
p   =. >0{y
b   =. >1{y
mod =. >2{y
if.    p <: 2 do. mod | b mm^:(<:p) b 
else.  
       if.    0 = 2|p  do.  mod |  mm~ mat_pow_mod (-:p);b;mod  
       else.                mod | b mm mat_pow_mod (<:p);b;mod

       end.
end.
) 
a =: |: 6 6 $ 1 6 0 0 0 0 1 1 5 0 0 0 1 1 1 4 0 0 1 1 1 1 3 0 1 1 1 1 1 2 1 1 1 1 1 1 1 
v =: 7 0 0 0 0 0  
' '
(10x^9) | +/ (mat_pow_mod (<:10x^12);a;10x^9) mm v

NB. [0] time jl < 458.ijs 
NB.                                                                                           
NB.   423341841
NB.  
NB.real	0m0.055s
NB.user	0m0.043s
NB.sys	0m0.009s




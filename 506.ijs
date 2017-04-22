pow_mod =: 3 : 0 
p   =. >0{y
b   =. >1{y
mod =. >2{y
if.    p < 10 do. mod | b^p 
else.  
       if.    0 = 2|p  do.  mod | *:  pow_mod (-:p);b;mod  
       else.                mod | b * pow_mod (<:p);b;mod

       end.
end.
) 
m =: 123454321x
d =: 999998000001x
e =: 999999x
v =: 1 2 3 4 32 123 43 2123 432 1234 32123 43212 34321 23432 123432
p =: 234321 343212 432123 321234 123432 432123 212343 432123 123432 321234 432123 343212 234321 123432 123432
st =: 3 :  ' m |((-(10x^6)) +(_999999x*y) + pow_mod (>:y);(10x^6);(m*d))%d'
sm =: 3 :  ' m |( <: pow_mod (6x*(>:y));10x;(m*e))%e '
vn =: 3 :  ' m |( (pow_mod (<.(<:y)%15);(10x^6);m)  *(15|<:y) { v )   + ( (15|<:y) { p ) * sm <:<.(<:y)%15'
an =: 4 :  '  ( ( sm y )  * (<:x) { v ) + ( ( st y)  * (<:x) { p) '
aa =: 3 :  ' m | +/ (>:i.15) an y '
sum_round =: 3 : ' if.  0 < <.(<:y)%15 do. aa <: (<.(<:y)%15) else. 0 end.'
sum_rest  =: 3 : ' +/ vn >: (+ i.@(y&-))~ 15 *(<.(<:y)%15)  '
sum =: 3 : 'm| ( sum_round y )+ sum_rest y'
' '
sum 10x^14
NB. [0] time jl <506.ijs 
                                              
NB.   18934502
      
NB. real	0m0.060s
NB. user 	0m0.038s
NB. sys	        0m0.018s




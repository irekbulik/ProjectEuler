NB. Looks that the jumps over occur at g(n) = 3 n. It is definitely true for 
NB. n = 9 and g(n) = 27. Let us now assume it will always happen at that and we prove it latter.
NB. 
NB. With that assumption, the next jump will occur when n+d is not coprime to 3n+d-1 = 2n -1 + n + d, 
NB. from definition of g(n).
NB. 
NB. It follows that n + d is not coprime to 2n - 1. And we look for the smallest such d. 
NB. 2n-1 can be written as p1*p2*p3... without taking into account the exponents. Because n and 2n-1 are 
NB. always coprime, we must hit shared factors with delta smaller than p1. We compute reminder of n divided by 
NB. p1, and when the delta is p1 - reminder. So the new n where the jump occurs is 
NB.  
NB.  	 n + ( p1 - p1 | n ) ad d = p1 - p1|n 
NB.  
NB. and what is the value at the new point? We have assumed that jumps always occur or 3 n, We should 
NB. get it recursively now. We know now that gcd at new n with the previous g(n-1) is p1. 
NB. By defintion, the value just before jump is 3n + d-1.  
NB. 
NB. The g(n) at jump is then 3n + d - 1 + p1. Well, we know that 2n-1 is divisible by p1. Then
NB.		  n = k p1 + (p1+1)/2 for some k, Such that p1|n = (p1+1)/2.  
NB. Note that p1 is never 2. We can now see that 
NB.               d - 1 + p1 = p1 - (p1+1)/2 - 1 + p1 = 3(p1-1)/2 
NB. 	 and   
NB.		  d = (p1-1)/2  
NB.  
NB.      so 3n + d - 1 + p1 =  3 (n + (p1-1)/2) which means that after jump g(n) = 3 n 
NB.
g =: 3 : 0  
     assert. y >:9
y0=. 9 
g0=. 27
next =. ( [+(]-(]|[)) ) (0&{@q:@<:@*&2)
   NB. evaluate next jump n.  
   while. y0 <: y do. 
   y1 =. y0
   y0 =. next~ y0
   end.

   (3*y1) + (y - y1) 
)
] g 10^15x
NB. OK, so the patter of 3n holds.
NB.  2744233049300770
NB. real	0m3.185s
NB. user	0m3.121s
NB. sys		0m0.011s


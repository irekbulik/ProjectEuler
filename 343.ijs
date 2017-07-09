NB. Looks like nice recursion. We note that if x = 1 and y is odd, we can bring it to the same
NB. as x = 1 and (y-1)/2. Now, we study what happens when x = 1 and y is even. In that case, we can 
NB. guarantee that either x+d divides y-d or the answer is y. Let us assume that we can find reduction 
NB. that does not reset x to 1. Then we have 
NB.  
NB.      1 + d = l * p    
NB.      y - d = k * p  
NB.       
NB.      with l > 1. Clearly, d >= p. Then we can set d <- d - p and we cann satisfy that equations with l <- l - 1 and k <- k + 1  
NB.  
NB. We therefore always work with x = 1. So, if y is even we are set and if y is odd we are looking for minimal d/maximal k solution 
NB.      
NB.      y - d = k + k * d  
NB.
NB.  or the maximum k such that the equation   y - k = 0 mod k + 1 is solved. 
NB. 					       (y+1) - (k+1) = 0 mod k + 1 
NB. 					       (y+1) = 0 mod(k+1) with maximal k 
NB.
NB.  k is one less than the maximum prime factor of y + 1. Now, we now d * (k+1) must divide y-d. 
NB.  if the result is 1, that is the answer. Otherwise, we solve for new y. 

f =: 3 : 0   
     if.      1 = y     do. 1 
     elseif.  1 = 2 | y do. f -: <: y  
     elseif.            do. 
             	   k =. <: {: q: >: y 
		   if.   k = y do. y 
	           else. d =. (y-k)%(>:k) 
                        ny =. (y-d)%(k*>:d)
		        NB. That means we simplify to integer at that step
			if. 1 < ny do. f ny else. k end.
		   end.
     end.
)
NB. That is super slow. I probably hit some large primes. Should Optimize.
NB. Well, finish proving that recursion is not needed and you always get the answer being the largest prime dividing n - 1 
NB. We could even oneline that x^3+1  = (x+1)*(x^2-x+1)      	
+/<:(({:@q:@>:)>.({:@q:@>:@(*<:))) >:i.2000000x

NB. Wolfram
NB. Sum[Max[Last[FactorInteger[i + 1]][[1]], Last[FactorInteger[i^2 - i + 1]][[1]]] - 1, {i, 1, 2000000}]
NB. 269533451410884183  
NB.
NB. PARI
NB. 1+sum(i=2,2*10^6,max( vecmax(factor(1+i)[,1]) , vecmax(factor(i*i-i+1)[,1]) )-1 ) 
NB. 269533451410884183



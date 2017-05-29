Let us parametrize the problem in the following way. We pull a random letter l
or w and build the chain of that letters with equal probabilities. Position in
the i corresponds to win or loose of player mod(i-1,n)+1. The game ends then the
substring "lw" is encountered and then length of the mod(length of the string -
1,n)+1 gives us the winner. 

We can build our transition matrix with the current states: (w)inner (l)ooser (e)nd: 
  w   l   e 
w 1/2 1/2 0 
l 1/2 0   1/2
e 0   0   1 
 
We always start with vector (1 0 0) as 1 cannot win in the first turn. 
The probability that the string will end with size n is 

 (n-1)
-------
 2^(n)

Which is quite easy to see now. 

So, the final answer: 

               n*m+k-1
P (k) =  sum ------------,{m,0,inf}
 n            2^(n*m+k)  

We can evaluate the sums, but the numbers are large anyway.

Pn(k) = 2^n/2^k * ( n + (k-1)*(2^n-1) )/(2^n-1)^2

So, let us see if we can simply evaluate, as we assume that there are no 
shared factors in the expression. 
```j
pow_mod =: 3 : 0 
p   =. >0{y
b   =. >1{y
mod =. >2{y
if.    p < 2 do. mod | b^p 
else.  
       if.    0 = 2|p  do.  mod | *:  pow_mod (-:p);b;mod  
       else.                mod | b * pow_mod (<:p);b;mod
       end.
end.
)  
n=:7+10x^8
k=:7+10x^4
mm=:10x^8
a=: pow_mod (n;2;mm)
b=: pow_mod ((n-k);2;mm)
echo ' ' 
echo 'Answer 605 ' , ": mm|(*:<:a)*(mm|b * n + (<:k)*<:a)
```

                      
   Answer 605 59992576
   


```j
NB. It is quite easy to show that 
NB. 
NB. S(n,m) = (1/2*(n+2)(n+1))**m-(n+1)**m
NB.  
NB. S(n,m) = (n+1)**m *( (n+2)**m/2**m-1) 
NB.
NB.
NB. Now, we do the second part of the problem. Let us start with even n=2i
NB. we can see that the second part will be the only divisible 
NB. one. In particular, we can study just divisibility of 

NB. (1+i)**m-1 

NB. Here, it is quite clear that maximum power of 2 that divides this expression 
NB. is the maximal power of 2 that divides i. 

NB. Now, the odd n. We can parametrize it as 2i-1. We see that the number of divisors 
NB. is m times maximum power of 2 that divides i. 

NB. Putting it together. Please note that the lest step requires even limit of n 

n =: 10^12
m =: 904961
lim =:  <.2^.-:n
echo ' '
echo 'Answer 561 = ' , ": (>:m)*+/(}.*(2-/\<.@((-:n)%2&^@>:))) i.>:lim
echo 'Timing       ' , ( ": 10000 (6!:2) ' (>:m)*+/(}.*(2-/\<.@((-:n)%2&^@>:))) i.>:lim') , 's' 
```

                                                                            
   Answer 561 = 452480999988235494
   Timing       2.5e_5s
   
   


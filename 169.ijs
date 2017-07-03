```j
NB. Well, yeah, so that took me some time. Got some brute force stuff and then discovered the 
NB. Calkin–Wilf tree... So yeah, got myself trapped with the first way, but here how do it it.
NB. 
NB.  Say we want to know f(2n). Then we can take all valid expressions for n, and then double each term. They are  
NB.  valid expressions but not all. There are no partitions that have 2^0. Well, so how do we get those missing ones?
NB.  We partition n-1 and double it (no 1 possible) and then we drop 1 + 1 
NB. 
NB.      f(2n) = f(n) + f(n-1)
NB. 
NB.  What about number 2n+1? Well, it is the same as taking all the partitions of n and then add 1. 
NB.
NB.     f(2n+1) = f(n)
NB. 
f =: 3 : 0 M.
   assert. y >:  0 
   if.      y <: 1 
	do. 1 
   else.
	if.  0=2|y do. (f -:y)+(f<:-:y)
	else.           f <.-:y
	end. 
   end. 
)
echo ' '
echo ' Answer 169 = ' , ": f 10x^25 
```

                                              
    Answer 169 = 178653872807
   

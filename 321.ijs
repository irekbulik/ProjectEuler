Well, pretty lame but we seems to be able to guess solution. 
I did it by hand for 1,2 and then they give answer for 3. I also get the correct triangular
numbers listed in the problem 

M(n) = 2n+1 + M(n-1)
with M(0) = 0
 
	2n(n+2)
M(n) = ---------
	   2 
and we need to check when 2n*(n+2) is some k*(k+1)

Those are the first indices of triangular numbers
 index  n                 k  
 1	1		  2
 2	3                 5	
 3	10               15	
 4	22               32
 5	63               90         
 6	133             189
 7	372             527
 8 	780            1104
 9	2173           3074
10	4551           6437
11	12670         17919   
12	26530           
13	73851            
14	154633		 
15	430440
16	901272
17	2508793

Well, alternatively, we can treat it as Diophantene equation 
2x^2 + 4x - y^2 -y =0 
and solve it with https://www.alpertron.com.ar/JQUAD.HTM
We get chain of soltuions:
X(n+1) = 3 * X(n) + 2 * Y(n) + 3 
X(n+1) = 4 * X(n) + 3 * Y(n) + 5
where X and Y are n and k. We see there are solutions with seed 1,2 and 3,5 

```j
n =: 40 
new_x =: 3&+@(3&*@(0{[)+2&*@(1{[))
new_y =: 5&+@(4&*@(0{[)+3&*@(1{[))
prep  =: ((new_x,new_y),])^:(<:-:n)
get_x =: ((2&|@>:@i.@#)#]) 
echo ' ' 
echo 'Answer : ' , ": +/get_x  (1 2) (,&:prep) (3 5) 
echo 'Time   : ' , (": ( 100 (6!:2) '+/get_x  (1 2) (,&:prep) (3 5) ') ) ,'s' 
```

                   
   Answer : 2470433131948040
   Time   : 0.0004s
   

Brute force recursion gives us few values for strings of size
2  325
3  10400
4  164450
5  1710280
6  13123110
7  78936000
8  385881925
9  1568524100
10 5380787555
11 15730461760
The important observation is that it does not really matter 
how many letters alphabet has. The number of correct string 
of size n with alphabet of size m is n ! m * number of correct 
string of size n with alphabet of size n. This is obviously because there 
are n!m sorted strings that we can from by replacing ordered subsets. 

The problem is then to find the correct strings of n characters such that there is only 
one pair of characters in order. The conjecture is 

number of valid string of size n is 2 * (number of valid string n-1) + n-1
with bottom case of n=1 is 0. We clearly see it works for n = 2, where there is 
1 valid string : 12 out of total 12,21 strings. 

Let us look at size n string. The first number of valid strings is obtained by putting 
n at the beginning of size n-1 correct strings. The other is to take the reverse sorted 
string of size n-1 and put n after 1st,2nd..n-1th: this gives us n-1. Then, for each correct
string we put n in between the correct pair, so that only one new correct pair is formed. 

Bringing it all together

```j
n =: 26 
echo ' ' 
echo 'Answer : ' , ": {. \:~ (!&n >:i.n) * (,(#+2&*@{:))^:(<:n) (0) 
echo 'Time   : ' , ( ": ( 1000 (6!:2) '{. \:~ (!&n >:i.n) * (,(#+2&*@{:))^:(<:n) (0)' ) ), 's'
``` 

       
   Answer : 409511334375
   Time   : 3e_5s
   

       

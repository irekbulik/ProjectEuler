For even number, the streak is always 1. We only care about the odd numbers.
2*n+1+k not divisible by k+1 means that 2*n is not divisible by k+1.  

The pattern looks quite simple. Let us say that we have the smallest n such that 
2*n is not divisible by k+1, so the streak is k. Then all m*(2*n) also have streak 
k, unless k+1 divides m.

Let us set up the program:
We look for P(s,N) there are N-2 numbers with our cutoff. 
If s=1, that are all even numbers

P(1,N) = Floor[(N-2)/2] = Floor[(N-2)/1] - Floor[(N-2)/2]

Now for 

P(2,N) = Floor[(N-2)/2] - Floor[(N-2)/2*3]

The next starting point is lcm (2,3) = 6, the next one is lcm(6,4) = 12, lcm(12,5) = 60, lcm(60,6) = 60, lcm(60,7) = 420 

P(3,N) = Floor[(N-2)/6]  - Floor[(N-2)/12]
P(4,N) = Floor[(N-2)/12] - Floor[(N-2)/60]
P(5,N) = Floor[(N-2)/60] - Floor[(N-2)/60]
P(6,N) = Floor[(N-2)/60] - Floor[(N-2)/420]
.
.
.
.
Quite easy to follow now, by constructing the arr

```j
 n    =: 31
 arr =: *./ \ >:i.>:n
 p =: 3 : 0 
 <.(((4^y)-2)%((<:y)}arr)) - <.(((4^y)-2)%(y}arr))
 )
 echo ' ' 
 echo 'Answer 601: ' , ": +/p"0 >:(i.n)
 echo   'Time ' , (": 1000 (6!:2) '+/p"0 >:(i.n)') , 's'
```  

             
   Answer 601: 1617243
   Time 0.00023s
   
             

We can go bigger 
```j
 n    =: 1000x
 arr =: *./ \ >:i.>:n
 p =: 3 : 0 
 <.(((4^y)-2)%((<:y)}arr)) - <.(((4^y)-2)%(y}arr))
 )
 echo ' ' 
 echo 'Answer 601: ' , ": +/p"0 >:(i.n)
 echo   'Time ' , (": (6!:2) '+/p"0 >:(i.n)') , 's'
```  

             
   Answer 601: 78163478329839183160407026661012541370837128094158642688900459709337360781675580514377272713535358684558330026834025419046419196738432320895492352194957064708331918930507
   Time 3.06s
   
```j
 n    =: 2000x
 arr =: *./ \ >:i.>:n
 p =: 3 : 0 
 <.(((4^y)-2)%((<:y)}arr)) - <.(((4^y)-2)%(y}arr))
 )
 echo ' ' 
 echo 'Answer 601: ' , ": +/p"0 >:(i.n)
 echo   'Time ' , (": (6!:2) '+/p"0 >:(i.n)') , 's'
```  

             
   Answer 601: 1731919572548692202846914286040511824060397350377620539165026797816440990810700788264438901216644412215743206783015499720231238672345314242215054108822141096786344930368828704928443228987293985982094733940758039872697198224819393077398292558975...
   Time 18.5s
   
   


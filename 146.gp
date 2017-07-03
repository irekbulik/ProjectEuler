\\Well, because the primes must be consecutive, we see that 
\\0) n must be even so that n^2+1 is odd.
\\1) n^2 + 5  must not be a prime. You can see that follows that n is multiple of 5. Of course that solves +15 and +25 
\\2) n^2 + 11 must be divisible by 3 or n^2+9 or n^2+13 will be. That solved n^2+17 and n^2+23 not being prime. 
\\3) we are left with n^2+19 and 21
\\ About a minute in pari, not feeling like working that one out more. 
\\ Answer is ::  676333270
 {
 tot = 0;
 for(n=10,150*10^6-1,
   if(Mod(n^2+11,3)==0,
     if(!isprime(n^2+19)&&!isprime(n^2+21),
       if(isprime(n^2+1)&&isprime(n^2+3)&&isprime(n^2+7)&&isprime(n^2+9)&&isprime(n^2+13)&&isprime(n^2+27),tot+=n;print(n))
     )
   );
 n+=9;
 );
 print("total = ",tot)
 }













\\ That is just shameless ... 1.5 min. I think that at this point it defeats the purpose :) 
\\total = 5437849 

 {
 tot = 0;
 for(n=1,50*10^6,
       if(isprime(2*n^2-1),tot+=1)
 );
 print("total = ",tot)
 }


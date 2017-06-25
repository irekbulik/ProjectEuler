A little bruteforce but PARI has all the tools. 

    {
         tot=0;
         for(i = 3, 10^8,
         j=i;
         while(Mod(j,2)==0,j/=2);
         while(Mod(j,5)==0,j/=5);
         if(j>1,tot+=znorder(Mod(10,j)))
         );
         print(tot) 
    }	

446572970925740






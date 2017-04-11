NB. We deal with problem
NB. (x-n)*(y-n) = n*n 
NB. So x, and y must belong to divisors of n^2 shifted by n. 
NB. The list can be generated here
factors =: ( [: /:~@, */&>@{@((^ i.@>:)&.>/)@q:~&__ ) NB. From Roseta_Code. Quite fast
grade =: 0&<@{: * {. >: {:			      NB. 
NB. Counts number of solutions 
sols =: 3 : 0 
{. +/0&< ( grade * ])"1 |:y&+ (] ,: (*:y)&%) (- , ]) factors *:y 
)
NB. After combating Nonce Error 
( 1000&<@sols"0  # ])(180000+i.10000)


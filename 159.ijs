NB. This is very nice problem for DP
NB. We just need a function that returns the factors of a number 
NB. drops the first and last entry, then form the pairs a n/a.

NB. Factors. Get the pairs of prime and exponent. 
NB. Insert (^ i.@>:) between them. Use the under command to open,close box.
NB. We also use catalogue to make combinations.
NB. We then cry how lame we are in J and thank for Roseta Code. 
NB. 
factors  =: /:~@~.@,@(*/@>@{@((>.@(^ i.@>:))&.>/@(__&q:)))
divisors =: }.@(>.@-:@# {. ] )@( ,. |. )@factors
mdrs =: 3 : 0 M.
NB.>./ ((>:9&|<:y ), +/"1 mdrs"0 divisors y) 
>./ ((>:9&|<:y ), +/"1 mdrs"0 divisors y) 
)
sum_it =: 3 : 0 
sum =. 0
for_j. }.}.i.y do. sum=.sum + mdrs j end.
sum
)
NB. Cannot make it FAST. God I suck at it. I can port it to Wolfram and we 
NB. get answer in ~ 1 minute. Clearly I can make F implementation that has 
NB. no loop and divisors sieve that will run in no time. But that is not the point.
NB. Clear[m]
NB. m[n_] := m[n] = 
NB.   If[n == 1, 1, 
NB.    Max[Mod[n - 1, 9] + 1, 
NB.     Tr /@ Map[m, 
NB.       Drop[{Drop[Drop[Divisors[n], 1], -1], 
NB.          n/Drop[Drop[Divisors[n], 1], -1]} // 
NB.         Transpose, -Quotient[DivisorSigma[0, n] - 2, 2]], {2}]]]
NB.  AbsoluteTiming[Table[m[i], {i, 2, 10^6 - 1}] // Tr]
NB.  {73.926220, 14489159}



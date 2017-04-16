NB. 252541322550
NB. real	1m0.647s
NB. user	0m59.827s
NB. sys		0m0.168s

fib=: 3 : 0 M.
 if. 1>:y do. y else. (fib y-1)+fib y-2 end.
)

fib_mod =: 4 : 0 M.
  if.    y<100 do. x | fib y 
  else.  
      if.   0 = 2|y do. x |  ( x | ( ( x  fib_mod <:-:y) + ( x fib_mod >:-:y ) ) ) * (x fib_mod -:y ) 
      else.             x |  (*: (x fib_mod -:>:y )) + ( *: (x fib_mod <:-:>:y ) )
      end.
  end.
)

pow_mod =: 3 : 0 
p   =. >0{y
b   =. >1{y
mod =. >2{y
if.    p < 10 do. mod | b^p 
else.  
       if.    0 = 2|p  do.  mod | *:  pow_mod (-:p);b;mod  
       else.                mod | b * pow_mod (<:p);b;mod

       end.
end.
)                             
NB.					      n+1             n
NB.          x * ( F(1)(x-1) - F(2) x + F(n) x    +  F(n+1) x 
NB. P(n,x) = --------------------------------------------------
NB.                          x*x + x - 1
NB. 
modInv =: 4 : 0 
assert.  0 = x +. y
x y&|@^ <: 5 p: y
)

poly =: 4 : 0 
 ord =. > 0 { x 
 mod =. > 1 { x 
 xva =. y
 mod =. mod *  ( <: xva+*:xva)
 temp =. mod | xva * (<:xva) + (-xva) + ( ( mod fib_mod ord ) * pow_mod (>:ord);xva;mod ) + ( ( mod fib_mod >:ord ) * pow_mod ord;xva;mod )  
 mod | temp
 temp % ( <: xva+*:xva)
)
](!15x) | +/((10^15x);(!15x))&poly"0 (>:i.100)

NB. The first code was in wolfram but then I saw the cool solution of Roger Hui
NB. and I decided to redo my way. Worse way, but it is cool nontheless
   f=: 3 : '(( %. (>: ^/ ])@i.@# ) p. >:@#) (11 $1 _1)&p.@>:i.y'
   +/(f"0)>:i.10x
NB. The lameness is from me defining a monad so i can then do the rank stuff. 
NB. [0] time jl < 101.ijs 
NB.            37076114526
NB.         
NB. real	0m0.171s
NB. user	0m0.065s
NB. sys	0m0.018s



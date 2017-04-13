NB.             51 1 2 5 8
NB.       
NB. real	0m0.868s
NB. user	0m0.850s
NB. sys 	0m0.012s
NB.
list =: ((3#4)#: i.4^3){ +`-`*`%
app =: 4 : '  (  x { list) / y '
is_int =: ( 0=1&|@(* _>| ) ) *. (_>|)
gen_list =: 3 : '(is_int # ]) ((app&y)"0) (i.4^3)'
NB. THERE IS ABSOLUTE VALUE HERE BECAUSE WE CAN PUT - IN FRONT, ALWAYS!
count_list =: 3 : '+/1&=(-i.@#) /:~(0&<# ])~.|,gen_list"1 (i.!4) A. y'
NB. THAT IS SUPER LAME 
{: /:~ (count_list , ]) "1 }.~.([ *((0=(#-#@~.)) * ([ -: /:~)))"1 ((>:(4#9)#: i.9^4))


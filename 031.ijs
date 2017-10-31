 NB.          0.81
NB.	    73682
real_change=: 4 : 0 
xx =: x 
y change 0
)

change =:  4 : 0 M.
NB. GIVEN BOX WITH AMOUNT AND FIRST COIN, GIVES ALL THE CURRENT CHANGES
 if.     0=x do. 1 
 elseif. 0>x do. 0
 elseif. 0<x do. +/((x&-)@(]{&(xx))change])"0 y+i.(#xx)-y
 end.
)

(6!:2) '(1 2 5 10 20 50 100 200 ) real_change 200'
(1 2 5 10 20 50 100 200 ) real_change 200


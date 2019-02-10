m =. 60
NB. Get all divisors of 2**m - 1 
div =. 3 : '~.*/"1>:(#:@(i.@(2&^@#))#<:@])q:<:2x^y'
a =. ~. ,/ div"0 >:>:i.<:<:m
b =. div m
echo ' '
echo 'Answer 622 = ' , ": +/ >:(-. b e. a) # b

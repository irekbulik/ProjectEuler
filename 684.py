"""
Answer 684 = 922058210

real	0m0.050s
user	0m0.035s
sys	0m0.011s
"""
mod = 1000000007
def s(n):
    l = n//9
    r = n - l*9
    p = pow(10,l,mod)
    return p-1 + p * r

def sol(n):
    inv_mod = 111111112 # 9^-1%mod 
    lim = n//9
    off = lim*9
    # ans = t1 - t2 + t3
    t1 = 45 * ( pow(10, lim, mod) - 1 ) * inv_mod
    t2 = - lim * 9
    t3 = sum(s(i) for i in range(off,n+1))
    return (t1 + t2 + t3 )%mod

ans = 0
fm2, fm1 = 0, 1
for k in range(2,91):
    f = fm1 + fm2
    ans = (ans + sol(f))%mod
    fm2, fm1 = fm1, f

print(f"Answer 684 = {ans%mod}")

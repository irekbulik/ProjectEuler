"""
Cool problem, learned something.

Solution explanation:

define f(n) and c(n) to be sum and count of valid number with digit sum of n.
then:

    f(n+1) = sum_i=1^9 f(n-i)*10 + c(n-i)*i

Answer 377 = 732385277

real	0m1.789s
user	0m1.755s
sys	    0m0.019s
"""

N = 9
MOD = 10**N


def matmul(a, b):
    n, k = len(a), len(a[0])
    m = len(b[0])
    rv = [[0] * m for _ in range(n)]
    for i in range(n):
        for j in range(m):
            rv[i][j] = sum(a[i][l]*b[l][j] % MOD for l in range(k))
    return rv


def get_remaining(ds, number, remaining):
    if ds < 0:
        return
    if ds == 0:
        remaining.append(number)
    for i in range(1, 10):
        get_remaining(ds-i, number*10+i, remaining)


def power_mod(t, power):
    b = t
    rv = [[1 if i == j else 0 for j in range(len(t))] for i in range(len(t))]
    while power > 0:
        if power & 1:
            rv = matmul(rv, b)
        b = matmul(b, b)
        power >>= 1
    return rv


t = [[0] * 18 for _ in range(18)]

for i in range(8):
    t[i][i+1] = 1
    t[i+9][i+10] = 1
for i in range(9):
    t[8][i] = 10
    t[8][9+i] = 9-i
    t[9+8][9+i] = 1

# initialize

f, g = [0]*9, [0]*9
for i in range(9):
    remaining = []
    get_remaining(i+1, 0, remaining)
    f[i] = sum(remaining)
    g[i] = len(remaining)
v = f + g
v = [[v[i]] for i in range(len(v))]

ans = sum(matmul(power_mod(t,target-9),v)[8][0] for target in [13**i for i in
    range(1,18)])
print(f'Answer 377 = {ans%MOD}')




from scipy import stats
import numpy as np
N = 10_000_000
def sol(N):
    return (N-np.sqrt(N)*stats.norm.ppf(0.25))/np.log(10)

def find_root(N):
    guess = sol(N)
    l,h = guess*0.98, guess*1.02
    while True:
        mid = (l+h)/2
        val = 1 - stats.erlang.cdf(mid*np.log(10),N)
        if abs(val-0.25) < 1e-10:
            break
        if val > 0.25:
            l = mid
        else:
            h = mid
    return mid

print(f"Answer 697 = {find_root(N):.2f}")



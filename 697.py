from scipy import stats
import numpy as np
#
# first approach sol was CLT but it is not accurate enough. The second one writes
# Ln(X_N) = - sum_i - Ln(U_i) + Ln(C)
# Ln(X_N) = - sum_i W_i + Ln(C)       where W ~ Exp(1)
# Ln(X_N) = - Y_N + Ln(C)             where Y_N is Erlang(1,N)
#
#
N = 10_000_000
def sol(N):
    return (N-np.sqrt(N)*stats.norm.ppf(0.25))/np.log(10)

def find_root(N):
    guess = sol(N)
    l,h = guess*0.9999, guess*1.0001
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



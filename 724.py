import time
#
# T_i => Time to Choose ith stationary drone given we chose i-1 already
# T = Sum_i T_i
#
# E[T]   = N * Sum[1/i, {i,1,N}]

def ET(N):
    return N*sum(1/i for i in range(1,N+1))

def ET2(N):
    P = [(N-i+1)/N for i in range(1,N+1)]
    return sum((1-p)/p/p for p in P) + ET(N)**2

N = 100_000_000
t = time.time()
ans = (ET(N)+ET2(N))/2/N
print(f"Answer 728 = {ans} time taken {time.time()-t}")



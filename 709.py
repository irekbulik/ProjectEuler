"""
 Implemented, noticed pattern, found quadratic implementation https://oeis.org/A000364
"""
N = 24680//2
prime = 1020202009
S = [0] * (N+1)
S[0] = 1
for k in range(1,N+1):
    S[k] = (k*S[k-1])%prime
for k in range(1,N+1):
    for j in range(k,N+1):
        S[j] = ((j-k)*S[j-1]+(j-k+1)*S[j])%prime
print(f'Answer 709 = {S[N]}')

# bit embarrassing
"""
answer =  538319652
538319652

real	7m20.737s
user	6m41.894s
sys	0m3.669s
"""

import sympy
from collections import Counter

class Euler650:
    """ euler 650 """
    def __init__(self):
        N = 20000
        mod = 1000000007
        self.primes = [i for i in range(N+1) if sympy.isprime(i)]
        ans = 0
        for i in range(1, N+1):
            val = 1
            for prime, exponent in self.prime_factor_of_binomial(i).items():
                val = (val * pow(prime, exponent+1, mod) - val) * \
                    pow(prime-1, mod-2, mod)
            ans = (ans + val) % mod
        print('answer = ', ans)
        print(ans)

    def prime_factor_of_binomial(self, n):
        """ get prime factors of binomial product """
        factors = Counter()
        for j in self.primes:
            i = j
            if i > n:
                break
            while i <= n:
                factors[j] += n//i * (n+1)
                l = (n+1)//i-1
                factors[j] -= 2*(i*l*(l+1)//2 + (n+1-(l+1)*i)*(l+1))
                i *= j
        return factors


a = Euler650()

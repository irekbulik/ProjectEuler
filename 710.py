"""
solves 710

Found pattern

"""
from functools import lru_cache
class Euler710:


    def solution():
        """ solves the problem """
        @lru_cache()
        def c(n):
            if n < 3:
                return 1
            if n <5:
                return 2
            return (c(n-1) + c(n-5))%1_000_000
        def t(n):
            return pow(2,n//2,1_000_000) - c(n)
        n = 42
        while t(n)%1_000_000 != 0:
            n+=1
        print(n)

Euler710.solution()

"""
[0] time python3 692.py
Answer 692: 842043391019219959

real	0m0.045s
user	0m0.031s
sys	0m0.011s
=====================
"""
class Euler692:
    """ bruteforce gives us A139764 then we have it easy as limit is Fibonacci """
    def __init__(self):
        fm1, fm2 = 1,1
        gm1, gm2 = 1,1
        while fm1 < 23416728348467685:
            f = fm1 + fm2
            g = gm1 + gm2 - fm2 + f
            gm1, gm2 = g, gm1
            fm1, fm2 = f, fm1
        self.ans = g
    def solve(self):
        return self.ans
print( f"Answer 692: {Euler692().solve()}")

""" Euler666 """
from scipy.optimize import root
class Euler666:
    """ answer 0.48023168

    real	0m2.836s
    user	0m2.818s
    sys	0m0.144s """

    def __init__(self):
        k = 500
        m = 10
        r = [306]
        for _ in range(k*m+m+1):
            r.append(r[-1]**2 % 10007)
        values = [1/k] * k

        def eval_one(i, val):
            res = 0
            for j in range(m):
                q = r[i*m+j] % 5
                if q == 0:
                    res += 1/m
                elif q == 1:
                    res += 1/m * val[i] * val[i]
                elif q == 2:
                    res += 1/m * val[(2*i) % k]
                elif q == 3:
                    res += 1/m * val[(i*i+1) % k]**3
                else:
                    res += 1/m * val[i]*val[(i+1) % k]
            return res - val[i]

        def eval_func(val):
            return [eval_one(i, val) for i in range(len(val))]

        self.__sol = root(eval_func, values)

    def solution(self):
        """ just print """
        if max(self.__sol.x) < 1:
            print("answer {:.8f}".format(self.__sol.x[0]))
        else:
            print("wrong solution with probs over 1")


SOLUTION = Euler666()
SOLUTION.solution()

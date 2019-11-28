# there is a patter that could help us 
import math
class Euler686():
    def __init__(self):
        log2 = math.log10(2)
        smal = math.log10(1.23)
        high = math.log10(1.24)
        def correct(i):
            return smal < (i*log2)%1 < high
        i = found = 0
        while found < 678910:
            i+=1
            if correct(i):
                found += 1
        self.solution = i
        
solution = Euler686()
print(f"answer = {solution.solution}")

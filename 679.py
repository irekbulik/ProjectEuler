# one could clean the code but the problem is very easy
class Euler679():
    def __init__(self):
        alphabet = set(['A', 'E', 'F', 'R'])
        keywords = { 'FREE' : 1<<0, 'FARE' : 1<<1, 'AREA': 1<< 2, 'REEF': 1 << 3}
        strings  = {s : i for i,s in enumerate([i+j+k for i in alphabet for j in alphabet for k in alphabet])}
        states   = [ 0 ] * ( 16 * 64 )
        states[0:64] = [1] * 64
        for size in range(4,30+1):
            new = [ 0 ] * len(states)
            for letter in alphabet:
                for string in strings:
                    new_string = string + letter
                    if new_string in keywords:
                        for i in range(16):
                            for j in range(16):
                                if (j | keywords[new_string] ) == i and j & keywords[new_string] == 0:
                                    new[strings[new_string[1:]]+i*64] += states[strings[string]+j*64]
                    else:
                        for i in range(16):
                            new[strings[new_string[1:]]+i*64] += states[strings[string]+i*64]
            states = new
        self.solution = sum(states[15*64:])

euler = Euler679()
print(f"ans = {euler.solution}")

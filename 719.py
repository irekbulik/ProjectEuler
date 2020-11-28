def dr(n):
    return (n - 1) % 9 + 1

def can_sum(n, t):
    number = str(n)
    def can_sum_l(number, start, target):
        if len(number) == 0:
            return target == 0
        if start == len(number):
            return target == int(number)
        if target < 0:
            return False
        if int(number) == target:
            return True
        return (
            can_sum_l(number, start+1, target) or can_sum_l(number[start+1:], 0, target - int(number[: start + 1]))
        )
    return can_sum_l(number,0,t)

def solve():
    acc = 0
    for i in range(2, 1_000_000+1):
        if i%10000 == 0:
            print(i)
        if dr(i*i) == dr(i):
            if can_sum(i*i,i):
                acc += i*i
    return acc

print(solve())
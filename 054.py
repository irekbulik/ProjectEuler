import sys, os
from collections import Counter
def map_value(v):
    try:
        return int(v)
    except:
        if v == 'T':
            return 10
        if v == 'J':
            return 11 
        if v == 'Q':
            return 12
        if v == 'K':
            return 13
        if v == 'A':
            return 14
        raise("error")
        
def split_cards(cards):
    v = [map_value(c[0]) for c in cards]
    s = [c[1] for c in cards]
    return v,s
    
def royal_flush(v,s):
    return len(set(s)) == 1 and set(v) == set([10,11,12,13,14])

def straight(v,s):
    v = sorted(v,reverse=True)
    if all([ v[i-1]-v[i] == 1 for i in range(1,5) ]):
        return True, v
    return False,[]

def flush(v,s):
    if len(set(s)) == 1:
        return True, sorted(v, reverse=True)
    return False,[]

def straght_flush(v,s):
    if flush(v,s)[0] and straight(v,s)[0]:
        return flush(v,s)
    return False,[]

def four_of_a_kind(v,s):
    c = Counter(v)
    if max(c.values()) == 4:
        c1,c2 = c.keys()
        return True, [c1,c2] if c[c1] == 4 else [c2,c1]
    return False,[]
    
def full_house(v,s):
    c = Counter(v)
    if sorted(c.values()) == [2,3]:
        c1,c2 = c.keys()
        return True, [c1,c2] if c[c1] == 3 else [c2,c1]
    return False,[]

def three_of_a_kind(v,s):
    c = Counter(v)
    if max(c.values()) == 3:
        r = [i for i in v if c[i] == 3] + sorted([i for i in v if c[i] != 3], reverse=True)
        return True,r
    return False,[]

def two_pairs(v,s):
    c = Counter(v)
    if sorted(c.values()) == [1,2,2]:
        return True, sorted([i for i in v if c[i] == 2], reverse=True) + sorted([i for i in v if c[i] != 2], reverse=True)
    return False,[]

def pair(v,s):
    c = Counter(v)
    if max(c.values()) == 2:
        return True, sorted([i for i in v if c[i] == 2], reverse=True) + sorted([i for i in v if c[i] != 2], reverse=True)
    return False,[]

def highest(v,s):
    return True, sorted(v, reverse=True)

def winner(v1,s1,v2,s2):
    if royal_flush(v1,s1):
        print('p1')
    if royal_flush(v2,s2):
        print('p2')
    if royal_flush(v1,s1) and not royal_flush(v2,s2):
        return 1
    if royal_flush(v2,s2) and not royal_flush(v1,s1):
        return 2
    for function in [straght_flush, four_of_a_kind, full_house, flush, straight, three_of_a_kind, two_pairs, pair, highest]:
        p1h,p1c = function(v1,s1)
        p2h,p2c = function(v2,s2)
        if not p1h and not p2h:
            continue
        if p1h and p2h:
            return 1 if p1c > p2c else 2
        if p1h and not p2h:
            return 1
        return 2
    

ans = 0
with open(os.path.join(os.environ['HOME'],"Downloads/p054_poker.txt")) as filehandle:
    for line in filehandle:
        v = line.split()
        v1,s1 = split_cards(v[:5])
        v2,s2 = split_cards(v[5:])
        if winner(v1,s1,v2,s2) == 1:
            ans += 1
print(ans)

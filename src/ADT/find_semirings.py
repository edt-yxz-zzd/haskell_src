


'''
I define semiring as G=(*,+,E) where
    a(bc)=(ab)c
    a+(b+c)=(a+b)+c
    a(b+c)=ab+ac
    (a+b)c=ac+bc

define sum_id, sum_err, mul_id, mul_err as
    sum_id + a = a = a + sum_id
    sum_err + a = sum_err = a + sum_err
    mul_id * a = a = a * mul_id
    mul_err * a = mul_err = a * mul_err

    (both left/right id/err)

to find semirings with E = {sum_id, sum_err, mul_id, mul_err}
    and (a*b=b*a), (a+b=b+a) -- commutative
    since mul_err = mul_err * (a + b) = mul_err + mul_err
    so there 5 unknowns:
        mul_err + mul_id
        mul_id + mul_id
        {sum_err, sum_id} * {sum_err, sum_id}
        total 4**5 possibles


----
only one result! for 4 element commutative semirings
a * a = a
a + a = a
(+):
    sum_id < [mul_id|mul_err] < sum_err
    mul_err + mul_id = sum_err
(*):
    mul_id < [sum_id|sum_err] < mul_err
    sum_id + sum_err = mul_err


max??
    e.g. sum_err is the min one s.t. sum_err >=+ both mul_id and mul_err
    a + b = min{c | c >+ a, c >+ b}
    a * b = min{c | c >* a, c >* b}
    a*(b+c) = max* a $ max+ b c
    a*b+a*c = max+ (max* a b) (max* a c)

    ==>> a+a=a=a*a
    NOTE: ==+ is not `is`
    [a <=* b][a <=* c] ==>> a*(b+c)=a*b+a*c=b+c <==> [a <=* b+c]
        since [b+c = max+ b c = b or c]
        ==>> [a <=* b+c] ==>> a*(b+c)=b+c=a*b+a*c
    [a >=* b][a >=* c] ==>> a*(b+c)=a*b+a*c=a+a=a <==> [a >=* b+c]
        since [b+c = max+ b c = b or c]
        ==>> [a >=* b+c] ==>> a*(b+c)=a=a+a=a*b+a*c
    [b <* a <* c] ==>>
        a*(b+c)=a*b+a*c=a+c
        [a+b == a or c]
            right = max+ a c = a or c
            left = max* a (b+c)
            [a <+ c]
                right = c
                [b <+ c] ==>> left = max* a c = c = right
                [b >+ c] ==>> left = max* a b = a =/= right ==>> error
                [b+c =/= b or c] ==>> c >=* b+c
            [a >+ c]
                right = a
                ==>> a == max* a (b+c) ==>> a >=* b+c
                [b <+ c] ==>> a >=* c ==>> error
                [b >+ c] ==>> a >=* b
                [b+c =/= b or c] ==>> a >= b+c
            [a <+ c][b <+ c] or [a <+ c][b+c >+ b and c][c >=* b+c]
            [a >+ c][b >+ c] or [a >+ c][b+c >+ b and c][a >=* b+c]
            <==> [a+b <+ c] or ...
            1) [a <+ c][b <+ c] ==>> a*(b+c) = a*c = c = c+(a or b) = a*c+a*b
            2) [a >+ c][b >+ c] ==>> a*(b+c) = a*b = (a or b)+c = a*b + a*c
            3) [a <+ c][b+c >+ b and c][c >=* b+c]
                ==>> a*(b+c) =?=  a*c+a*b = c+a = c
                    a*(b+c) <=* a*c = c = a*c+a*b
                    require [c <=* a*(b+c)]
            4) [a >+ c][b+c >+ b and c][a >=* b+c]
                ==>> a*(b+c) =?=  a*c+a*b = c+a = a
                    a*(b+c) <=* a*a = a = a*c+a*b
                    require [a <=* a*(b+c)]
        [d = a+c =/= a or c]
            ==>> ...


* = &&
    T < F
    U < X
+ = ||
    F < T
    X < U
flip pairs are {mul_id, sum_id} {sum_err, mul_err}
    ==>> (F,T) == (sum_id, mul_id) or (mul_err, sum_err)
    ==>> (X,U) == (mul_err, sum_err) or (sum_id, mul_id)
    1) bad
        sum_id = X
        mul_id = U
        mul_err = F
        sum_err = T
        &&: U < [X|T] < F
        ||: X < [U|F] < T
        X && T == F??
        U && T == T??
    2) good
        sum_id = F
        mul_id = T
        mul_err = X
        sum_err = U
        &&: T < [F|U] < X
        ||: F < [T|X] < U
        F && U == X
        X || T == U
'''

from itertools import product
from pprint import pprint

mul_id = 'mul_id'
mul_err = 'mul_err'
sum_id = 'sum_id'
sum_err = 'sum_err'
E = {mul_err, mul_id, sum_err, sum_id}

to_find_sums = [(mul_err, mul_id), (mul_id, mul_id)]
to_find_muls = [(sum_err, sum_err), (sum_id, sum_id), (sum_err, sum_id)]
def to_key(a,b):
    return frozenset([a,b])
def get(table, ab):
    a, b = ab
    return table[to_key(a,b)]
def set(table, ab, r):
    a, b = ab
    table[to_key(a,b)] = r

class Temp:
    def __init__(self):
        self.mul_table = {}
        self.sum_table = {}
        self.init_muls()
        self.init_sums()
    def mul(self, a,b):
        return get(self.mul_table, (a,b))
    def add(self, a,b):
        return get(self.sum_table, (a,b))
    def set_sum(self, ab, r):
        set(self.sum_table, ab, r)
    def set_mul(self, ab, r):
        set(self.mul_table, ab, r)
    def init_sums(tables):
        _set = tables.set_sum
        _set((mul_err, mul_err), mul_err)
        for a in E:
            _set((a,sum_id), a)
            _set((a,sum_err), sum_err)
    def init_muls(tables):
        _set = tables.set_mul
        for a in E:
            _set((a,mul_id), a)
            _set((a,mul_err), mul_err)
    def verify(self):
        return all(map(self.verifies, product(E, repeat=3)))
    def verifies(self, abc):
        return all(self.iter_verifies(abc))
    def iter_verifies(self, abc):
        mul = self.mul
        add = self.add
        a, b, c = abc
        yield add(add(a,b), c) == add(a,add(b,c))
        yield mul(mul(a,b), c) == mul(a,mul(b,c))
        yield mul(a, add(b,c)) == add(mul(a,b), mul(a,c))
        yield mul(add(b,c), a) == add(mul(b,a), mul(c,a))
    def __str__(self):
        return str((self.mul_table, self.sum_table))
    def pprint(self):
        sums = ls_table(E, self.add, '+')
        muls = ls_table(E, self.mul, '*')
        pprint(sums)
        pprint(muls)

def ls_table(E, table_op, op_str):
    ls = []
    for a,b in product(E, repeat=2):
        r = table_op(a,b)
        ls.append((r, op_str, (a,b)))
    return ls

def f():
    t = Temp()
    Lsum = len(to_find_sums)
    Lmul = len(to_find_muls)
    assert Lsum + Lmul == 5
    for rs in product(E, repeat=5):
        for r, ab in zip(rs[:Lsum], to_find_sums):
            t.set_sum(ab, r)
        for r, ab in zip(rs[Lsum:], to_find_muls):
            t.set_mul(ab, r)
        if t.verify():
            #print(t)
            t.pprint()


'''
[
 ('sum_err', '+', ('mul_err', 'mul_id')),
 ('mul_id', '+', ('mul_id', 'mul_id')),
]

[
 ('sum_id', '*', ('sum_id', 'sum_id')),
 ('mul_err', '*', ('sum_id', 'sum_err')),
 ('sum_err', '*', ('sum_err', 'sum_err'))
 ]
'''



'''
[('mul_err', '+', ('mul_err', 'mul_err')),
 ('sum_err', '+', ('mul_err', 'mul_id')),
 ('mul_err', '+', ('mul_err', 'sum_id')),
 ('sum_err', '+', ('mul_err', 'sum_err')),
 ('sum_err', '+', ('mul_id', 'mul_err')),
 ('mul_id', '+', ('mul_id', 'mul_id')),
 ('mul_id', '+', ('mul_id', 'sum_id')),
 ('sum_err', '+', ('mul_id', 'sum_err')),
 ('mul_err', '+', ('sum_id', 'mul_err')),
 ('mul_id', '+', ('sum_id', 'mul_id')),
 ('sum_id', '+', ('sum_id', 'sum_id')),
 ('sum_err', '+', ('sum_id', 'sum_err')),
 ('sum_err', '+', ('sum_err', 'mul_err')),
 ('sum_err', '+', ('sum_err', 'mul_id')),
 ('sum_err', '+', ('sum_err', 'sum_id')),
 ('sum_err', '+', ('sum_err', 'sum_err'))]

[('mul_err', '*', ('mul_err', 'mul_err')),
 ('mul_err', '*', ('mul_err', 'mul_id')),
 ('mul_err', '*', ('mul_err', 'sum_id')),
 ('mul_err', '*', ('mul_err', 'sum_err')),
 ('mul_err', '*', ('mul_id', 'mul_err')),
 ('mul_id', '*', ('mul_id', 'mul_id')),
 ('sum_id', '*', ('mul_id', 'sum_id')),
 ('sum_err', '*', ('mul_id', 'sum_err')),
 ('mul_err', '*', ('sum_id', 'mul_err')),
 ('sum_id', '*', ('sum_id', 'mul_id')),
 ('sum_id', '*', ('sum_id', 'sum_id')),
 ('mul_err', '*', ('sum_id', 'sum_err')),
 ('mul_err', '*', ('sum_err', 'mul_err')),
 ('sum_err', '*', ('sum_err', 'mul_id')),
 ('mul_err', '*', ('sum_err', 'sum_id')),
 ('sum_err', '*', ('sum_err', 'sum_err'))]
'''



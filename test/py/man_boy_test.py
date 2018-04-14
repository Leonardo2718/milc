#! /usr/bin/python3
def a(k, x1, x2, x3, x4, x5):
    def b():
        def c():
            return a(k, x1, x2, x3, x4, x5)
        nonlocal k
        k -= 1
        return a(k, c(), x1, x2, x3, x4)
    return x4 + x5 if k <= 0 else b()

for i in range(0,8):
    print(a(i,1,-1,-1,1,0))

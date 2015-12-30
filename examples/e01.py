# return something using a global var
x = 2
def test1(a, b):
    return a + b + x

# no return here, nil as a return value
def test2(a, b):
    a + b

# just a few funcalls
def test3(a, b):
    return length(concat(a, b))

# set a global var
def test4():
    global z
    z = 10

# a simple while loop
def test5():
    while 10:
        while 11:
            break
        break

# a simple while loop using a continue
def test6():
    a = 1
    while 10:
        a = a + 1
        if a < 10: continue
        break
    return a

# if/elif/else
def test7(a):
    if a < 5:
        return -1
    elif a == 5:
        return 0
    else:
        return 1

# two string syntaxes
def test8():
    return concat("double" ,'single')

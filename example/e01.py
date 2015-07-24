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

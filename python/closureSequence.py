def makeSucc(n):
	def succ():
		return n, makeSucc(n+1)
	return succ

def makeFibSucc(n1, n2):
	def succ():
		return n1 + n2, makeFibSucc(n1 + n2, n1)
	return succ

# Data Seq a = a :> Seq a
# type Seq a = () -> (a, Seq a)
# (s, s -> a, s -> s) -> Seq a
def makeGenSucc(initialState, useState, nextState):
	def succ():
		return useState(initialState), makeGenSucc(nextState(initialState), useState, nextState)
	return succ

def succIter(succ):
	while True:
		n, succ = succ()
		yield n

def take(n, iterable):
	i = 1
	for x in iterable:
		if i > n:
			break
		i += 1
		yield x

succ = makeFibSucc(1,1)

for x in take(10, succIter(succ)):
	print(x)

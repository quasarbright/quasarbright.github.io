def scanl(iterable, combiner, initial=None):
	iterator = iter(iterable)
	if initial is None:
		initial = next(iterator)
	acc = initial
	for x in iterator:
		acc = combiner(acc, x)
		yield acc

def integral(iterable):
	return scanl(iterable, lambda x,y: x + y)

def pairs(iterable):
	first, rest = uncons(iterable)
	for x in rest:
		yield (first, x)
		first = x

def derivative(iterable):
	return map(lambda p: p[1]-p[0], iterable)

def chain(*iterables):
	for iterable in iterables:
		for x in iterable:
			yield x

def take(iterable, n):
	i = 1
	for x in iterable:
		if i > n:
			break
		else:
			i += 1
			yield x

def drop(iterable, n):
	i = 1
	for x in iterable:
		if i < n:
			continue
		else:
			yield x

def splitAt(iterable, n):
	return take(iterable,n), drop(iterable, n)

def uncons(iterable):
	iterator = iter(iterable)
	first = next(iterator)
	return first, iterator

def shiftedWindow(iterable, r):
	"""(abcd,2) -> [ab,bc,cd]
	"""
	first, rest = splitAt(iterable, r)
	first = list(first)
	if len(first) < r:
		return
	yield first
	for x in rest:
		first = first[1:] + [x]
		yield first

def count(n=0):
	while True:
		yield n
		n += 1

def cycle(xs):
	while True:
		for x in xs:
			yield x

def repeat(x, n=None):
	if n is None:
		while True:
			yield x
	else:
		for i in range(n):
			yield x

def filterIter(pred, iterable):
	for x in iterable:
		if pred(x):
			yield x

def dropWhile(pred, iterable):
	done = False
	for x in iterable:
		if not pred(x):
			done = True
		if done:
			yield x

def takeWhile(pred, iterable):
	for x in iterable:
		if pred(x):
			yield x
		else:
			break

def mapIter(f, iterable):
	for x in iterable:
		yield f(x)

def groupBy(iterable, key=None):
	if key is None:
		key = lambda x: x
	iterator = iter(iterable)
	first = next(iterator)
	currItems = [first]
	currKey = key(first)
	for x in iterator:
		newKey = key(x)
		if currKey == newKey:
			currItems.append(x)
		else:
			yield (currKey, currItems)
			currItems = [x]
			currKey = newKey
	yield (currKey, currItems)

def islice(iterable, start, stop=None, step=1):
	iterator = iter(iterable)
	for _ in range(start):
		next(iterator)
	i = start
	if i >= stop:
		return
	off = 0
	for x in iterable:
		if i % step == 0:
			yield x
		i += 1
		off += 1
		if i >= stop:
			break

def zip_shortest(*iterables):
	iterators = [iter(iterable) for iterable in iterables]
	while True:
		xs = []
		for iterator in iterators:
			try:
				xs.append(next(iterator))
			except StopIteration:
				return
		yield tuple(xs)

def product(iterable):
	for x in iterable:
		for y in iterable:
			yield x, y

# def permutations(iterable, r=None):
# 	"""not quite in right order"""
# 	if r is None:
# 		r == len(iterable)
# 	if r == 0:
# 		return [[]]
# 	first, rest = uncons(iterable)
# 	child_perms = permutations(rest, r-1)
# 	for i in range(r):
# 		for child_perm in child_perms:
# 			yield child_perm[:i] + [first] + child_perm[i:]
# 	yield permutations(rest, r)

def permutations(iterable, r=None):
    pool = tuple(iterable)
    n = len(pool)
    r = n if r is None else r
    for indices in product(range(n), repeat=r):
        if len(set(indices)) == r:
            yield tuple(pool[i] for i in indices)

def sub_sequences(iterable, r=None):
	if r is None:
		r == len(iterable)
	if r == 0:
		return [[]]
	first, rest = uncons(iterable)
	child_sub_sequences = sub_sequences(rest, r-1)
	for child_sub_sequence in child_sub_sequences:
		yield [first] + child_sub_sequence
	for x in sub_sequences(rest, r):
		yield x

def apply_forever(f, n):
	while True:
		yield n
		n = f(n)

def unique(iterable):
	seen = set()
	for x in iterable:
		if x not in seen:
			yield x
		seen.add(x)

def find_first(predicate, iterable):
	for x in iterable:
		if predicate(x):
			return x

def foldl(combiner, base, iterable):
	for x in iterable:
		base = combiner(base, x)
	return base



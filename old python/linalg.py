import math, pdb, sys
class Vector(list):
    def __str__(self):
        maxlen = 0
        for e in self:
            maxlen = max(maxlen,len(str(round(e,5))))
        strarr = []
        for e in self:
            word = str(round(e,5))
            while len(word) < maxlen:
                word += ' '
            strarr.append(word)
        return '\n'.join(['| {0} |'.format(strarr[i]) for i in range(0,len(strarr))])
    def __repr__(self):
        return '<{0}>'.format(str(list(self)).strip('[]'))
    def __add__(self,other):
        assert len(self) == len(other)
        if type(other) == int or type(other) == float:
            temp = self.copy()
            for i in range(0,len(self)):
                temp[i] += other
            return temp
        else:
            return self.copy().add(other)
    def __mul__(self,other):
        #cross product or scalar multiplication
        if type(other) == int or type(other) == float:
            return self.copy().mult(other)
        else:
            assert len(self) == len(other)
            return self.copy().cross(other)
    def __or__(self,other):
        #dot product
        assert len(self) == len(other)
        return self.copy().dot(other)
    def __sub__(self,other):
        assert len(self) == len(other)
        return self + other*-1
    def add(self,other):
        assert len(self) == len(other)
        for i in range(0,len(self)):
            self[i] += other[i]
        return self
    def sub(self,other):
        assert len(self) == len(other)
        return self.add(other.copy().mult(-1))
    def dot(self,other):
        assert len(self) == len(other)
        if len(self) != len(other):
            raise Exception('dimensions of vectors must be equal')
        ans = 0
        for i in range(0,len(self)):
            ans += self[i] * other[i]
        return ans
    def cross(self,other):
        assert len(self) == len(other)
        a = self
        while len(a)<3:
            a.append(0)
        b = other
        while len(b)<3:
            a.append(0)
        x = a[1]*b[2]-a[2]*b[1]
        y = a[2]*b[0]-a[0]*b[2]
        z = a[0]*b[1]-a[1]*b[0]
        return Vector([x,y,z])
    def mag(self):
        s = 0
        for x in self:
            s += x*x
        return math.sqrt(s)
    def normalize(self):
        mag = self.mag()
        if not mag == 0:
            for i in range(0,len(self)):
                self[i] = self[i]/mag
        return self
    def mult(self,k):
        for i in range(0,len(self)):
            self[i] = self[i] * k
        return self
    def setMag(self,mag):
        self.normalize()
        self.mult(mag)
        return self
    def copy(self):
        return Vector([0+x for x in self])
    def rotate(self,angle):
        #currently onl works in 2D
        T = Matrix([ [math.cos(angle),math.sin(angle)] , [math.cos(angle+math.pi/2),math.sin(angle+math.pi/2)] ])
        self = T * self
        return self
class Matrix(list):
    @staticmethod
    def fromShape(rows,cols):
        return Matrix([[0 for c in range(0,cols)] for r in range(0,rows)])
    @staticmethod
    def ID(n):
        ans = []
        for x in range(0,n):
            row = [0 for i in range(0,n)]
            row[x] = 1
            ans.append(row)
        return Matrix(ans)
    def __init__(self,arr):
        list.__init__(self,arr)
        self.rows = len(self)
        self.cols = len(self[0])
        for x in self:
            if len(x) != self.cols:
                raise Exception('not a rectangular matrix')
    def __str__(self):
        maxlen = 0
        for l in self:
            for e in l:
                maxlen = max(maxlen,len(str(round(e,5))))
        strarr = []
        for l in self:
            row = []
            for e in l:
                word = str(round(e,5))
                while len(word) < maxlen:
                    word += ' '
                row.append(word)
            strarr.append(row)
        return '\n'.join([ '| {0} |'.format(' '.join( [e for e in row] )) for row in strarr ])
    def copy(self):
            return Matrix([[0+x for x in y] for y in self])
    def append(self,ele):
        list.append(self,ele)
        self.rows = len(self)
        self.cols = len(self[0])
    def __mul__(self,other):
        if type(other) == int or type(other) == float:
            ans = self.copy()
            for r in range(0,self.rows):
                for c in range(0,self.cols):
                    ans[r][c] *= other
            return ans
        elif type(other) == Vector:
            asMatrix = Matrix([list(other)]).transpose()
            return Vector((self * asMatrix).getCol(0))
        else:
            if self.cols != other.rows:
                raise Exception('matrix dimensions not valid for multiplication')
            ans = []
            for r in range(0,self.rows):
                row = []
                for c in range(0,other.cols):
                    prod = Vector(self[r]) | Vector(other.getCol(c))
                    row.append(prod)
                ans.append(row)
            return Matrix(ans)
    def __or__(self,other):
        assert self.rows == other.rows and self.cols == other.cols, '{0}x{1} vs {2}x{3}'.format(self.rows,self.cols,other.rows,other.cols)
        ans = 0
        for r in range(0,self.rows):
            for c in range(0,self.cols):
                ans += self[r][c] + other[r][c]
        return ans
    def __add__(self,other):
        # print self
        # print other
        # print '{0}x{1} vs {2}x{3}'.format(self.rows,self.cols,other.rows,other.cols)

        ans = self.copy()
        if type(other) == int or type(other) == float:
            for r in range(0,self.rows):
                for c in range(0,self.cols):
                    ans[r][c] += other
        else:
            assert self.rows == other.rows and self.cols == other.cols, '{0}x{1} vs {2}x{3}'.format(self.rows,self.cols,other.rows,other.cols)
            for r in range(0,self.rows):
                for c in range(self.cols):
                    ans[r][c] += other[r][c]
        return ans
    def __sub__(self,other):
        assert self.rows == other.rows and self.cols == other.cols, '{0}x{1} vs {2}x{3}'.format(self.rows,self.cols,other.rows,other.cols)
        return self + (other*-1)
    def getCol(self,c):
        return [x[c] for x in self]
    def transpose(self):
        return Matrix([self.getCol(c) for c in range(0,self.cols)])
    def det(self):
        if self.rows != self.cols:
            raise Exception('cannot calculate the determinant of a non-square matrix')
        def f(M):
            if M.rows == 1:
                return M[0][0]
            elif M.rows == 2:
                return M[0][0]*M[1][1]-M[0][1]*M[1][0]
            ans = 0
            sign = 1
            for i in range(0,M.rows):
                ans += sign * M[0][i] * f(M.minor(0,i))
                sign = sign * -1
        return f(self)
    def minor(self,r,c):
        ans = []
        for i in range(0,self.rows):
            row = []
            for j in range(0,self.cols):
                if i != r and j != c:
                    row.append(M[i][j])
            if row != []:
                ans.append(row)
        return Matrix(ans)
    def inverse(self):
        ans = []
        for r in range(0,self.rows):
            row = []
            sign = 1
            for c in range(0,self.cols):
                row.append(sign*self.minor(r,c).det())
                sign *= -1
            ans.append(row)
        return Matrix(ans).transpose()

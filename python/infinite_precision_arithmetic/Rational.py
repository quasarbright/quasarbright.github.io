from BigInt import *
class Rational:
    '''
    constructs a fraction, doesn't simplify
    immutable
    '''
    def __init__(self, numerator, denominator=BigInt(1)):
        if type(numerator) is not type(BigInt(0)) or type(denominator) is not type(BigInt(0)):
            raise TypeError('{0}, {1}'.format(type(numerator), type(denominator)))
        if denominator == BigInt(0):
            raise ZeroDivisionError('Rational({0}, {1})'.format(numerator, denominator))
        if numerator == BigInt(0):
            self.numerator = numerator
            self.denominator = BigInt(1)
            self.positive = True
        else:
            self.numerator = abs(numerator)
            self.denominator = abs(denominator)
            self.positive = numerator.positive == denominator.positive


    def __str__(self):
        if self.positive:
            return 'Rational({0} / {1})'.format(''.join(self.numerator.digits), ''.join(self.denominator.digits))
        else:
            return 'Rational(-{0} / {1})'.format(''.join(self.numerator.digits), ''.join(self.denominator.digits))


    def __eq__(self, other):
        if type(self) is not type(other):
            raise TypeError('{0} {1}'.format(type(self), type(other)))
        # check signs and cross multiply
        return self.positive == other.positive and self.numerator * other.denominator == self.denominator * other.numerator


    def __lt__(self, other):
        if self.positive and not other.positive:
            return False
        if other.positive and not self.positive:
            return True
        if self == other:
            return False
        # same sign and unequal

    def simplify(self):
        if self.numerator == BigInt(0):
            return self
        else:
            gcd = self.numerator.gcd(self.denominator)
            numerator = self.numerator // gcd
            denominator = self.denominator // gcd
            ans =  Rational(numerator, denominator)
            ans.positive = self.positive
            return ans


    def common_denominator(self, other):
        '''
        returns two rationals with the same denominator
        '''
        pass

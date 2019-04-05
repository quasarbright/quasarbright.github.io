import unittest
'''
an object is what an object has
hashmap of fields and methods

goals:
inheritance
dynamic dispatch
field and method access

tools: functions, lambdas, dictionaries
'''


def dot(obj, attribute_name, args=None):
    if attribute_name in obj['fields']:
        return obj['fields'][attribute_name]
    elif attribute_name in obj['methods']:
        # check if args is iterable
        try:
            iter(args)
            if isinstance(args, str):
                raise TypeError()
        except TypeError:
            raise TypeError('a list of arguments must be passed to a method')
        # check if args length is correct for the method
        f = obj['methods'][attribute_name]
        expected_len_args = f.__code__.co_argcount - 1
        if len(args) != expected_len_args:
            raise TypeError('{} takes {} positional arguments, but {} were given'.format(
                attribute_name, expected_len_args, len(args)))
        # everything is good, call the method and return output
        return obj['methods'][attribute_name](obj, *args)
    else:
        # this attribute doesn't exist in the object
        # TODO look for attribute in super type
        raise AttributeError(
            'unknown attribute for {}: {}'.format(obj, attribute_name))

# point = {
#     'fields': {
#         "x":3,
#         "y":4
#     },
#     'methods': {

#     }
# }


def make_point(x, y):
    point = {
        'fields': {
            'x': x,
            'y': y
        },
        'methods': {
            'mag': lambda this: (dot(this, 'x')**2 + dot(this, 'y')**2)**0.5
        }
    }
    return point


class TestOOP(unittest.TestCase):
    def setUp(self):
        self.point = make_point(3, 4)

    def test_field_access(self):
        self.assertEqual(dot(self.point, 'x'), 3)
        self.assertEqual(dot(self.point, 'y'), 4)
        with self.assertRaises(Exception, msg='a list of arguments must be passed to a method'):
            dot(self.point, 'mag')
        with self.assertRaises(AttributeError):
            dot(self.point, 'nonfield')

    def test_direct_method_use(self):
        self.assertEqual(dot(self.point, 'mag', []), 5.0)

    def test_method_arg_validation(self):
        with self.assertRaises(TypeError, msg='a list of arguments must be passed to a method'):
            dot(self.point, 'mag', 234)
        with self.assertRaises(TypeError, msg='a list of arguments must be passed to a method'):
            dot(self.point, 'mag', 'hey i am iterable')
        with self.assertRaises(TypeError):
            dot(self.point, 'mag', ['an argument where there should be none'])


if __name__ == '__main__':
    unittest.main()

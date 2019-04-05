'''
an object is what an object has
hashmap of fields and methods

goals:
inheritance
dynamic dispatch
field and method access

tools: functions, lambdas, dictionaries
'''

def dot(obj, attribute_name, *args):
    if attribute_name in obj['fields']:
        return obj['fields'][attribute_name]
    elif attribute_name in obj['methods']:
        return obj['methods'][attribute_name](*args)
    else:
        raise AttributeError('unknown attribute error: {}'.format(attribute_name))

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
            'x':x,
            'y':y
        },
        'methods': {

        }
    }
    return point

if __name__ == '__main__':
    point = make_point(3, 5)
    print(dot(point, 'x'))
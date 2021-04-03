import math
import os
import random
import re
import sys


class Vector:
    def __init__(self, x, y, i):
        self.x = x
        self.y = y
        self.i = i
    def __hash__(self):
        return hash((self.x, self.y))
    def __eq__(self, other):
        return (self.x, self.y) == (other.x, other.y)



def cross(a,b,c):
    return (a.x - b.x)*(b.y - c.y) - (a.y - b.y)*(b.x - c.x)

def lcirc(xs, i):
    return xs[i:] + xs[:i]

def is_convex(vectors):
    if len(vectors) < 4:
        return True
    else:
        cross_signs = [cross(a, b, c) > 0 for (a, b, c) in zip(lcirc(vectors, 2), lcirc(vectors, 1), vectors)]
        return all(cross_signs) or not any(cross_signs)

def is_in_polygon(vector, polygon):
    cross_signs = [cross(u,vector,v) > 0 for (u,v) in zip(lcirc(polygon,1), polygon)]
    return all(cross_signs) or not any(cross_signs)

def get_covering_polygon(vectors):
    vectors = set(vectors)
    good_shapes = []
    def test_shape(shape):
        return is_convex(shape) and all([is_in_polygon(vector, shape) for vector in vectors])
    def go(remaining_vectors, shape_so_far):
        if len(remaining_vectors) == 0 and test_shape(shape_so_far):
            good_shapes.append(shape_so_far)
        else:
            for new_shape_end in remaining_vectors:
                new_shape = shape_so_far[:]
                new_shape.append(new_shape_end)
                new_remaining_vectors = set(remaining_vectors)
                new_remaining_vectors.remove(new_shape_end)
                help(new_remaining_vectors, new_shape_end)
    go(vectors, [])
    return min(good_shapes, key=len)
            


budget = ... # budget for the month
adjustments = [...] # list of percentage adjustments for each day,
# 1 being 100% and the default
paces = [] # absolute paces (daily spending) for each day
total_spent_so_far = 0
for day in month: # 1,2,3,...31
    # how much to spend this day
    pace = (budget - total_spent_so_far) / (len(month) - day + 1)
    # factor in percent adjustment
    adjusted_pace = pace * adjustments[day]
    # set the pace for this day
    paces[day] = adjusted_pace
    # assume we spend this much today, and continue
    total_spent_so_far += adjusted_pace
return paces
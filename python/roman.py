import math
import os
import random
import re
import sys
import functools


def roman_to_int(s: str) -> int:
    """converts a roman numeral string to an integer
    """
    s = s.upper()
    s_len = len(s)
    values = {"I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000}

    # the roman numeral as an int
    ans = 0
    for i, letter in enumerate(s):
        if letter not in values:
            raise ValueError(
                "unknown roman numeral value: "+letter)
        rest = s[i:]
        if i < s_len - 1:
            next_letter = s[i+1]
            if next_letter not in values:
                raise ValueError(
                    "unknown roman numeral value: "+next_letter)
            curr_val = values[letter]
            next_val = values[next_letter]
            if curr_val < next_val:
                ans -= curr_val
            else:
                ans += curr_val
        else:
            ans += values[letter]
    return ans

class Student:
    def __init__(self, name: str, grade: int, p_str: str):
        self.name = name
        self.grade = grade
        self.p_str = p_str
        self.p_score = roman_to_int(p_str)
    def __str__(self):
        return f"{self.name}, {self.grade}, {self.p_str}"
    def __repr__(self):
        return str(self)

def parse_student(line: str) -> Student:
    pat = r"(\w+ \w+), (\d+), ([IVXCDM]+)"
    match = re.match(pat, line)
    if match is not None:
        name, grade_str, p_str = match.groups()
        grade = int(grade_str)
        return Student(name, grade, p_str)
    else:
        raise ValueError("couldn't parse student: "+line)

def compare_students(s1: Student, s2: Student) -> int:
    if s1.p_score == s2.p_score:
        if s1.name == s2.name:
            # descending grade order, so reverse subtraction
            return s2.grade - s1.grade
        elif s1.name < s2.name:
            return -1
        else:
            return 1
    else:
        return s1.p_score - s2.p_score

def sort_students(students):
    return sorted(students, key=functools.cmp_to_key(compare_students))

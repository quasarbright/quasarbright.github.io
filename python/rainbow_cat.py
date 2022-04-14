import sys

lineNo = 0
for line in sys.stdin:
    print("\033[38;2;255;0;0m")
    print(line, end="")
    print("\033[0m")
    lineNo += 1

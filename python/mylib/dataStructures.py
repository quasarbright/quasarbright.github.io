class StackUnderFlowError(Exception):
    pass
class Stack(list):
    def isEmpty(self):
        return len(self) == 0
    def push(self,e):
        self.append(e)
    def pop(self):
        if len(self) == 0:
            raise StackUnderFlowError()
        else:
            return super().pop(len(self)-1)
    def peek(self):
        return self[-1]
class QueueUnderFlowError(Exception):
    pass
class Queue(object):
    def __init__(self,l=Stack([])):
        self.head = l
        self.tail = Stack([])
    def __str__(self):
        if self.tail.isEmpty():
            return str(self.head)
        else:
            return str(self.tail[::-1])
    def __repr__(self):
        return str(self)
    def enqueue(self,e):
        if self.tail.isEmpty():
            self.head.push(e)
        else:
            while not self.tail.isEmpty():
                try:
                    self.head.push(self.tail.pop())
                except StackUnderFlowError:
                    raise QueueUnderFlowError()
            self.head.push(e)
    def dequeue(self):
        if self.head.isEmpty():
            try:
                return self.tail.pop()
            except StackUnderFlowError:
                raise QueueUnderFlowError()
        else:
            while not self.head.isEmpty():
                try:
                    self.tail.push(self.head.pop())
                except StackUnderFlowError:
                    raise QueueUnderFlowError()
            try:
                return self.tail.pop()
            except StackUnderFlowError:
                raise QueueUnderFlowError()

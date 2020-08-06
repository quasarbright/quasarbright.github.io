memo = {}
def findSum(nums, target):
    '''nums is a (finite) sorted tuple of numbers, target is a number
    '''
    if (nums, target) in memo:
        return memo[(nums, target)]
    elif sum(nums) == target:
        memo[(nums, target)] = nums
        return nums
    else:
        for i in range(len(nums)):
            # "remove" a number from nums and see if that makes the sum
            newNums = nums[:i] + nums[i+1:]
            result = findSum(newNums, target)
            if result is not None:
                memo[(nums, target)] = result
                return result
    # we never found a sum
    memo[(nums, target)] = None
    return None

def insert(tup, num):
    for i, current in enumerate(tup):
        if num < current:
            return tup[:i] + (num,) + tup[i:]
    return tup + (num,)

def findSumFromStream(stream, target):
    nums = tuple()
    for num in stream:
        nums = insert(nums, num)
        result = findSum(nums, target)
        if result is not None:
            return result
    return None

print(findSumFromStream([1, 2, 7, 3], 6)) # prints (1, 2, 3)
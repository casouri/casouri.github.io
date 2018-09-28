print('\n\nBubble')

def bubble(lis):
    for last_num in range(len(lis) - 1, 0, -1):
        for index in range(0, last_num):
            if lis[index] > lis[index + 1]:
                tmp = lis[index]
                lis[index] = lis[index + 1]
                lis[index + 1] = tmp


alist = [54, 26, 93, 17, 77, 31, 44, 55, 20]
bubble(alist)
print(alist)


print('\n\nShort Bubble')

def short_bubble(lis):
    swapped_last_iter = True
    last_num = len(lis) - 1
    iter_count = 0
    while last_num and swapped_last_iter:
        iter_count += 1
        for index in range(0, last_num):
            if lis[index] > lis[index + 1]:
                swapped_last_iter = True
                tmp = lis[index]
                lis[index] = lis[index + 1]
                lis[index + 1] = tmp
            else:
                swapped_last_iter = False
        last_num -= 1
    print(iter_count)


alist = [20, 30, 40, 90, 50, 60, 70, 80, 100, 110]
short_bubble(alist)
print(alist)

print('\n\nSelection')

def selection(lis):
    # note that it's to 1, not 0
    # because we only need to place n to n-1 position
    # then 0 position will be left with the smallest number
    for index_of_last_num in range(len(lis) - 1, 1, -1):
        index_of_max = 0
        # look for max number ('s index)
        for index in range(0, index_of_last_num + 1):
            if lis[index] > lis[index_of_max]:
                index_of_max = index
        # swap
        tmp = lis[index_of_last_num]
        lis[index_of_last_num] = lis[index_of_max]
        lis[index_of_max] = tmp


alist = [54, 26, 93, 17, 77, 31, 44, 55, 20]
selection(alist)
print(alist)

print('\n\nInsertion')

def insertion(lis):
    for index_of_moving_num in range(1, len(lis)):
        moving_num = lis[index_of_moving_num]
        # this position is at the end of the sorted list
        position = index_of_moving_num

        # shift forward until reached the beginning of the list
        # or found the right position
        while position > 0 and lis[position - 1] > moving_num:
            # shift the number before back
            lis[position] = lis[position - 1]
            position -= 1

        lis[position] = moving_num


alist = [54, 26, 93, 17, 77, 31, 44, 55, 20]
insertion(alist)
print(alist)

print('\n\nShell')

def gap_insertion(lis, start_pos, gap):
    # same as insertion but step by sublist_count instead of 1
    for index in range(start_pos + gap, len(lis), gap):
        moving_num = lis[index]
        position = index
        while position >= gap and lis[position - gap] > moving_num:
            lis[position] = lis[position - gap]
            position = position - gap
        lis[position] = moving_num


def shell(lis):
    sublist_count = len(lis) // 2
    while sublist_count > 0:
        for start_pos in range(sublist_count):
            gap_insertion(lis, start_pos, sublist_count)
        print(
            'Sorted sublists of number %d and list looks like' % sublist_count)
        print(lis)
        sublist_count = sublist_count // 2


alist = [54, 26, 93, 17, 77, 31, 44, 55, 20]
shell(alist)
print(alist)

print('\n\nShell')

def merge(lis):
    print("Splitting:", lis)
    if len(lis) > 1:
        # split and merge
        mid = len(lis) // 2
        left = lis[:mid]
        right = lis[mid:]

        merge(left)
        merge(right)

        # merge them
        i = 0
        j = 0
        k = 0
        while i < len(left) and j < len(right):
            # before left and right run out
            # compare their smallest element (the first)
            # and insert the smaller one into the main list
            if left[i] < right[j]:
                lis[k] = left[i]
                i += 1
            else:
                lis[k] = right[j]
                j += 1
            k += 1
        # in case one list runs out before another,
        # which is likely to happen
        while i < len(left):
            lis[k] = left[i]
            i += 1
            k += 1
        while j < len(right):
            lis[k] = right[j]
            j += 1
            k += 1
    print("Merging:", lis)

alist = [54,26,93,17,77,31,44,55,20]
merge(alist)
print(alist)

print('\n\nQuick')

def quick(lis, first=None, last=None):
    # first and last default to 0 and len -1
    first = 0 if first == None else first
    last = len(lis) - 1 if last == None else last

    if first < last:
        # step 3
        split_point = partition(lis, first, last)
        # recursively sort
        quick(lis, first, split_point - 1)
        quick(lis, split_point + 1, last)

def partition(lis, first, last):
    pivot_value = lis[first]

    leftmark = first + 1
    rightmark = last

    done = False
    while not done:
        # step 3.1
        # move leftmark until hit a number greater than pivot value
        while leftmark <= rightmark and lis[leftmark] <= pivot_value:
            leftmark += 1
        # step 3.2
        # move rightmark until hit a number less than pivot value.
        while leftmark <= rightmark and lis[rightmark] >= pivot_value:
            rightmark -= 1

        if rightmark < leftmark:
            done = True
        else:
            # swap
            tmp = lis[rightmark]
            lis[rightmark] = lis[leftmark]
            lis[leftmark] = tmp

    # Now all numbers left of rightmark is smaller than pivot value
    # and all numbers left of leftmark is greater than pivot value
    tmp = lis[first] # pivot value
    lis[first] = lis[rightmark]
    lis[rightmark] = tmp

    return rightmark # return as split_point


alist = [54,26,93,17,77,31,44,55,20]
quick(alist)
print(alist)

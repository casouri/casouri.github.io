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

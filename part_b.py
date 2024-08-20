import functools
from functools import reduce



#part_b
#ex_1
print("exercise 1 : ")
def fibonacci_sequence(n):
    if n == 0: #if n=0 it should reutrn empty list beacuse first 0 elemnts
        return []
    elif n == 1:# if n=1 then need to return the first valuse of fiboncci sequence that is always 0
        return [0]
    else:
        return functools.reduce(lambda x, _: x + [x[-1] + x[-2]], range(n - 2), [0, 1])#reduce return a list

# Example
print(fibonacci_sequence(0))  # Output: []
print(fibonacci_sequence(1))  # Output: [0]
print(fibonacci_sequence(2))  # Output: [0, 1]
print(fibonacci_sequence(5))  # Output: [0, 1, 1, 2, 3]
print("\n")

#ex_2
print("exercise 2 : ")

def concatenate_strings(lst):
    # Check if the list is empty
    if lst:
        # concatenate the list of strings with spaces in between, reduce return a list
        return reduce(lambda x, y: f"{x} {y}", lst)
    else:
        # if list is empty return empty string ""
        return ""

# Example
strings = ["Hello", "from", "shani", "haker"]
result = concatenate_strings(strings)
print(result)
print("\n")


#ex_3
print("exercise 3 : ")

from functools import reduce

def cumulative_sum_of_squares(lists):
    return list(map(  # map to each sublist from list of lists
        lambda sublist: reduce(  # Second lambda - reduce to calculate cumulative sum
            lambda cumulative_sum, num: cumulative_sum + (lambda x: x**2)(num),  # square the number
            filter(  # filter to keep only even numbers
                lambda x: x % 2 == 0, sublist  # check if the number is even
            ),
            0  # Initial value for reduce
        ),
        lists
    ))

# Example
lists_of_numbers = [
    [1, 2, 3, 4],  # Even numbers: 2, 4 -> 2^2 + 4^2 = 4 + 16 = 20
    [5, 6, 7, 8],  # Even numbers: 6, 8 -> 6^2 + 8^2 = 36 + 64 = 100
    [9, 10, 11, 12]  # Even numbers: 10, 12 -> 10^2 + 12^2 = 100 + 144 = 244
]
result = cumulative_sum_of_squares(lists_of_numbers)
print(result)  # Output will be : [20, 100, 244]
print("\n")


# ex_4
print("\nExercise 4 : ")

from functools import reduce

def cumulative_operation(operation, initial):
    return lambda seq: reduce(operation, seq, initial)

# Function using the cumulative operation to calculate factorial
factorial = lambda n: cumulative_operation(lambda x, y: x * y, 1)(range(1, n + 1))

# Function using the cumulative operation to calculate exponentiation
exponentiation = lambda base, exp: (
    1 / cumulative_operation(lambda x, y: x * y, 1)([base] * abs(exp)) if exp < 0 #condition to work on negetive exponent
    else cumulative_operation(lambda x, y: x * y, 1)([base] * exp)
)

# Examples
print("\nexercise 4: ")
n = 5
print(f"Factorial of {n}: {factorial(n)}")

base, exp = 2, 3
print(f"{base} raised to the power of {exp}: {exponentiation(base, exp)}")

n = 0
print(f"Factorial of {n}: {factorial(n)}")

base, exp = 2, -1
print(f"{base} raised to the power of {exp}: {exponentiation(base, exp)}")


#ex_5
#idea of original code is to take the num list and for each element if it't even we enter it to even list
# for each element from even list we square it and enter to square list
#we sum all the elements from square list to sum_square and print it

#we rewrite the code in one line using filter, map, and reduce in one line:

print("\nExercise 5 : ")

result = reduce(lambda x, y: x + y, map(lambda x: x ** 2, filter(lambda x: x % 2 == 0, [1, 2, 3, 4, 5, 6])))
print(result)
#for the list it was provided num, the output is 56

#ex_6
print("\nExercise 6 : ")
def count_palindromes(lists_of_strings):
    return list(map(lambda sublist: reduce(lambda count, word: count + 1, filter(lambda x: x == x[::-1], sublist), 0), lists_of_strings))

#it reutrns a list that each element contain the count of palindrome in the sublist
# Example
lists_of_strings = [["madam", "hello", "level"], ["racecar", "world", "noon"], ["not", "a", "palindrome"]]
result = count_palindromes(lists_of_strings)
print(result)  # Output: [2, 2, 0]

#ex_8
print("\nExercise 8: ")

def filter_a_1nd_sort_primes(lst): #function that receive lst and return prime only in descending order
    return sorted([x for x in lst if (lambda x: x > 1 and all(x % i for i in range(2, int(x ** 0.5) + 1)))(x)], reverse=True)

# Example
nums = [10, 2, 3, 4, 5, 13, 17]
print(filter_a_1nd_sort_primes(nums))

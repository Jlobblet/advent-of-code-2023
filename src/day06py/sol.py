import os
from itertools import starmap
from functools import reduce
from operator import mul

def ways_to_win(time, distance):
    factor = next(i for i in range(time) if i * (time - i) > distance)
    return time - 2 * factor + 1


def parse_line(line):
    return [int(x) for x in line.split(": ")[-1].split()]


def parse(input):
    l1, l2 = input.strip().split("\n")
    l1, l2 = parse_line(l1), parse_line(l2)
    return zip(l1, l2)


def part1(input):
    return reduce(mul, starmap(ways_to_win, parse(input)))


def main():
    filename = os.environ["INPUT"]
    with open(filename) as file:
        input = file.read()

    print(part1(input))
    

if __name__ == "__main__":
    main()

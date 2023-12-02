from pathlib import Path

word_lut = {
    "one" : 1,
    "two" : 2,
    "three" : 3,
    "four" : 4,
    "five" : 5,
    "six" : 6,
    "seven" : 7,
    "eight" : 8,
    "nine" : 9,
}

for i in range(10):
    word_lut[str(i)] = i

def get_first(line, part):
    for i in range(len(line)):
        if part == 1:
            if line[i].isdigit():
                return int(line[i])
        else:
            for word, num in word_lut.items():
                if line[i:].startswith(word):
                    return num
            
def get_last(line, part):
    for i in reversed(range(len(line))):
        if part == 1:
            if line[i].isdigit():
                return int(line[i])
        else:
            for word, num in word_lut.items():
                if line[:i].endswith(word):
                    return num

with open("input/Day01.txt") as infile:
    lines = infile.readlines()

for part in (1, 2):
    total = 0
    for line in lines:
        total += 10 * get_first(line, part) + get_last(line, part)
    print(f"Part {part}: {total}")

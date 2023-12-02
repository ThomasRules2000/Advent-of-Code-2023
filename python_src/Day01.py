word_lut = {
    "one"   : 1,
    "two"   : 2,
    "three" : 3,
    "four"  : 4,
    "five"  : 5,
    "six"   : 6,
    "seven" : 7,
    "eight" : 8,
    "nine"  : 9,
}

digit_lut = {str(x): x for x in range(10)}

def get_first(line, lut):
    for i in range(len(line)):
        for word, num in lut.items():
            if line[i:].startswith(word):
                return num
            
def get_last(line, lut):
    for i in reversed(range(len(line))):
        for word, num in lut.items():
            if line[:i].endswith(word):
                return num

with open("input/Day01.txt") as infile:
    lines = infile.readlines()

for part, lut in enumerate((digit_lut, word_lut | digit_lut), start=1):
    total = 0
    for line in lines:
        total += 10 * get_first(line, lut) + get_last(line, lut)
    print(f"Part {part}: {total}")

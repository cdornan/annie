

import re
import sys

def regexGet(regex, groupIndex, subject):
    matches = re.findall(regex, subject)
    result = []
    if matches != None:
        for m in matches:
            if type(m) == type(""):
                result.append(m)
            else:
                result.extend([m[groupIndex] for m in matches])
    return result

def emailHighlighter(value):
    return regexGet("[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+", 0, value)
  # return regexGet("\\S+@\\S+\\.\\S+", 0, value)

def phoneNumberHighlighter(subject):
    return regexGet("[+]?[0-9]{8,13}",  0, subject)

def dutchPostalCodeHighlighter(subject):
    return regexGet("[0-9]{4} *[A-Za-z]{2}", 0, subject)

def namesHighlighter(subject):
    return regexGet("[A-Z][a-z]+ +[A-Z][a-z]+", 0, subject)

highlighters = [
    emailHighlighter,
    phoneNumberHighlighter,
    dutchPostalCodeHighlighter,
    namesHighlighter
]

def main():
    for file in sys.argv:
        print("Opening file ", file)
        with open(file, 'r') as inputFile:
            for line in inputFile:
                for highlighter in highlighters:
                    highlighter(line)


if __name__ == "__main__":
    main()

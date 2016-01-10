# Advent of Code

import sys

# Day 1: Not Quite Lisp
def day1():
	floor = 0
	basement = 0
	count = 0

	with open(sys.argv[1],'r') as f:
		instructions = f.read()

	for char in instructions:
		if char == "(":
			floor += 1
		else:
			floor -=1
		count += 1
		if basement == 0 and floor == -1:
			basement = count

	print("Floor: ",floor)
	print("Basement: ",basement)


# Day 2: I Was Told There Would Be No Math
def day2():
	paper = 0
	ribbon = 0

	with open(sys.argv[1],'r') as f:
		for line in f:
			dimensions = line.split('x')
			l = int(dimensions[0])
			w = int(dimensions[1])
			h = int(dimensions[2])
			side1 = l*w
			side2 = l*h
			side3 = w*h
			perim1 = l+l+w+w
			perim2 = l+l+h+h
			perim3 = w+w+h+h
			paper += 2*side1 + 2*side2 + 2*side3 + min([side1,side2,side3])
			ribbon += min([perim1,perim2,perim3]) + l*w*h

	print("Paper: ",paper)
	print("Ribbon: ",ribbon)




if __name__ == "__main__":
	#day1()
	day2()


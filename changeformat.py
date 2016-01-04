import csv

f = open('64peopleData.csv','r')
reader = csv.reader(f)
newData = []

for row in reader:
	newRow = row
	s1 = row[1]
	newRow[1] = s1[1]
	s7 = row[7]
	newRow[7] = s7[1]
	newData.append(newRow)
f.close()
firstRow = newData[0];
firstRow[1] = '一天用時'
firstRow[7] = '一月用量'
newData[0] = firstRow

f = open('64pDchanged.csv', 'w')
w = csv.writer(f)
w.writerows(newData)
f.close()

newData.pop(0)

points = [['speed_Mbps','price_NTD']]
for x in newData:
	print(x)
	x0 = int(x[0])
	k = 1.405 - 0.225*x0

	x7 = int(x[7])
	if x7 == 1:
		u = 150
	elif x7 == 2:
		u = 350
	elif x7 == 3:
		u = 750
	elif x7 == 4:
		u = 1.25*1024
	elif x7 == 5:
		u = 2*1024

	m = k*u
	x1 = int(x[1])
	h = x1
	t = 30*h*3600

	A = m/t/20

	for i in range(2,7):
		if i == 2:
			q = 0.1
		elif i == 3:
			q = 0.15
		elif i == 4:
			q = 0.5
		elif i == 5:
			q = 1.0
		elif i == 6:
			q = 1.5

		xi = int(x[i])
		scale = xi - 3

		scale = 1 + 0.1*scale
		

		P = q*(A*scale)

		point = [q, P]
		points.append(point)

f = open('64pDpoints.csv', 'w')
w = csv.writer(f)
w.writerows(points)
f.close()
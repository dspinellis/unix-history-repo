" p3

rad = 02000
diam = rad+rad
diam3 = diam-0200
middle: 0100000
high: 0177700-010000
low: 010000
ballinit: 0
	-nball
	dac 9f+t

" Q ball

	lac middle
	dac ball1+x
	lac low
	dac ball1+y
	dzm ball1+vx
	dzm ball1+vy
	dzm ball1+sflg
	jms put

" top row

	lac d1
	dac ball1+sflg
	lac high
	dac ball1+y
	-diam-diam-diam
	tad ball1+x
	dac ball1+x
	jms put
	jms put
	jms put
	jms put
	jms put

" second row

	-diam3
	tad ball1+y
	dac ball1+y
	-diam-diam-diam
	tad ball1+x
	dac ball1+x
	-diam-rad
	tad ball1+x
	dac ball1+x
	jms put
	jms put
	jms put
	jms put

" third row

	-diam3
	tad ball1+y
	dac ball1+y
	-diam-diam-diam-rad
	tad ball1+x
	dac ball1+x
	jms put
	jms put
	jms put

" fourth row

	-diam3
	tad ball1+y
	dac ball1+y
	-diam-diam-rad
	tad ball1+x
	dac ball1+x
	jms put
	jms put

" last row

	-diam3
	tad ball1+y
	dac ball1+y
	-diam-rad
	tad ball1+x
	dac ball1+x
	jms put

	jmp ballinit i

put: 0
	jms putball; ball1; 9f+t
	-diam-1
	cma
	tad ball1+x
	dac ball1+x
	isz 9f+t
	jmp put i
	jmp put i
t = t+1

getball: 0
	-1
	tad getball i
	dac 8
	isz getball
	lac getball i
	dac 9f+t
	lac nballp
	tad 9f+t i
	cll; mul; bvsize
	lacq
	tad listpm1
	dac 9
	-bvsize
	dac 9f+t
1:
	lac 9 i
	dac 8 i
	isz 9f+t
	jmp 1b
	isz getball
	jmp getball i

putball: 0
	-1
	tad putball i
	dac 8
	isz putball
	lac putball i
	dac 9f+t
	lac nballp
	tad 9f+t i
	cll; mul; bvsize
	lacq
	tad listpm1
	dac 9
	-bvsize
	dac 9f+t
1:
	lac 8 i
	dac 9 i
	isz 9f+t
	jmp 1b
	isz putball
	jmp putball i
t = t+1

" p1

t = 0
	las
	dac .sw
	law 13
	sys sysloc
	dac .pb
	jms ballinit
	lac nballp
	cll; mul; bvsize
	lacq
	tad listpm1
	dac 15
	tad d1
	sys capt
	law outline-1
	dac 8
	-noutline
	dac 9f+t
1:
	lac 8 i
	dac 15 i
	isz 9f+t
	jmp 1b
	lac 15
	dac displist
	lac o400000
	dac 15 i
	jms dump

loop:

" dump/restore

	lac waitup
	sza
	jmp 2f
	lac .pb i
	als 5; ral
	snl
	jmp 1f
	jms dump
	jmp 3f
1:
	sma
	jmp 3f
	jms restore
	jmp 3f
2:
	lac .pb i
	als 5; ral
	szl
	jmp 3f
	spa
	jmp 3f
	dzm waitup
3:
	sys time
	lacq
	sad stime
	jmp loop
	tad dm1
	sad stime
	jmp loop
	tad d1
	dac stime
	lac .pb i
	als 7
	spa
	sys exit

	lac displist
	dac 15

	-nball
	dac 9f+t

" q and stich controls

	jms getball; ball1; 9f+t
	lac ball1+vx
	lmq
	lac ball1+vy
	omq
	sza
	jmp 1f
	jms stickcont
	jms putball; ball1; 9f+t

1:
	jms getball; ball1; 9f+t

" if in pocket, ignore

	lac ball1+sflg
	spa
	jmp 4f
	lac 9
	dac 14

" update

	lac ball1+vx
	lrss 6
	tad ball1+x
	and o177777
	dac ball1+x
	lac ball1+vy
	lrss 6
	tad ball1+y
	and o177777
	dac ball1+y

" display

	lac ball1+x
	lrss 6
	xor o142000 " setx
	dac 15 i
	lac ball1+y
	lrss 6
	xor o164000 " sety
	dac 15 i

	law circle-1
	dac 8
	-circsize
	dac 9f+t+1
2:
	lac 8 i
	dac 15 i
	isz 9f+t+1
	jmp 2b

" degrade velocity
	jms frict

" edge collision

	lac lefttest
	tad ball1+x
	sma
	jmp 2f
	jms pocketlr
		jmp 4f
	lac ball1+vx
	sma
	jmp 2f
	cma
	tad d1
	dac ball1+vx
2:
	lac bottest
	tad ball1+y
	sma
	jmp 2f
	jms pockettb
		jmp 4f
	lac ball1+vy
	sma
	jmp 2f
	cma
	tad d1
	dac ball1+vy
2:
	lac righttest
	tad ball1+x
	spa
	jmp 2f
	jms pocketlr
		jmp 4f
	lac ball1+vx
	spa
	jmp 2f
	cma
	tad d1
	dac ball1+vx
2:
	lac toptest
	tad ball1+y
	spa
	jmp 2f
	jms pockettb
		jmp 4f
	lac ball1+vy
	spa
	jmp 2f
	cma
	tad d1
	dac ball1+vy
2:

" ball/ball collision

	lac 9f+t
	tad d1
	sma
	jmp 4f
	dac 9f+t+1

2:
	lac 14 i
	dac ball2+0
	lac 14 i
	dac ball2+1
	lac 14 i
	dac ball2+2
	lac 14 i
	dac ball2+3
	lac 14 i
	dac ball2+4
	lac ball2+sflg
	spa
	jmp 3f
	lac ball1+x
	cma
	tad ball2+x
	cma
	lmq
	gsm
	dac .+3
	lacq
	muls; ..
	dac 9f+t+3
	lrss 4
	sza
	jmp 3f
	lac ball1+y
	cma
	tad ball2+y
	cma
	lmq
	gsm
	dac .+3
	lacq
	muls; ..
	tad 9f+t+3
	lrss 4
	sza
	jmp 3f
	jms ballball
	jms putball; ball2; 9f+t+1
3:
	isz 9f+t+1
	jmp 2b
4:

	jms putball; ball1; 9f+t
	isz 9f+t
	jmp 1b

	lac o400000
	dac 15 i
	jmp loop
t = t+3

" p4

stickcont: 0

" display stick

	lac ball1+x
	lrss 6
	xor o142000 " setx
	dac 15 i
	lac ball1+y
	lrss 6
	xor o164000 " sety
	dac 15 i

	lac qcos
	lrss 8
	sma
	jmp 1f
	cma
	tad d1
	xor o2000 " minus
1:
	xor o100000 " vecx hold
	dac 15 i

	lac qsin
	lrss 8
	sma
	jmp 1f
	cma
	tad d1
	xor o2000 " minus
1:
	xor o124000 " vecy vis
	dac 15 i

" rotation

	lac .pb i
	rtl
	sma rar
	jmp 2f

" coarse rotation

	sma
	jmp 1f
	szl
	jmp 3f
	jms rotate; mcsin; ccos
	jmp 3f
1:
	snl
	jmp 3f
	jms rotate; csin; ccos
	jmp 3f

" fine rotation

2:
	sma
	jmp 1f
	szl
	jmp 3f
	jms rotate; mfsin; fcos
	jmp 3f
1:
	snl
	jmp 3f
	jms rotate; fsin; fcos
3:

" strike

	lac .pb i
	rtl; ral
	sma ral
	jmp 1f

	lac qcos
	dac ball1+vx
	lac qsin
	dac ball1+vy
	jmp stickcont i

1:	sma
	jmp stickcont i
	lac qcos
	lrss 1
	dac ball1+vx
	lac qsin
	lrss 1
	dac ball1+vy
	jmp stickcont i

rotate: 0
	lac rotate i
	dac 9f+t+1
	lac 9f+t+1 i
	dac 9f+t
	isz rotate
	lac rotate i
	dac 9f+t+1
	lac 9f+t+1 i
	dac 9f+t+1
	isz rotate

	lac qsin
	gsm
	dac .+3
	lac 9f+t+1
	muls; ..; llss 2
	dac 9f+t+2
	lac qcos
	gsm
	dac .+3
	lac 9f+t
	muls; ..; llss 2
	tad 9f+t+2
	dac 9f+t+3

	lac qsin
	gsm
	dac .+3
	lac 9f+t
	muls; ..; llss 2
	cma
	dac 9f+t+2
	lac qcos
	gsm
	dac .+3
	lac 9f+t+1
	muls; ..; llss 2
	tad 9f+t+2
	dac qcos
	lac 9f+t+3
	dac qsin
	jmp rotate i
t = t+4

prad: 02000
pocketlr: 0
	lac ball1+sflg
	sna
	jmp 2f
	-1
	tad prad
	cma
	tad bottest
	tad ball1+y
	spa
	jmp 3f
	lac prad
	tad toptest
	tad ball1+y
	sma
	jmp 3f
	-1
	tad middle
	cma
	tad ball1+y
	sma
	cma
	tad prad
	sma
	jmp 3f
2:
	isz pocketlr
	jmp pocketlr i
3:
	-1
	dac ball1+sflg
	jmp pocketlr i

pockettb: 0
	lac ball1+sflg
	sna
	jmp 2f
	-1
	tad prad
	cma
	tad lefttest
	tad ball1+x
	spa
	jmp 3f
	lac prad
	tad righttest
	tad ball1+x
	sma
	jmp 3f
2:
	isz pockettb
	jmp pockettb i
3:
	-1
	dac ball1+sflg
	jmp pockettb i

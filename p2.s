" p2

frict: 0
	las
	and d1
	sza
	jmp frict i
	lac ball1+vx
	cll; muls; frfac
	dac ball1+vx
	lac ball1+vy
	cll; muls; frfac
	dac ball1+vy
	gsm
	dac 9f+t
	lac ball1+vx
	gsm
	tad 9f+t
	tad minvx
	sma
	jmp frict i
	dzm ball1+vx
	dzm ball1+vy
	jmp frict i
t = t+1

ballball: 0
	llss 15 " x**2+y**2 in q
	cma
	tad o300000
	dac 1f; dac 2f
	lac ball2+y
	cma
	tad ball1+y
	cma
	cll; muls; 1:..; llss 6
	dac sin
	lac ball2+x
	cma
	tad ball1+x
	cma
	cll; muls; 2:..; llss 6
	dac cos

" calculate closing velocities

	lac ball1+vx
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	dac 9f+t
	lac ball1+vy
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	tad 9f+t
	dac vp1

	lac ball2+vx
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	dac 9f+t
	lac ball2+vy
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	tad 9f+t
	dac vp2
	cma
	tad vp1
	cma
	sma
	jmp ballball i

" calculate tangential velocities

	lac ball1+vx
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	dac 9f+t
	lac ball1+vy
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	cma
	tad 9f+t
	cma
	dac vt1

	lac ball2+vx
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	dac 9f+t
	lac ball2+vy
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	cma
	tad 9f+t
	cma
	dac vt2

" recalculate x,y velocities
" with interchanged closing components

	lac vp2
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	cma
	dac 9f+t
	lac vt1
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	tad 9f+t
	cma
	dac ball1+vx

	lac vp2
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	dac 9f+t
	lac vt1
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	tad 9f+t
	dac ball1+vy

	lac vp1
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	cma
	dac 9f+t
	lac vt2
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	tad 9f+t
	cma
	dac ball2+vx

	lac vp1
	gsm
	dac .+3
	lac sin
	muls; ..; llss 3
	dac 9f+t
	lac vt2
	gsm
	dac .+3
	lac cos
	muls; ..; llss 3
	tad 9f+t
	dac ball2+vy

	jmp ballball i
t = t+1

dump: 0
	lac o17
	sys creat; dmpname
	spa
	sys save
	dac waitup
	sys write; qsin; 1
	lac waitup
	sys write; qcos; 1
	lac nballp
	cll; mul; bvsize
	lacq
	dac 0f
	lac waitup
	sys write; list; 0:..
	lac waitup
	sys close
	jmp dump i

restore: 0
	sys open; dmpname; 0
	spa
	sys save
	dac waitup
	sys read; qsin; 1
	lac waitup
	sys read; qcos; 1
	lac nballp
	cll; mul; bvsize
	lacq
	dac 0f
	lac waitup
	sys read; list; 0:..
	sad 0b
	skp 
	sys save
	lac waitup
	sys read; dump; 1
	sza
	sys save
	lac waitup
	sys close
	jmp restore i

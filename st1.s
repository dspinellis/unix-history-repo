" space travel 1

t = 0

start:			"[------ stuff for gravity - scan markup]
	law 13
	sys sysloc
	dac .pbson
	lac pww
	dac 1f
	dac 2f
	lac pw
	dac 3f
	-32
	dac cplan
4:
	fld; 1:0
	fmp; 2:0
	-1
	tad aexp
	dac aexp
	fng
	fad; fpone
	fst; 3:0
	lac 1b
	tad d3
	dac 1b
	dac 2b
	lac 3b
	tad d3
	dac 3b
	isz cplan
	jmp 4b
	law dspl-1
	dac clistp
	dac lanflg
	dzm crflg
	dzm goflg
	dzm .pbsint
	dzm forflg
	dzm bacflg
	dzm dspflg	"[line connecting this instruction to... - scan markup]
	dzm locflg
	dzm locpar	"[... this instruction - scan markup]
	lac d1
	dac par
	jms dispname
	fld; prsq+4
	sqrt
	fst; rpar
	fst; y
	fst; oy
	fld; fpone
	fst; sphi
	fst; stheta
	jms dspsca
	fld; fpzero
	fst; x
	fst; ox
	fst; cphi
	fst; ctheta
	law displist
	sys capt
	jmp loop

loop:
	law dspl-1
	dac clistp
	jms contrl	"[arrow from below points to this instruction - scan markup]
	lac par
	jms absxy
	jms shipxy
	lac goflg
	spa
	jmp loop3	"[long dash after loop3 - scan markup]
	fld; fpzero
	fst; ax
	fst; ay
	fst; maxa
	lac nplan
	skp
loop1:
	lac cplan
	tad dm1
	spa
	jmp loop2
	dac cplan
	tad fppar
	dac fcplan
	lac i fcplan
	dac fcplan
	jms updacc
	jms displa
	lac cplan
	sza
	jms updpln
	lac cplan
	and o7
	sad o7
	jms contrl
	jmp loop1

loop2:
	lac lanflg
	sma
	jms updshp
loop3:
	fld; horizv	"[a box enclosing all code from loop3 to loop4 - scan markup]
	lac scale	"[arrow drawn from box up to location above - scan markup]
	cma
	tad vscale
	tad aexp
	dac aexp
	jms inscr
		jmp loop4
	tad o141577
	dac i clistp	"[crossed out with an arrow pointint to "dac dspvel" - scan markup]
	lac o164372	"[crossed out - scan markup]
	dac clistp i	"[crossed out - scan markup]
	cla		"[crossed out - scan markup]
	jms dsplanet	"[crossed out - scan markup]
loop4:			"[inside a drawn box - scan markup]
	sys time "put delay here.....
	dzm dispcl
	lac crflg
	sma
	jmp 1f
	lac o20714 "cl
	dac dispcl
	jmp 2f
1:
	lac lanflg
	sma
	jmp 2f
	lac o114 "l
	dac dispcl
2:
	lac dhalt
	dac i clistp
	jmp loop " check 2-display question

contrl: 0
	lac i .pbson
	xor .pbsint
	and .pbson i
	sna
	jmp noneon
	lmq
	spa ral
	sys exit
	sma
	jmp 1f
	dzm goflg
	dzm crflg
1:
	lacq
	als 6
	sma ral
	jmp 1f
	spa
	jmp noneon
	isz scale
	nop
	jms dspsca "uprange
	jmp noneon
1:
	sma
	jmp noneon
	-1
	tad scale
	dac scale
	jms dspsca "downrange
noneon:
	dzm forflg
	dzm bacflg
	lac i .pbson
	dac .pbsint
	als 2
	sma
	jmp 1f
	lac dhalt
	dac forflg
	lac goflg
	sma
	dzm lanflg
1:
	lac i .pbson
	als 3
	sma
	jmp 1f
	lac dhalt
	dac bacflg
	lac goflg
	sma
	dzm lanflg
1:
	lac i .pbson
	als 4
	sma
	jmp 1f
	ral
	spa
	jmp i contrl
	dzm 9f+t
	jmp 2f
1:
	ral
	sma
	jmp i contrl
	lac dhalt
	dac 9f+t
2:
	fld; cphi
	fmp; sdphi
	lac 9f+t
	sma
	fng
	fst; ftmp1
	fld; sphi
	fmp; cdphi
	fad; ftmp1
	fst; ftmp2
	fld; sphi
	fmp; sdphi
	lac 9f+t
	spa
	fng
	fst; ftmp1
	fld; cphi
	fmp; cdphi
	fad; ftmp1
	fst; cphi
	fld; ftmp2
	fst; sphi
	jmp i contrl

t = t+1

shipxy: 0
	fld; absx
	fad; x
	fng
	fst; shipx
	fld; absy
	fad; y
	fng
	fst; shipy
	jmp i shipxy

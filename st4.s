	" space travel 4

displa: 0
	lac locpar
	sad cplan
	skp
	jmp 2f
	lac locflg	"[arrow points here from the text "how to set?" - scan markup]
	sma
	jmp 1f
	fld; cphi	"[-\                     - scan markup]
	fmp; absy	"[  |			 - scan markup]
	fst; ftmp1	"[  |			 - scan markup]
	fld; sphi	"[  |			 - scan markup]
	fmp; absx	"[  |			 - scan markup]
	fad; ftmp1	"[  |			 - scan markup]
	fdv; dpar	"[  |			 - scan markup]
	fst; stheta	"[  \  lock calculation	 - scan markup]
	fld; sphi	"[  /			 - scan markup]
	fmp; absy	"[  |			 - scan markup]
	fng		"[  |			 - scan markup]
	fst; ftmp1	"[  |			 - scan markup]
	fld; cphi	"[  |			 - scan markup]
	fmp; absx	"[  |			 - scan markup]
	fad; ftmp1	"[  |			 - scan markup]
	fdv; dpar	"[  |			 - scan markup]
	fst; ctheta	"[  |			 - scan markup]
	jmp 2f		"[-/			 - scan markup]
1:
	fld; sphi
	fst; stheta
	fld; cphi
	fst; ctheta
2:
	fld; absx
	sfmp; ctheta
	fst; ftmp1
	fld; absy
	sfmp; stheta
	fad; ftmp1
	lac aexp
	cma
	tad scale
	cma
	dac aexp
	fst; spy
	dzm inflg
	jms inscr
		jmp 1f
	tad o145777
	dac clistp i
	jms rotx
	lac dhalt
	dac inflg
	jms inscr
		jmp 1f
	tad o161577
	dac i clistp
	lac cplan
	jms dsplanet
1:
	jms drcirc
	jmp i displa

rotx: 0
	fld; absx
	sfmp; stheta
	fst; ftmp1
	fld; absy
	sfmp; ctheta
	fng
	fad; ftmp1
	lac aexp
	cma
	tad scale
	cma
	dac aexp
	fst; spx
	jmp i rotx

surf: 0
	-1
	tad setx
	cma
	dac tsetx
	lac setx
	tad o141577
	dac i clistp
	-1
	tad sety
	cma
	dac tsety
	lac sety
	tad o165777
	dac clistp i
	lac narcs
	dac nt
	fld; wx
	fst; twx
	fld; wy
	fst; twy
	fld; v
	fng
	fst; v
2:
	fld; v
	sfmp; twy
	fng
	fst; ftmp1
	fld; vv
	sfmp; twx
	fad; ftmp1
	fst; ftmp2
	fld; v
	sfmp; twx
	fst; ftmp1
	fld; vv
	sfmp; twy
	fad; ftmp1
	fst; twy
	fad; spy
	jms inscr
		jmp 1f
	tad tsety
	dac dely
	cma
	tad d1
	tad tsety
	dac tsety
	fld; ftmp2
	fst; twx
	fad; spx
	jms inscr
		jmp 1f
	tad tsetx
	dac delx
	cma
	tad d1
	tad tsetx
	dac tsetx
	lac delx
	sma
	jmp .+3
	cma
	tad o41
	alss 6
	dac delx
	lac dely
	sma
	jmp .+3
	cma
	tad o41
	tad delx
	tad o220000
	dac i clistp
	isz nt
	jmp 2b
	jmp i surf
1:
	isz surf
	jmp i surf

drcirc: 0
	lac grvflg
	spa
	jmp i drcirc
	lac fcplan
	tad prsq
	dac .+2
	fld; ..
	sqrt
	lac aexp
	cma
	tad scale
	cma
	dac aexp
	fst; dtmp1
	fcp; thrs
	spa
	jmp i drcirc
	fng
	lac dpar
	cma
	tad scale
	cma
	dac dpar
	fad; dpar
	sfdv; dpar
	fst; dtmp2
	sfmp; spy
	fst; wy
	jms inscr
		jmp i drcirc
	dac sety
	lac inflg
	sma
	jms rotx
	fld; dtmp2
	sfmp; spx
	fst; wx
	jms inscr
		jmp i drcirc
	dac setx
	fld; spy
	fng
	fad; wy
	fst; wy
	fld; spx
	fng
	fad; wx
	fst; wx
	fld; dtmp1
	sfmp; pid10
	fcp; f400
	spa
	jmp 1f
	lac d400
	dac narcs
	jmp 2f
1:
	fix
	tad dm20
	spa
	cla
	tad d20
	dac narcs
	flt
2:
	fst; dtmp1
	-1
	tad narcs
	cma
	dac narcs
	fld; f2pi
	sfdv; dtmp1
	fst; v
	sfmp; v
	-1
	tad aexp
	dac aexp
	fng
	fad; fpone
	fst; vv
	lac o40004
	dac i clistp
	jms surf
		jmp i drcirc
	jms surf
		jmp i drcirc
	jmp i drcirc

	" space travel 3

updshp: 0
	lac forflg
	spa
	jmp .+4
	lac bacflg
	sma
	jmp 3f
	fld; ascale
	lac  forflg
	sma
	jmp 1f
	lac bacflg
	sma
	jmp 1f+1
	fld; fpzero
	jmp 2f
1:
	fng
	lac scale
	tad aexp
	dac aexp
	lac forflg
	sma
	jmp .+3
2:
	lac accflg	"[arrow pointing here from the text "how to set?" - scan markup]
	sma
	jmp .+3
	fad; maxa
	fng
	fst; ftmp1
	fmp; ctheta
	fad; ax
	fst; ax
	fld; ftmp1
	fmp; stheta
	fad; ay
	fst; ay
3:
	fld; ox
	fng
	fad; ax
	fst; ftmp1
	fld; x
	lac aexp
	tad d1
	dac aexp
	fad; ftmp1
	fst; ftmp1
	fld; x
	fst; ox
	fld; ftmp1
	fst; x
	fld; oy
	fng
	fad; ay
	fst; ftmp1
	fld; y
	lac aexp
	tad d1
	dac aexp
	fad; ftmp1
	fst; ftmp1
	fld; y
	fst; oy
	fld; ftmp1
	fst; y
	lac par
	sad maxj
	jmp i updshp
	jms absxy
	jms shipxy
	lac par
	jms absv
	fld; ox
	fng
	fad; x
	fad; absx
	fst; ox
	fld; oy
	fng
	fad; y
	fad; absy
	fst; oy
	lac maxj
	dac par
	jms absv
	fld; ox
	fng
	fad; absx
	fst; ox
	fld; oy
	fng
	fad; absy
	fst; oy
	lac par
	jms absxy
	fld; absx
	fad; shipx
	fng
	fst; x
	fad; ox
	fst; ox
	fld; absy
	fad; shipy
	fng
	fst; y
fadins:
	fad; oy
	fst; oy
	lac par
	tad fppar
	dac 1f
	lac i 1f
	tad prsq
	dac 1f
	fld; 1:..
	sqrt
	fst; rpar
	jms dspsca
	lac par
	jms dispname
	jmp i updshp

inscr: 0
	fng
	fix
	tad d383
	spa
	jmp i inscr
	tad dm768
	sma
	jmp i inscr
	isz inscr
	jmp i inscr

absxy: 0
	sna
	jmp 7f
	lmq
	lac fldins
	dac 2f-1
	dac 4f-1
	lacq
1:
	dac absi
	sna
	jmp i absxy
	tad fppar
	dac 9f+t
	lac i 9f+t
5:
	tad px
	dac 2f
fldins:
	fld; absx
	fad; 2:..
	fst; absx
	lac i 9f+t
6:
	tad py
	dac 4f
	fld; absy
	fad; 4:..
	fst; absy
	lac fadins
	dac 2b-1
	dac 4b-1
	lac absi
	tad ppar
	dac 9f+t
	lac i 9f+t
	jmp 1b
7:
	dzm absx
	dzm absx+1
	dzm absx+2
	dzm absy
	dzm absy+1
	dzm absy+2
	jmp i absxy
	t = t+1

	" space travel 2

absv: 0
	dzm absx
	dzm absx+1
	dzm absx+2
	dzm absy
	dzm absy+1
	dzm absy+2
1:
	dac absi
	sna
	jmp i absv
	tad fppar
	dac 9f+t
	jms invert
	fld; ftmp1
	fng
	fad; absx
	fst; absx
	fld; ftmp2
	fng
	fad; absy
	fst; absy
	jms invert
	fld; ftmp1
	fad; absx
	fst; absx
	fld; ftmp2
	fad; absy
	fst; absy
	lac absi
	tad ppar
	dac 9f+t
	lac i 9f+t
	jmp 1b

invert: 0
	lac i 9f+t
	dac fcplan
	tad pww
	dac 1f
	dac 2f
	lac fcplan
	tad px
	dac 3f
	lac fcplan
	tad py
	dac 4f
	fld; 1:..
	fng
	fst; 2:..
	jms updpln
	fld; 3:..
	fst; ftmp1
	fld; 4:..
	fst; ftmp2
	jmp i invert

	t = t+1

updpln: 0
	lac fcplan
	lmq
	tad px
	dac 1f
	dac 5f
	lacq
	tad py
	dac 3f
	dac 6f
	dac 0f
	lacq
	tad pw
	dac 2f
	dac 7f
	lacq
	tad pww
	dac 4f
	dac 8f

	fld; 1:..
	fst; ftmp1
	fmp; 2:..
	fst; ftmp2
	fld; 3:..
	fmp; 4:..
	fng
	fad; ftmp2
	fst; 5:..
	fld; 6:..
	fmp; 7:..
	fst; ftmp2
	fld; ftmp1
	fmp; 8:..
	fad; ftmp2
	fst; 0:..
	jmp updpln i

updacc: 0
	lac cplan
	sad par
	jmp upda2
	jms absxy
	fld; absx
	fad; shipx
	jmp 1f
upda2:
	fld; x
	fng
1:
	fst; absx
	fmp; absx
	fst; ftmp1
	lac cplan
	sad par
	jmp 1f
	fld; absy
	fad; shipy
	jmp 2f
1:
	fld; y
	fng
2:
	fst; absy
	fmp; absy
	fad; ftmp1
	fst; dtmp1
	sqrt
	fst; dpar
	lac cplan
	sad par
	skp
	jmp upda5
	fld; ox
	fng
	fad; x
	fst; ftmp1
	fmp; y
	fst; horizv
	fld; y
	fng
	fad; oy
	fst; ftmp2
	fmp; x
	fad; horizv
	fdv; dpar
	fst; horizv
	fld; dpar
	fcp; rpar
	sma
	jmp upda5
	lac lanflg
	spa
	jmp upda5
	fld; ftmp1
	fmp; ftmp1
	fst; ftmp1
	fld; ftmp2
	fmp; ftmp2
	fad; ftmp1
	fcp; crash
	spa
	jmp 1f
	lac dhalt
	dac goflg
	dac crflg
1:
	lac dhalt
	dac lanflg
	fld; rpar
	fdv; dpar
	fst; ftmp1
	fmp; x
	fst; x
	fst; ox
	fld;  ftmp1
	fmp; y
	fst; y
	fst; oy
	lac par
	jms absxy
	jms shipxy
	jmp upda2
upda5:
	fcp; fardst
	spa
	jmp 1f
	lac cplan
	sna
	jmp 1f
	lac dhalt
	dac grvflg
	jmp i updacc
1:
	dzm grvflg
	lac fcplan
	tad accl
	dac 1f
	fld; 1:..
	fdv; dtmp1
	fcp; maxa
	spa
	jmp 2f
	fst; maxa
	lac cplan
	dac maxj
2:
	fdv; dpar
	fst; ftmp1
	fmp; absx
	fad; ax
	fst; ax
	fld; ftmp1
	fmp; absy
	fad; ay
	fst; ay
	jmp i updacc

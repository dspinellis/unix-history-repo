/
/ NEGATION & ABSOLUTE VALUE
/
_ABS2:
	tst	(sp)
	bge	1f
_NEG2:
	neg	(sp)
1:
	sxt	-(sp)
	return
_ABS4:
	tst	(sp)
	bge	1f
_NEG4:
	mov	(sp)+,r0
	neg	r0
	neg	(sp)
	sbc	r0
	mov	r0,-(sp)
1:
	return
_ABS8:
	absf	(sp)
	return
_NEG8:
	negf	(sp)
	cfcc
	return

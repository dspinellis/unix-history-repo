/
/ CONVERSIONS
/
_STOI:
	tst	(sp)
	sxt	-(sp)
	return
_STOD:
	tst	(sp)
	sxt	-(sp)
_ITOD:
	movif	(sp)+,fr0
	movf	fr0,-(sp)
	return
_ITOS:
	tst	(sp)+
	return

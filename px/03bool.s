/
/ BOOLEAN OPERATIONS
/
_AND:
	tst	(sp)+
	bne	1f
	clr	(sp)
1:
	return
_OR:
	bis	(sp)+,(sp)
	return
_NOT:
	inc	(sp)
	bic	$!1,(sp)
	return

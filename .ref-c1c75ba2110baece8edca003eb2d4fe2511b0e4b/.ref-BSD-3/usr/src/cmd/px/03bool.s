#
# 03bool.s
#
# BOOLEAN OPERATIONS
#
_AND:
	incl	r10
	mcomw	(sp)+,r0
	bicw2	r0,(sp)
	jmp	(r8)
_OR:
	incl	r10
	bisw2	(sp)+,(sp)
	jmp	(r8)
_NOT:
	incl	r10
	xorw2	$1,(sp)
	jmp	(r8)

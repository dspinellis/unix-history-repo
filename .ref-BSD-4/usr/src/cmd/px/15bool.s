#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)15bool.s 4.1 10/10/80";
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

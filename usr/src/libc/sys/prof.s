# @(#)prof.s	4.1 (Berkeley) 12/21/80
# profil

	.set	prof,44
.globl	_profil
_profil:
	.word	0x0000
	chmk	$prof
	ret

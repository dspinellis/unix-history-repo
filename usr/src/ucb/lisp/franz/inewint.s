	.asciz	"@(#)inewint.s	35.1	5/6/81"
	.globl	_inewint
	.globl	_blzero
	.data	0
cntloc:	.long	0
	.text
_inewint:.word	0
#	movab	cntloc,r0		# used when profiling
#	jsb	mcount
	movl	4(ap),r0
#	cvtlb	r0,r0
#	bvs	nottiny
#	moval	Fixzero[r0],r0
#	ret
# nottiny:
	cmpl	r0,$1024
	jgeq	alloc
	cmpl	r0,$-1024
	jlss	alloc
	moval	Fixzero[r0],r0
	ret
alloc:
	calls	$0,_newint
	movl	4(ap),0(r0)
	ret
_blzero:				# blzero(where,howmuch)
					# char *where;
					# zeroes a block of length howmuch
					# beginning at where.
	.word	0
	movc5	$0,*4(ap),$0,8(ap),*4(ap)
	ret

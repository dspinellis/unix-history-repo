	.globl	_dmlad
#
#	routine for destructive multiplication and additon to a bignum by
#	two fixnums.
#
#	from C, the invocation is dmlad(sdot,mul,add);
#	where sdot is the address of the first special cell of the bignum
#	mul is the multiplier, add is the fixnum to be added (The latter
#	being passed by value, as is the usual case.
#
#
#	Register assignments:
#
#	r11 = current sdot
#	r10 = carry
#	r9  = previous sdot, for relinking.
#
_dmlad:	.word	0x0e00
	movl	4(ap),r11		#initialize cell pointer
	movl	12(ap),r10		#initialize carry
loop:	emul	8(ap),(r11),r10,r0	#r0 gets cell->car times mul + carry
#	ediv	$0x40000000,r0,r10,(r11)#cell->car gets prod % 2**30
#					#carry gets quotient
	extzv	$0,$30,r0,(r11)
	extv	$30,$32,r0,r10
	movl	r11,r9			#save last cell for fixup at end.
	movl	4(r11),r11		#move to next cell
	bneq	loop			#done indicated by 0 for next sdot
	tstl	r10			#if carry zero no need to allocate
	beql	done			#new bigit
	mcoml	r10,r3			#test to see if neg 1.
	bneq	alloc			#if not must allocate new cell.
	tstl	(r9)			#make sure product isn't -2**30
	beql	alloc
	movl	r0,(r9)			#save old lower half of product.
	brb	done
alloc:	calls	$0,_newsdot		#otherwise allocate new bigit
	movl	r10,(r0)		#store carry
	movl	r0,4(r9)		#save new link cell
done:	movl	4(ap),r0
	ret

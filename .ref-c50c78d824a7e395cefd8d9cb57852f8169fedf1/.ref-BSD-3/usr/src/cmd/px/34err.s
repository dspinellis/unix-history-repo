#
# 34err.s
#
	.set	ECHR,1
	.set	ESYSTEM,2
	.set	EBUILTIN,3
	.set	EHALT,4
	.set	ENILPTR,5
	.set	EPASTEOF,6
	.set	ESQRT,7
	.set	ESTKNEMP,8
	.set	ESUBSCR,9
	.set	EREFINAF,10
	.set	EWRITE,11
	.set	ENAMESIZE,12
	.set	ELN,13
	.set	EBADOP,14
	.set	EBADINUM,15
	.set	EGOTO,16
	.set	ECASE,17
	.set	ESEEK,18
	.set	ECREATE,19
	.set	EOUTOFMEM,20
	.set	ECTTOT,21
	.set	ESTLIM,22
	.set	ESTKOVFLO,23
	.set	EBADFNUM,24
	.set	EREMOVE,25
	.set	ECLOSE,26
	.set	EOPEN,27
	.set	EARGV,28
	.set	EPACK,29
	.set	EUNPACK,30
	.set	ERANGE,31
	.set	EASRT,32
	.set	EREADIT,33
	.set	EWRITEIT,34
	.set	EINTR,35
	.set	EASSIGN,36
	.set	EFIXADD,37
	.set	EFLTADD,38
	.set	EFIXSUB,39
	.set	EFLTSUB,40
	.set	EFIXMUL,41
	.set	EFLTMUL,42
	.set	EFIXDIV,43
	.set	EFLTDIV,44
	.set	EMODDIV,45
	.set	EFIXNEG,46
	.set	ELLIMIT,47
	.set	EFRAMESIZE,48
	.set	ETRASHHEAP,49
#
# Fielding interrupts and processing errors
#
# Process interpreter detected errors
#
error:
	movzwl	_perrno,-(sp)
	calls	$1,_error
	jmp	(r8)

_endinterpret:
	.byte	'e,'n,'d, 0

#
# Keyboard interrupts
#
	.align	1
	.globl	_intr
_intr:
	.word	0
	pushal	_intr		#reset interrupt signal
	pushl	$SIGINT
	calls	$2,_signal
	pushl	$EINTR
	calls	$1,_error
	ret
#
# Segmentation Violations => No more memory available for the stack
#
	.align	1
	.globl	_memsize
_memsize:
	.word	0
	pushl	$ESTKOVFLO
	calls	$1,_error
	ret
#
# Process computational errors
#
	.align	1
	.globl	_except
_except:
	.word	0
	pushal	_except		#reset signal
	pushl	$SIGFPE
	calls	$2,_signal
	movl	PC(fp),r0	#r0 has PC at point following error
	moval	errtbl-4,r1	#r1 points to error offset table
l3404:
	addl2	$4,r1		#determine cause of error
	cmpl	r0,(r1)+
	blssu	l3405		#not in table => system error
	cmpl	r0,(r1)+
	bgtru	l3404
	movzwl	(r1),-(sp)	#select error message
	brb	l3406
l3405:
	pushl	$ESYSTEM
l3406:
	calls	$1,_error
	ret
#
# Table of offsets and their associated errors
#
	.align	1
errtbl:
	.long	_AS2, _OFF, EASSIGN
	.long	_INX2, _NIL, ESUBSCR
	.long	_ADD2, _ADD28, EFIXADD
	.long	_ADD28, _SUB2, EFLTADD
	.long	_SUB2, _SUB28, EFIXSUB
	.long	_SUB28, _SQR2, EFLTSUB
	.long	_SQR2, _MUL28, EFIXMUL
	.long	_MUL28, _DIV2, EFLTMUL
	.long	_DIV2, _MOD2, EFIXDIV
	.long	_MOD2, _ABS2, EMODDIV
	.long	_ABS2, _ABS8, EFIXNEG
	.long	_DVD2, _IND1, EFLTDIV
	.long	_RANG2, _CASE1OP, ERANGE
	.long	_STOI, _UNDEF, EBUILTIN
	.long	_PACK, _UNPACK, EPACK
	.long	_UNPACK, _GET, EUNPACK
	.long	0xffffffff
#
# recover values of dp and lino from the stack
#
	.globl	_fetchdp

_fetchdp:
	.word	R2|R3|R4|R5|R6|R7|R8|R9|R10|R11
	pushl	fp		#sift through the stack to get the
	movl	sp,oldsp	# values of dp and lino
l3401:
	bicw3	$0xf000,MASK(fp),mask	#register save mask
	moval	REGS(fp),sp		#point to saved registers
	popr	mask			#pop them
	cmpl	PC(fp),$_interpret	#check for interpreter frame
	blss	l3402			#not found
	cmpl	PC(fp),$_endinterpret	#check for end of interpreter
	blss	l3403			#found
l3402:
	movl	FP(fp),fp	#get next frames registers
	jbr	l3401
l3403:
	movl	oldsp,sp	#restore current frame
	movl	(sp)+,fp
	movl	r9,*4(ap)	#return dp
	movl	r11,*8(ap)	#return lino
	ret
	.data
oldsp:	.space	4		#old value of sp
mask:	.space	2		#register pop mask
	.text

#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)34err.s 4.1 10/10/80";
#
	.set	EARGV ,1
	.set	EASRT ,2
	.set	EASSIGN ,3
	.set	EBADFNUM ,4
	.set	EBADINUM ,5
	.set	EBADOP ,6
	.set	EBUILTIN ,7
	.set	ECASE ,8
	.set	ECHR ,9
	.set	ECLOSE,10
	.set	ECREATE,11
	.set	EFIXADD,13
	.set	EFIXDIV,14
	.set	EFIXMUL,15
	.set	EFIXNEG,16
	.set	EFIXSUB,17
	.set	EFLTADD,18
	.set	EFLTDIV,19
	.set	EFLTMUL,20
	.set	EFLTSUB,21
	.set	EFMTSIZE,22
	.set	EGOTO,23
	.set	EHALT,24
	.set	EINTR,25
	.set	ELLIMIT,26
	.set	ELN,27
	.set	EMODDIV,28
	.set	ENAMESIZE,29
	.set	ENAMRNG,30
	.set	ENILPTR,31
	.set	ENUMNTFD,32
	.set	EOPEN,33
	.set	EOUTOFMEM,34
	.set	EPACK,35
	.set	EPASTEOF,36
	.set	ERANGE,37
	.set	EREADIT,38
	.set	EREFINAF,39
	.set	EREMOVE,40
	.set	ESEEK,41
	.set	ESQRT,42
	.set	ESTKNEMP,43
	.set	ESTKOVFLO,44
	.set	ESTLIM,45
	.set	ESUBSCR,46
	.set	ESYSTEM,47
	.set	ETRASHHEAP,48
	.set	EUNPACK,49
	.set	EWRITE,50
	.set	EWRITEIT,51
	.set	ECTLWR,12
	.set	ECTUPR,52
	.set	ECTSNG,53
	.set	ENARGS,54
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
# Die gracefully on unexpected signals
#
	.align	1
	.globl	_syserr
_syserr:
	.word	0
	pushl	$ESYSTEM
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
	movl	16(ap),r0	#r0 has PC at point following error
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
	.long	_ADD2, _ADD28, EFIXADD
	.long	_ADD28, _SUB2, EFLTADD
	.long	_SUB2, _SUB28, EFIXSUB
	.long	_SUB28, _MUL2, EFLTSUB
	.long	_MUL2, _MUL28, EFIXMUL
	.long	_MUL28, _DIV2, EFLTMUL
	.long	_DIV2, _MOD2, EFIXDIV
	.long	_MOD2, _ABS2, EMODDIV
	.long	_ABS2, _ABS8, EFIXNEG
	.long	_DVD2, _IND1, EFLTDIV
	.long	_STOI, _UNDEF, EBUILTIN
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

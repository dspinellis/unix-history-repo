/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)Katanf.s	7.1 (Berkeley) %G%
 */

#include "../tahoe/SYS.h"

	.text
ENTRY(Katanf, 0)
	tstl	4(fp)			# if (arg > 0)
	jleq	1f
	pushl	20(fp)			# hfs
	pushl	8(fp)
	pushl	4(fp)
	callf	$16,satan
	ret				# return(satan(arg));
1:					# else
	pushl	20(fp)			# hfs
	lnd	4(fp)
	pushd
	callf	$16,satan
	lnf	r0
	stf	r0
	ret				# return(-satan(-arg));

ASENTRY(satan, R2|R3)
	subl3	$60,fp,sp
	cmpd2	4(fp),_sq2m1		# if (arg < sq2m1)
	jgeq	1f
	pushl	12(fp)			# hfs
	pushl	8(fp)
	pushl	4(fp)
	callf	$16,xatan
	ret				# return(xatan(arg));
1:
	cmpd2	4(fp),_sq2p1		# else if (arg > sq2p1)
	jgtr	9f
	pushl	12(fp)			# hfs
	ldd	one; pushd
	ldd	4(fp); pushd
	callf	$24,_Kaddd		# (arg+1.0)
	pushl	12(fp)			# hfs of _Kdivd
	pushl	r1
	pushl	r0
	pushl	12(fp)			# hfs
	ldd	one; pushd
	ldd	4(fp); pushd
	callf	$24,_Ksubd		# (arg-1.0)
	pushl	r1
	pushl	r0
	callf	$24,_Kdivd		# (arg-1.0)/(arg+1.0)
	pushl	12(fp)			# hfs
	pushl	r1
	pushl	r0
	callf	$16,xatan		# xatan((ag-1.0)/(arg+1.0))
	pushl	12(fp)			# hfs
	pushl	r1
	pushl	r0
	ldd	_pio4; pushd
	callf	$24,_Kaddd
	ldd	r0; cvdf; stf r0
	ret				# return(pio4+xatan((xatan(...)));

9:
	pushl	12(fp)	
	ldd	4(fp); pushd
	ldd	one; pushd
	callf	$24,_Kdivd		# (1.0/arg)
	pushl	12(fp)			# hfs
	ldd	r0; pushd
	callf	$16,xatan
					# clrl	-60+4(fp)
					# movl	r0,-60(fp)
	pushl	12(fp)			# hfs
	ldd	r0; pushd
	ldd	_pio2; pushd
	callf	$24,_Ksubd
	ldd	r0; cvdf; stf r0
	ret

ASENTRY(xatan, 0)
	subl3	$68,fp,sp
	pushl	12(fp)			# hfs
	ldd	4(fp); pushd; pushd
	callf	$24,_Kmuld		# argsq = arg*arg;
	ldd	r0
	std	-60(fp)			# argsq
	pushl	12(fp)			# hfs
	pushd			
	ldd	_p5; pushd
	callf	$24,_Kmuld		# p5*argsq
	pushl	12(fp)			# hfs
	ldd	_p4; pushd			
	ldd	r0; pushd
	callf	$24,_Kaddd		# (p5*argsq+p4)
	pushl	12(fp)			# hfs
	ldd	-60(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld		# (p5*argsq+p4)*argsq
	pushl	12(fp)			# hfs
	ldd	_p3; pushd			
	ldd	r0; pushd
	callf	$24,_Kaddd		# ((p5*argsq+p4)*argsq+p3)
	pushl	12(fp)			# hfs
	ldd	-60(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld		# (..)*argsq
	pushl	12(fp)			# hfs
	ldd	_p2; pushd			
	ldd	r0; pushd
	callf	$24,_Kaddd		# (..)*argsq+p2)
	pushl	12(fp)			# hfs
	ldd	-60(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld		# ((..)*argsq+p2)*argsq
	pushl	12(fp)			# hfs
	ldd	_p1; pushd			
	ldd	r0; pushd
	callf	$24,_Kaddd		# ((..)*argsq+p2)*argsq+p1)
	pushl	12(fp)			# hfs
	ldd	-60(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld		# (..)*argsq
	pushl	12(fp)			# hfs
	ldd	_p0; pushd			
	ldd	r0; pushd
	callf	$24,_Kaddd		# ((..)*argsq+p1)*argsq+p0)
	pushl	12(fp)			# hfs
	ldd	4(fp); pushd
	ldd	r0; pushd
	callf	$24,_Kmuld		# (..)*arg
	ldd	r0
	std	-68(fp); cvdf; stf r0	# value
	ret

	.data
	.align	2
_sq2p1:	.long	0x411A8279, 0x99FCEF31 # .double 2.414213562373095
_sq2m1:	.long	0x3FD413CC, 0xCFE77990 # .double .41421356237309503
_pio2:	.long	0x40C90FDA, 0xA22168C1 # .double 1.5707963267948966
_pio4:	.long	0x40490FDA, 0xA22168C1 # .double .78539816339744829
_p0:	.long	0x407FFFFF, 0xFD687A4B # .double .99999999939652999
_p1:	.long	0xBFAAAAA2, 0x09F9DBF2 # .double -.3333330762079
_p2:	.long	0x3F4CC820, 0x0670059B # .double .199982166665
_p3:	.long	0xBF11D182, 0x6601878B # .double -.142400777317
_p4:	.long	0x3ED88B47, 0x4EFC9AF9 # .double .10573440275
_p5:	.long	0xBE772E4B, 0x0E689AEB # .double -.060346883
one:	.long	0x40800000, 0x00000000 # .double 1

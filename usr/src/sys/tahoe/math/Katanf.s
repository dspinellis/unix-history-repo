	.data
	.align	2
_sq2p1:.long	0x411A8279, 0x99FCEF31 # .double 2.414213562373095
	.align	2
_sq2m1:.long	0x3FD413CC, 0xCFE77990 # .double .41421356237309503
	.align	2
_pio2:.long	0x40C90FDA, 0xA22168C1 # .double 1.5707963267948966
	.align	2
_pio4:.long	0x40490FDA, 0xA22168C1 # .double .78539816339744829
	.align	2
_p0:.long	0x407FFFFF, 0xFD687A4B # .double .99999999939652999
	.align	2
_p1:.long	0xBFAAAAA2, 0x09F9DBF2 # .double -.3333330762079
	.align	2
_p2:.long	0x3F4CC820, 0x0670059B # .double .199982166665
	.align	2
_p3:.long	0xBF11D182, 0x6601878B # .double -.142400777317
	.align	2
_p4:.long	0x3ED88B47, 0x4EFC9AF9 # .double .10573440275
	.align	2
_p5:.long	0xBE772E4B, 0x0E689AEB # .double -.060346883
	.text
LL0:	.align	1
	.globl	_Katanf
	.set	L22,0x0
	.data
	.text
	.data
	.align	2
L35:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.data
	.align	2
L36:	.long	0x40800000, 0x00000000 # .double 1
	.align	2
L37:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.set	L28,0xC
	.data
	.text
	.set	L38,0x0
	.data
	.text
_Katanf:.word	L22
	tstl	4(fp)			# if (arg > 0)
	jleq	L27
	pushl	20(fp)			# hfs
	pushl	8(fp)
	pushl	4(fp)
	callf	$16,_satan
	ret				# return(satan(arg));
L27:					# else
	pushl	20(fp)			# hfs
	lnd	4(fp)
	pushd
	callf	$16,_satan
	lnf	r0
	stf	r0
	ret				# return(-satan(-arg));
L2000003:
	pushl	12(fp)	
	ldd	4(fp)
	pushd
	ldd	L35
	pushd
	callf	$24,_Kdivd		# (1.0/arg)
	pushl	12(fp)			# hfs
	ldd	r0
	pushd
	callf	$16,_xatan
					# clrl	-60+4(fp)
					# movl	r0,-60(fp)
	pushl	12(fp)			# hfs
	ldd	r0
	pushd
	ldd	_pio2
	pushd
	callf	$24,_Ksubd
	ldd	r0
	cvdf
	stf	r0
	ret

	.align	1
_satan: .word	L28
	subl3	$60,fp,sp
	cmpd2	4(fp),_sq2m1		# if (arg < sq2m1)
	jgeq	L33
	pushl	12(fp)			# hfs
	pushl	8(fp)
	pushl	4(fp)
	callf	$16,_xatan
	ret				# return(xatan(arg));
L33:	cmpd2	4(fp),_sq2p1		# else if (arg > sq2p1)
	jgtr	L2000003
	pushl	12(fp)			# hfs
	ldd	L37
	pushd
	ldd	4(fp)
	pushd
	callf	$24,_Kaddd		# (arg+1.0)
	pushl	12(fp)			# hfs of _Kdivd
	pushl	r1
	pushl	r0
	pushl	12(fp)			# hfs
	ldd	L36
	pushd
	ldd	4(fp)
	pushd
	callf	$24,_Ksubd		# (arg-1.0)
	pushl	r1
	pushl	r0
	callf	$24,_Kdivd		# (arg-1.0)/(arg+1.0)
	pushl	12(fp)			# hfs
	pushl	r1
	pushl	r0
	callf	$16,_xatan		# xatan((ag-1.0)/(arg+1.0))
	pushl	12(fp)			# hfs
	pushl	r1
	pushl	r0
	ldd	_pio4
	pushd
	callf	$24,_Kaddd
	ldd	r0
	cvdf
	stf	r0
	ret				# return(pio4+xatan((xatan(...)));

	.align	1
_xatan: .word	L38
	subl3	$68,fp,sp
	pushl	12(fp)			# hfs
	ldd	4(fp)
	pushd
	ldd	4(fp)
	pushd
	callf	$24,_Kmuld		# argsq = arg*arg;
	ldd	r0
	std	-60(fp)			# argsq
	pushl	12(fp)			# hfs
	pushd			
	ldd	_p5
	pushd
	callf	$24,_Kmuld		# p5*argsq
	pushl	12(fp)			# hfs
	ldd	_p4
	pushd			
	ldd	r0
	pushd
	callf	$24,_Kaddd		# (p5*argsq+p4)
	pushl	12(fp)			# hfs
	ldd	-60(fp)
	pushd
	ldd	r0
	pushd
	callf	$24,_Kmuld		# (p5*argsq+p4)*argsq
	pushl	12(fp)			# hfs
	ldd	_p3
	pushd			
	ldd	r0
	pushd
	callf	$24,_Kaddd		# ((p5*argsq+p4)*argsq+p3)
	pushl	12(fp)			# hfs
	ldd	-60(fp)
	pushd
	ldd	r0
	pushd
	callf	$24,_Kmuld		# (..)*argsq
	pushl	12(fp)			# hfs
	ldd	_p2
	pushd			
	ldd	r0
	pushd
	callf	$24,_Kaddd		# (..)*argsq+p2)
	pushl	12(fp)			# hfs
	ldd	-60(fp)
	pushd
	ldd	r0
	pushd
	callf	$24,_Kmuld		# ((..)*argsq+p2)*argsq
	pushl	12(fp)			# hfs
	ldd	_p1
	pushd			
	ldd	r0
	pushd
	callf	$24,_Kaddd		# ((..)*argsq+p2)*argsq+p1)
	pushl	12(fp)			# hfs
	ldd	-60(fp)
	pushd
	ldd	r0
	pushd
	callf	$24,_Kmuld		# (..)*argsq
	pushl	12(fp)			# hfs
	ldd	_p0
	pushd			
	ldd	r0
	pushd
	callf	$24,_Kaddd		# ((..)*argsq+p1)*argsq+p0)
	pushl	12(fp)			# hfs
	ldd	4(fp)
	pushd
	ldd	r0
	pushd
	callf	$24,_Kmuld		# (..)*arg
	ldd	r0
	std	-68(fp)			# value
	cvdf
	stf	r0
	ret

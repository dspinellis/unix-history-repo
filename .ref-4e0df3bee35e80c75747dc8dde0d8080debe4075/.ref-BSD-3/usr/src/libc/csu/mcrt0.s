# C runtime startoff including monitoring

	.set	exit,1
	.set	cbufs,300

.globl	start
.globl	_monitor
.globl	_sbrk
.globl	_main
.globl	_exit
.globl	_IEH3exit
.globl	_etext
.globl	_environ
.globl	__cleanup
.comm	countbase,4


start:
	.word	0x0000
	subl2	$8,sp
	movl	8(sp),(sp)  #  argc
	movab	12(sp),r0
	movl	r0,4(sp)  #  argv
L1:
	tstl	(r0)+  #  null args term ?
	bneq	L1
	cmpl	r0,*4(sp)  #  end of 'env' or 'argv' ?
	blss	L2
	tstl	-(r0)  # envp's are in list
L2:
	movl	r0,8(sp)  #  env
	movl	r0,_environ  #  indir is 0 if no env ; not 0 if env

	subl3	$eprol,$_etext,r1
	addl2	$7,r1
	extzv	$3,$16,r1,r1
	addl2	r1,r1		# tally size
	addl2	$8*cbufs+12,r1		# entrance count plus header
	pushl	$cbufs		# # entrance counters
	pushl	r1		# bufsiz
	pushl	r1		# for sbrk
	calls	$1,_sbrk
	cmpl	r0,$-1
	beql	nospace
# bandaid for sbrk not clearing memory (remove bandaid when fixed)
#	addl3	(sp),(sp),r1
#L100:
#	clrb	-1(r0)[r1]
#	sobgtr	r1,L100
# end bandaid
	pushl	r0
	addl3	$12,r0,countbase
	pushab	_etext
	pushab	eprol
	calls	$5,_monitor
	calls	$3,_main
	pushl	r0
	calls	$1,_exit

	.data
_environ:	.space	4
emsg:
	.byte	'N,'o,' ,'s,'p,'a,'c,'e,' ,'f,'o,'r,' 
	.byte	'm,'o,'n,'i,'t,'o,'r,' ,'b,'u,'f,'f,'e,'r,0xa,0x0
em1:
	.text

nospace:
	pushl	$em1-emsg
	pushab	emsg
	pushl	$2
	calls	$3,_write

_exit:
_IEH3exit:
	.word	0x0000
	calls	$0,__cleanup
	pushl	$0
	calls	$1,_monitor
	chmk	$exit
eprol:



# C runtime startoff

	.set	exit,1
.globl	_exit
.globl	start
.globl	_main
.globl	_environ
.globl	_xports
.globl _gstart
.globl _proflush


#
#	C language startup routine

#
#	special 512 byte area for nil (and possibly other atoms)
#	and special block of smallnums.
#
	.long	0
	.long	0
	.long	0
	.long	-4
	.long	20
	.byte	'n,'i,'l,0
	.long	0
	.long	0
	.long	-4
	.long	40
	.byte	'e,'o,'f,0
	.space 512-44
	.long	-128,-127,-126,-125,-124,-123,-122,-121
	.long	-120,-119,-118,-117,-116,-115,-114,-113
	.long	-112,-111,-110,-109,-108,-107,-106,-105
	.long	-104,-103,-102,-101,-100,-99,-98,-97
	.long	-96,-95,-94,-93,-92,-91,-90,-89
	.long	-88,-87,-86,-85,-84,-83,-82,-81
	.long	-80,-79,-78,-77,-76,-75,-74,-73
	.long	-72,-71,-70,-69,-68,-67,-66,-65
	.long	-64,-63,-62,-61,-60,-59,-58,-57
	.long	-56,-55,-54,-53,-52,-51,-50,-49
	.long	-48,-47,-46,-45,-44,-43,-42,-41
	.long	-40,-39,-38,-37,-36,-35,-34,-33
	.long	-32,-31,-30,-29,-28,-27,-26,-25
	.long	-24,-23,-22,-21,-20,-19,-18,-17
	.long	-16,-15,-14,-13,-12,-11,-10,-9
	.long	-8,-7,-6,-5,-4,-3,-2,-1
	.long	0,1,2,3,4,5,6,7
	.long	8,9,10,11,12,13,14,15
	.long	16,17,18,19,20,21,22,23
	.long	24,25,26,27,28,29,30,31
	.long	32,33,34,35,36,37,38,39
	.long	40,41,42,43,44,45,46,47
	.long	48,49,50,51,52,53,54,55
	.long	56,57,58,59,60,61,62,63
	.long	64,65,66,67,68,69,70,71
	.long	72,73,74,75,76,77,78,79
	.long	80,81,82,83,84,85,86,87
	.long	88,89,90,91,92,93,94,95
	.long	96,97,98,99,100,101,102,103
	.long	104,105,106,107,108,109,110,111
	.long	112,113,114,115,116,117,118,119
	.long	120,121,122,123,124,125,126,127
_xports:
	.long	__iob+0
	.long	__iob+16
	.long	__iob+32
	.long	__iob+48
	.long	__iob+64
	.long	__iob+80
	.long	__iob+96
	.long	__iob+112
	.long	__iob+128
	.long	__iob+144
	.long	__iob+160
	.long	__iob+176
	.long	__iob+192
	.long	__iob+208
	.long	__iob+224
	.long	__iob+240
	.long	__iob+256
	.long	__iob+272
	.long	__iob+288
	.long	__iob+304
	.space	512 - (20 * 4)

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
	calls	$3,_main
	pushl	r0
	calls	$1,_exit
	chmk	$exit
_gstart:
	.word	0
	moval	start,r0
	ret
_proflush:
	.word	0
	ret
#
	.data
_environ:	.space	4

/* 	file : Kfrexpf.x 
*/
	.data
	.text
LL0:	.align	1
	.globl	_Kfrexpf
	.data
	.align	2
L18:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.data
	.align	2
L21:	.long	0x40800000, 0x00000000 # .double 1
	.text
	.data
	.align	2
L22:	.long	0x41000000, 0x00000000 # .double 2
	.text
	.data
	.align	2
L25:	.long	0x40000000, 0x00000000 # .double .5
	.text
	.data
	.align	2
L28:	.long	0x40000000, 0x00000000 # .double .5
	.text
	.data
	.align	2
L29:	.long	0x41000000, 0x00000000 # .double 2
	.text
	.set	L12,0x0
	.data
	.text
_Kfrexpf:.word	L12
	subl3	$60,fp,sp
	clrl	-60(fp)		# j=0;
	clrl	-56(fp)		# neg=0;
	tstl	4(fp)		# if(x<0){
	jgeq	L16
	lnd	4(fp)
	std	4(fp)		# x = -x;
	movl	$1,-56(fp)	# neg=1;}
L16:	cmpd2	4(fp),L18	# if (x>1){
	jleq	L17
L19:	cmpd2	4(fp),L21	# while(x>1){
	jleq	L23
	addl2	$1,-60(fp)	# j=j+1;
	pushl	16(fp)		# hfs	
	ldd	L22
	pushd
	ldd	4(fp)
	pushd
	callf	$24,_Kdivd
	ldd	r0
	std	4(fp)		# x= x/2;
	jbr	L19
L17:	cmpd2	4(fp),L25	# if(x<0.5){
	jlss	L26
	jbr	L23
L2000001:
	subl2	$1,-60(fp)	# j = j-1;
	pushl	16(fp)		# hfs
	ldd	4(fp)
	pushd
	ldd	L29
	pushd
	callf	$24,_Kmuld
	ldd	r0
	std	4(fp)		# x = 2*x;
L26:	cmpd2	4(fp),L28	# while (x<0.5){
	jlss	L2000001
L23:	movl	-60(fp),*12(fp)	# *i=j;
	tstl	-56(fp)		# if (neg)
	jeql	L30
	lnd	4(fp)
	std	4(fp)
L30:	ldd	4(fp)
	cvdf
	stf	r0
	ret

.data
.text
LL0:.align	1
.globl	Xcmpf
.set	L12,0x1800
.data
.text
_cmpf:
Xcmpf:.word	L12
movl	4(fp),r12
movl	12(fp),r11
tstl	r12
jgeq	L16
xorl2	$-2147483648,r12
mnegl	r12,r12
L16:tstl	r11
jgeq	L19
xorl2	$-2147483648,r11
mnegl	r11,r11
L19:cmpl	r12,r11
jeql	L22
cmpl	r12,r11
jleq	L9999
movl	$1,r0
jbr	L9998
L9999:mnegl	$1,r0
L9998:ret
L22:clrl	r0
ret

 #
 # The assembler version is the output of cct for this whith minor editing
 # (_cmpf --> cmpf).
 #

 #cmpf(o1, o2)
 #	register o1, o2;
 #{
 #	if(o1 < 0) {
 #		o1 ^= 0x80000000;
 #		o1 = -o1;
 #	}
 #	if(o2 < 0) {
 #		o2 ^= 0x80000000;
 #		o2 = -o2;
 #	}
 #	if(o1 != o2)
 #		return(o1>o2 ? 1 : -1);
 #	return(0);
 #}

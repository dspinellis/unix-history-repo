.data
.text
LL0:.align	1
.globl	Xcmpd
.set	L12,0x1e00
.data
.text
_cmpd:
Xcmpd:.word	L12
movl	4(fp),r12
movl	8(fp),r10
movl	12(fp),r11
movl	16(fp),r9
tstl	r12
jgeq	L16
xorl2	$-2147483648,r12
tstl	r10
jeql	L17
mnegl	r10,r10
mcoml	r12,r12
jbr	L16
L17:mnegl	r12,r12
L16:tstl	r11
jgeq	L19
xorl2	$-2147483648,r11
tstl	r9
jeql	L20
mnegl	r9,r9
mcoml	r11,r11
jbr	L19
L20:mnegl	r11,r11
L19:cmpl	r12,r11
jeql	L22
cmpl	r12,r11
jleq	L9999
movl	$1,r0
jbr	L9998
L9999:mnegl	$1,r0
L9998:ret
L22:cmpl	r10,r9
jeql	L23
cmpl	r10,r9
jlequ	L9997
movl	$1,r0
jbr	L9996
L9997:mnegl	$1,r0
L9996:ret
L23:clrl	r0
ret

 #
 # The assembler version is the output of cct for this whith minor editing
 # (_cmpd --> cmpd).
 #

 #cmpd(hi1, lo1, hi2, lo2)
 #	register hi1, hi2;
 #	register unsigned lo1, lo2;
 #{
 #	if(hi1 < 0) {
 #		hi1 ^= 0x80000000;
 #		if(lo1) {
 #			lo1 = -lo1;
 #			hi1 = ~hi1;
 #		} else
 #			hi1 = -hi1;
 #	}
 #	if(hi2 < 0) {
 #		hi2 ^= 0x80000000;
 #		if(lo2) {
 #			lo2 = -lo2;
 #			hi2 = ~hi2;
 #		} else
 #			hi2 = -hi2;
 #	}
 #	if(hi1 != hi2)
 #		return(hi1>hi2 ? 1 : -1);
 #	if(lo1 != lo2)
 #		return(lo1>lo2 ? 1 : -1);
 #	return(0);
 #}

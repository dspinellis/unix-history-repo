s,"@(#)mc68000.sed 2.1 2/8/84",&,
s/jbsr._setup/.bss\
	.even\
Xl1:	.skip	8\
	.text/
s/jbsr._push2/subql	#2,sp/
s/jbsr._push4/subql	#4,sp/
s/jbsr._push8/subql	#8,sp/
s/jbsr._pushsze8/subql	#8,sp/
s/jbsr._pushsp/subl	sp@,sp\
	movl	sp,d0\
	addql	#4,d0\
	tstb	sp@(-100)/
s/jbsr._pop2/movw	sp@+,d0\
	extl	d0/
s/jbsr._pop4/movl	sp@+,d0/
s/jbsr._pop8/movl	sp@+,d0\
	movl	sp@+,d1/
s/jbsr._popsze8/movl	sp@+,Xl1\
	movl	sp@+,Xl1+4\
	movl	#Xl1,d0/
s/jbsr._popsp/addl	sp@,sp/
/jbsr._enableovrflo/d
/jbsr._disableovrflo/d

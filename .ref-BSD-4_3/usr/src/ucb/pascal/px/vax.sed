s,"@(#)vax.sed 2.1 2/8/84",&,
s/calls.\$0,_setup/.data\
	.align 2\
	.comm Ll1,8\
	.text/
s/calls.\$1,_push2/movw	(sp)+,(sp)/
/calls.\$1,_push4/d
/calls.\$2,_push8/d
/calls.\$2,_pushsze8/d
s/calls.\$1,_pushsp/subl2	(sp)+,sp\
	movl	sp,r0/
s/calls.\$0,_pop2/cvtwl	(sp)+,r0/
s/calls.\$0,_pop4/movl	(sp)+,r0/
s/calls.\$0,_pop8/movq	(sp)+,r0/
s/calls.\$0,_popsze8/movq	(sp)+,Ll1\
	movaq	Ll1,r0/
s/calls.\$1,_popsp/addl2	(sp)+,sp/
s/calls.\$0,_enableovrflo/bispsw	$0xe0/
s/calls.\$0,_disableovrflo/bicpsw	$0xe0/
s/*-4(fp)/(r11)/
s/-4(fp)/r11/

	.globl	dlab
	.text
	fmovemx	fp4-fp7,dlab
	fmovemx	fp4-fp7,sp@-
	fmovemx	d1,dlab
	fmovemx	d1,sp@-
	fmovemx	dlab,fp4-fp7
	fmovemx	sp@+,fp4-fp7
	fmovemx	dlab,d1
	fmovemx	sp@+,d1
	fmoveml	fpcr/fpsr,dlab
	fmoveml	dlab,fpcr/fpsr
	fmovemx	#0xf0,sp@-
	fmovemx sp@+,#0x0f
	fmovemx #0x0f,dlab
	fmovemx dlab,#0x0f
	.data
dlab:	.long	0

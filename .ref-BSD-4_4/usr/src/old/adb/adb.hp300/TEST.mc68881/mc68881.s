	.globl	tlab1,tlab2,dlab

	fabsl	dlab,fp1
	fabsx	fp2,fp3
	fabsx	fp4

	facosl	dlab,fp1
	facosx	fp2,fp3
	facosx	fp4

	faddl	dlab,fp1
	faddx	fp2,fp3

	fasinl	dlab,fp1
	fasinx	fp2,fp3
	fasinx	fp4

	fatanl	dlab,fp1
	fatanx	fp2,fp3
	fatanx	fp4

	fatanhl	dlab,fp1
	fatanhx	fp2,fp3
	fatanhx	fp4

	fbgt	here1
here1:	fbeq	here1
	fblt	here1
	fbgt	tlab2

	fcmpl	dlab,fp1
	fcmpx	fp2,fp3

	fcosl	dlab,fp1
	fcosx	fp2,fp3
	fcosx	fp4

	fcoshl	dlab,fp1
	fcoshx	fp2,fp3
	fcoshx	fp4

	fdbugt	d7,here2
here2:	fdbeq	d7,here2
	fdbult	d7,here2

	fdivl	dlab,fp1
	fdivx	fp2,fp3

	fetoxl	dlab,fp1
	fetoxx	fp2,fp3
	fetoxx	fp4

	fetoxm1l	dlab,fp1
	fetoxm1x	fp2,fp3
	fetoxm1x	fp4

	fgetexpl	dlab,fp1
	fgetexpx	fp2,fp3
	fgetexpx	fp4

	fgetmanl	dlab,fp1
	fgetmanx	fp2,fp3
	fgetmanx	fp4

	fintl	dlab,fp1
	fintx	fp2,fp3
	fintx	fp4

	fintrzl	dlab,fp1
	fintrzx	fp2,fp3
	fintrzx	fp4

	flog10l	dlab,fp1
	flog10x	fp2,fp3
	flog10x	fp4

	flog2l	dlab,fp1
	flog2x	fp2,fp3
	flog2x	fp4

	flognl	dlab,fp1
	flognx	fp2,fp3
	flognx	fp4

	flognp1l	dlab,fp1
	flognp1x	fp2,fp3
	flognp1x	fp4

	fmodl	dlab,fp1
	fmodx	fp2,fp3

	fmovel	dlab,fp1
	fmovel	fp5,dlab
	fmovep	fp6,dlab{d0}
	fmovep	fp6,dlab{#4}
	fmovel	dlab,fpcr
	fmovel	fpsr,dlab
	fmovecrx	#0,fp7
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

	fmull	dlab,fp1
	fmulx	fp2,fp3

	fnegl	dlab,fp1
	fnegx	fp2,fp3
	fnegx	fp4

	fnop

	freml	dlab,fp1
	fremx	fp2,fp3

	frestore	dlab
	fsave	dlab

	fscalel	dlab,fp1
	fscalex	fp2,fp3

	fsogl	dlab

	fsgldivl	dlab,fp1
	fsgldivx	fp2,fp3

	fsglmull	dlab,fp1
	fsglmulx	fp2,fp3

	fsinl	dlab,fp1
	fsinx	fp2,fp3
	fsinx	fp4

	.long	0xf2004031		| fsincosl	d0,fp1:fp0
	.long	0xf2000C31		| fsincosl	fp3,fp1:fp0
|	fsincosl	dlab,fp4:fp5
|	fsincosx	fp2,fp6:fp7

	fsinhl	dlab,fp1
	fsinhx	fp2,fp3
	fsinhx	fp4

	fsqrtl	dlab,fp1
	fsqrtx	fp2,fp3
	fsqrtx	fp4

	fsubl	dlab,fp1
	fsubx	fp2,fp3

	ftanl	dlab,fp1
	ftanx	fp2,fp3
	ftanx	fp4

	ftanhl	dlab,fp1
	ftanhx	fp2,fp3
	ftanhx	fp4

	ftentoxl	dlab,fp1
	ftentoxx	fp2,fp3
	ftentoxx	fp4

	ftrapngle
	ftrapnglew	#2
	ftrapnglel	#4

	ftstl	dlab
	ftstx	fp0

	ftwotoxl	dlab,fp1
	ftwotoxx	fp2,fp3
	ftwotoxx	fp4

tlab1:
	.space 0x10000
tlab2:
	fblt	tlab1

	rts

	.data
dlab:	.long 0

/ low core

fpp = 1
br4 = 200
br5 = 240
br6 = 300
br7 = 340
.globl	start

. = 0^.
	4
	br	1f

/ trap vectors
	trap; br7+0		/ bus error
	trap; br7+1		/ illegal instruction
	trap; br7+2		/ bpt-trace trap
	trap; br7+3		/ iot trap
	trap; br7+4		/ power fail
	trap; br7+5		/ emulator trap
	trap; br7+6		/ system entry

. = 040^.
1:	jmp	start

. = 060^.
	klin; br4
	klou; br4

. = 070^.
	pcin; br4
	pcou; br4

. = 100^.
	kwlp; br6
	kwlp; br6

. = 200^.
/	lpou; br4

. = 204^.
	rfio; br5

. = 214^.
	tcio; br6

. = 220^.
	rkio; br5

. = 224^.
	tmio; br5

. = 230^.
/	crin; br6

. = 240^.
	trap; br7+7		/ programmed interrupt
	trap; br7+10		/ floating point
	trap; br7+11		/ segmentation violation

. = 254^.
/	rpio; br5

/ floating vectors
/	1. kl br4
/	2. dc br5
/	3. dp br5
/	4. dm br5
/	5. dn br5
/	6. random junk
. = 300^.
	dcin; br5+0
	dcou; br5+0
	dcin; br5+1
	dcou; br5+1
	dcin; br5+2
	dcou; br5+2
	dcin; br5+3
	dcou; br5+3
	dcin; br5+4
	dcou; br5+4
	dcin; br5+5
	dcou; br5+5
	dcin; br5+6
	dcou; br5+6
	dcin; br5+7
	dcou; br5+7
	dcin; br5+10
	dcou; br5+10
	dcin; br5+11
	dcou; br5+11
	dcin; br5+12
	dcou; br5+12
	dcin; br5+13
	dcou; br5+13

	dpin; br6		/ DP-11
	dpou; br6

	dnou; br5		/ DN-11

. = 460^.
	vsin; br5
	vsou; br5
	vsin; br5
	vsou; br5

. = 500^.
	dr11ca; br5		/ 11/20 display
	dr11cb; br5
	dr11aa; br5		/ voice answerback
	dr11ab; br5
	catin; br5		/ typesetter
	catou; br5

//////////////////////////////////////////////////////
/ interface code to C

.globl	_clock
kwlp:
	jsr	r0,call; _clock

.globl	_rfintr
rfio:
	jsr	r0,call; _rfintr

.globl	_rkintr
rkio:
	jsr	r0,call; _rkintr

.globl	_tcintr
tcio:
	jsr	r0,call; _tcintr

.globl	_tmintr
tmio:
	jsr	r0,call; _tmintr

.globl	_klrint
klin:
	jsr	r0,call; _klrint

.globl	_klxint
klou:
	jsr	r0,call; _klxint

.globl	_pcrint
pcin:
	jsr	r0,call; _pcrint

.globl	_pcpint
pcou:
	jsr	r0,call; _pcpint

.globl	_dcrint
dcin:
	jsr	r0,call; _dcrint

.globl	_dcxint
dcou:
	jsr	r0,call; _dcxint

.globl	_dprint
dpin:
	jsr	r0,call; _dprint

.globl	_dpxint
dpou:
	jsr	r0,call; _dpxint

.globl	_dnint
dnou:
	jsr	r0,call; _dnint

.globl	_vsrintr
vsin:
	jsr	r0,call; _vsrintr

.globl	_vsxintr
vsou:
	jsr	r0,call; _vsxintr

.globl	_vtintr
dr11ca:
dr11cb:
	jsr	r0,call; _vtintr

.globl	_draaint
dr11aa:
	jsr	r0,call; _draaint

.globl	_drabint
dr11ab:
	jsr	r0,call; _drabint

.globl	_catintr
catin:
catou:
	jsr	r0,call; _catintr

SSR0 = 177572
SSR1 = 177574
SSR2 = 177576
.globl	_trap, _ssr
.globl	_nofault
trap:
	mov	PS,-4(sp)
	mov	SSR0,_ssr
	mov	SSR1,_ssr+2
	mov	SSR2,_ssr+4
	mov	$1,SSR0
	tst	_nofault
	bne	1f
	jsr	r0,call1; _trap
1:
	mov	_nofault,(sp)
	rti

PS = 177776
mfpi = 6500^tst
mtpi = 6600^tst
rti = 2
spl = 230

.globl	_runrun, _swtch, _u
ldfps = 170100^tst
stfps = 170200^tst
call1:
	tst	-(sp)
	spl	0
	br	1f

call:
	mov	PS,-(sp)
1:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	mfpi	sp
	mov	10.(sp),-(sp)
	bic	$!37,(sp)
	bit	$30000,PS
	beq	1f
	clrb	_runrun
	.if	fpp
	mov	$_u+4,r1
	stfps	(r1)+
	movf	fr0,(r1)+
	movf	fr4,fr0
	movf	fr0,(r1)+
	movf	fr5,fr0
	movf	fr0,(r1)+
	movf	fr1,(r1)+
	movf	fr2,(r1)+
	movf	fr3,(r1)+
	.endif
1:
	jsr	pc,*(r0)+
	bit	$30000,PS
	beq	1f
	tstb	_runrun
	beq	2f
	jsr	pc,_swtch
2:
	.if	fpp
	mov	$_u+4,r1
	mov	(r1)+,r0
	ldfps	r0
	movf	(r1)+,fr0
	movf	(r1)+,fr1
	movf	fr1,fr4
	movf	(r1)+,fr1
	movf	fr1,fr5
	movf	(r1)+,fr1
	movf	(r1)+,fr2
	movf	(r1)+,fr3
	ldfps	r0
	.endif
1:
	tst	(sp)+
	mtpi	sp
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	tst	(sp)+
	mov	(sp)+,r0
	rti

.data
.globl	_data, _edata, _end
_data:

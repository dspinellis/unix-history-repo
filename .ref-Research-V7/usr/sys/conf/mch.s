/ machine language assist
/ for 11/45 or 11/70 CPUs


/ non-UNIX instructions
mfpi	= 6500^tst
stst	= 170300^tst
mtpi	= 6600^tst
mfpd	= 106500^tst
mtpd	= 106600^tst
spl	= 230
ldfps	= 170100^tst
stfps	= 170200^tst
wait	= 1
rtt	= 6
reset	= 5

.PROFIL	= 0
HIPRI	= 340
HIGH	= 7
	.if	.PROFIL
HIGH	= 6
HIPRI	= 300
	.endif

/ Mag tape dump
/ save registers in low core and
/ write all core onto mag tape.
/ entry is thru 44 abs

.data
.globl	dump
dump:

/ save regs r0,r1,r2,r3,r4,r5,r6,KIA6
/ starting at abs location 4

	mov	r0,4
	mov	$6,r0
	mov	r1,(r0)+
	mov	r2,(r0)+
	mov	r3,(r0)+
	mov	r4,(r0)+
	mov	r5,(r0)+
	mov	sp,(r0)+
	mov	KDSA6,(r0)+

/ dump all of core (ie to first mt error)
/ onto mag tape. (9 track or 7 track 'binary')

.if HTDUMP
	mov	$HTCS1,r0
	mov	$40,*$HTCS2
	mov	$2300,*$HTTC
	clr	*$HTBA
	mov	$1,(r0)
1:
	mov	$-512.,*$HTFC
	mov	$-256.,*$HTWC
	movb	$61,(r0)
2:
	tstb	(r0)
	bge	2b
	bit	$1,(r0)
	bne	2b
	bit	$40000,(r0)
	beq	1b
	mov	$27,(r0)
.endif
HT	= 0172440
HTCS1	= HT+0
HTWC	= HT+2
HTBA	= HT+4
HTFC	= HT+6
HTCS2	= HT+10
HTTC	= HT+32

MTC = 172522
.if TUDUMP
	mov	$MTC,r0
	mov	$60004,(r0)+
	clr	2(r0)
1:
	mov	$-512.,(r0)
	inc	-(r0)
2:
	tstb	(r0)
	bge	2b
	tst	(r0)+
	bge	1b
	reset

/ end of file and loop

	mov	$60007,-(r0)
.endif
	br	.

.text
.globl	start, _end, _edata, _etext, _main

/ 11/45 and 11/70 startup.
/ entry is thru 0 abs.
/ since core is shuffled,
/ this code can be executed but once

start:
	bit	$1,SSR0
	beq	1f
	mov	$trap,34
	mov	$trap,0
	mov	$340+15.,2
	bit	$20,SSR3
	beq	9f
	mov	$70.,_cputype
	mov	$3,*$MSCR
9:
	clr	PS
	br	9f
1:
	inc	$-1
	bne	.
	reset
/ Set loc. 0 to trap to system, in case of
/ hardware glitch
	mov	$trap,0		/ in case of bad trap through 0
	mov	$340+15.,2	/ high pri, trap type 15
	clr	PS

/ set KI0 to physical 0

	mov	$77406,r3
	mov	$KISA0,r0
	mov	$KISD0,r1
	clr	(r0)+
	mov	r3,(r1)+

/ set KI1-6 to eventual text resting place

	mov	$_end+63.,r2
	ash	$-6,r2
	bic	$!1777,r2
1:
	mov	r2,(r0)+
	mov	r3,(r1)+
	add	$200,r2
	cmp	r0,$KISA7
	blos	1b

/ set KI7 to IO seg for escape

	mov	$IO,-(r0)

/ set KD0-7 to physical

	mov	$KDSA0,r0
	mov	$KDSD0,r1
	clr	r2
1:
	mov	r2,(r0)+
	mov	r3,(r1)+
	add	$200,r2
	cmp	r0,$KDSA7
	blos	1b

/ initialization
/ get a temp (1-word) stack
/ turn on segmentation
/ copy text to I space
/ clear bss in D space

	mov	$stk+2,sp
	mov	$65,SSR3		/ 22-bit, map, K+U sep
	bit	$20,SSR3
	beq	1f
	mov	$70.,_cputype
	mov	$3,*$MSCR		/ Disable UNIBUS traps, non-fatal traps
1:
	inc	SSR0
	mov	$_etext+100,r2
	mov	$_edata+100,r1
	add	$_etext-8192.,r1
1:
	mov	-(r1),-(sp)
	mtpi	-(r2)
	cmp	r1,$_edata
	bhi	1b
1:
	clr	(r1)+
	cmp	r1,$_end
	blo	1b

/ use KI escape to set KD7 to IO seg
/ set KD6 to first available core
/ If profiling, snag supervisor registers.

	mov	$IO,-(sp)
	mtpi	*$KDSA7
9:
	mov	$_etext-8192.+63.,r2
	ash	$-6,r2
	bic	$!1777,r2
	add	KISA1,r2
	.if	.PROFIL
	mov	r2,SISA2
	mov	r2,_proloc
	mov	$77406,SISD2
	add	$200,r2
	mov	r2,SISA2+2
	mov	$77406,SISD2+2
	add	$200,r2
	.endif
	mov	r2,KDSA6

/ Turn off write permission on kernel text
/ Take stuff above data out of address space

	mov	$KISD0,r0
1:
	mov	$77402,(r0)+
	cmp	r0,$KISD7
	blos	1b

	mov	$_end+63.,r0
	ash	$-6,r0
	bic	$!1777,r0
	mov	$KDSD0,r1
1:
	cmp	r0,$200
	bge	2f
	dec	r0
	bge	4f
	clr	(r1)
	br	3f
4:
	movb	r0,1(r1)
	br	3f
2:
	movb	$177,1(r1)
3:
	tst	(r1)+
	sub	$200,r0
	cmp	r1,$KDSD5
	blos	1b

/ set up supervisor D registers

9:
	mov	$6,SISD0
	mov	$6,SISD1

/ set up real sp
/ clear user block
/ test for floating point hardware

	mov	$_u+[usize*64.],sp
	mov	$1f,nofault
	setd			/ jump to 1f if this traps
	inc	fpp
1:
	clr	nofault
	mov	$_u,r0
1:
	clr	(r0)+
	cmp	r0,sp
	blo	1b
	.if	.PROFIL
	mov	$40000,r0
	mov	$10000,PS	/ prev = super
1:
	clr	-(sp)
	mtpi	(r0)+
	cmp	r0,$100000
	blo	1b
	jsr	pc,_isprof
	.endif

/ set up previous mode and call main
/ on return, enter user mode at 0R

	mov	$30000,PS
	jsr	pc,_main
	mov	$170000,-(sp)
	clr	-(sp)
	rtt

.globl	_rkboot, _rpboot
_rkboot:
	jmp	*$173000

_rpboot:
	jmp	*$173006


.globl	trap, call
.globl	_trap

/ all traps and interrupts are
/ vectored thru this routine.

trap:
	mov	PS,saveps
	tst	nofault
	bne	1f
	mov	SSR0,ssr
	mov	SSR1,ssr+2
	mov	SSR2,ssr+4
	mov	$1,SSR0
	jsr	r0,call1; jmp _trap
	/ no return
1:
	mov	$1,SSR0
	mov	nofault,(sp)
	rtt
.text

.globl	_runrun
call1:
	mov	saveps,-(sp)
	spl	0
	br	1f

call:
	mov	PS,-(sp)
1:
	mov	r1,-(sp)
	mfpd	sp
	mov	4(sp),-(sp)
	bic	$!37,(sp)
	bit	$30000,PS
	beq	1f
	jsr	pc,(r0)+
	tstb	_runrun
	beq	2f
	mov	$12.,(sp)		/ trap 12 is give up cpu
	jsr	pc,_trap
2:
	tst	(sp)+
	mtpd	sp
	br	2f
1:
	bis	$30000,PS
	jsr	pc,(r0)+
	cmp	(sp)+,(sp)+
2:
	mov	(sp)+,r1
	tst	(sp)+
	mov	(sp)+,r0
	rtt

.globl	_savfp
_savfp:
	tst	fpp
	beq	9f		/ No FP hardware
	mov	2(sp),r1
	stfps	(r1)+
	setd
	movf	fr0,(r1)+
	movf	fr1,(r1)+
	movf	fr2,(r1)+
	movf	fr3,(r1)+
	movf	fr4,fr0
	movf	fr0,(r1)+
	movf	fr5,fr0
	movf	fr0,(r1)+
9:
	rts	pc

.globl	_restfp
_restfp:
	tst	fpp
	beq	9f
	mov	2(sp),r1
	mov	r1,r0
	setd
	add	$8.+2.,r1
	movf	(r1)+,fr1
	movf	(r1)+,fr2
	movf	(r1)+,fr3
	movf	(r1)+,fr0
	movf	fr0,fr4
	movf	(r1)+,fr0
	movf	fr0,fr5
	movf	2(r0),fr0
	ldfps	(r0)
9:
	rts	pc

.globl	_stst
_stst:
	tst	fpp
	beq	9f
	stst	r0
	mov	r0,*2(sp)
9:
	rts	pc

.globl	_addupc
_addupc:
	mov	r2,-(sp)
	mov	6(sp),r2	/ base of prof with base,leng,off,scale
	mov	4(sp),r0	/ pc
	sub	4(r2),r0	/ offset
	clc
	ror	r0
	mov	6(r2),r1
	clc
	ror	r1
	mul	r1,r0		/ scale
	ashc	$-14.,r0
	inc	r1
	bic	$1,r1
	cmp	r1,2(r2)	/ length
	bhis	1f
	add	(r2),r1		/ base
	mov	nofault,-(sp)
	mov	$2f,nofault
	mfpd	(r1)
	add	12.(sp),(sp)
	mtpd	(r1)
	br	3f
2:
	clr	6(r2)
3:
	mov	(sp)+,nofault
1:
	mov	(sp)+,r2
	rts	pc

.globl	_display
_display:
	dec	dispdly
	bge	2f
	clr	dispdly
	mov	PS,-(sp)
	mov	$HIPRI,PS
	mov	CSW,r1
	bit	$1,r1
	beq	1f
	bis	$30000,PS
	dec	r1
1:
	jsr	pc,fuword
	mov	r0,CSW
	mov	(sp)+,PS
	cmp	r0,$-1
	bne	2f
	mov	$120.,dispdly		/ 2 sec delay after CSW fault
2:
	rts	pc

.globl	_backup
.globl	_regloc
_backup:
	mov	2(sp),r0
	movb	ssr+2,r1
	jsr	pc,1f
	movb	ssr+3,r1
	jsr	pc,1f
	movb	_regloc+7,r1
	asl	r1
	add	r0,r1
	mov	ssr+4,(r1)
	clr	r0
2:
	rts	pc
1:
	mov	r1,-(sp)
	asr	(sp)
	asr	(sp)
	asr	(sp)
	bic	$!7,r1
	movb	_regloc(r1),r1
	asl	r1
	add	r0,r1
	sub	(sp)+,(r1)
	rts	pc


.globl	_fubyte, _subyte
.globl	_fuword, _suword
.globl	_fuibyte, _suibyte
.globl	_fuiword, _suiword
_fuibyte:
	mov	2(sp),r1
	bic	$1,r1
	jsr	pc,giword
	br	2f

_fubyte:
	mov	2(sp),r1
	bic	$1,r1
	jsr	pc,gword

2:
	cmp	r1,2(sp)
	beq	1f
	swab	r0
1:
	bic	$!377,r0
	rts	pc

_suibyte:
	mov	2(sp),r1
	bic	$1,r1
	jsr	pc,giword
	mov	r0,-(sp)
	cmp	r1,4(sp)
	beq	1f
	movb	6(sp),1(sp)
	br	2f
1:
	movb	6(sp),(sp)
2:
	mov	(sp)+,r0
	jsr	pc,piword
	clr	r0
	rts	pc

_subyte:
	mov	2(sp),r1
	bic	$1,r1
	jsr	pc,gword
	mov	r0,-(sp)
	cmp	r1,4(sp)
	beq	1f
	movb	6(sp),1(sp)
	br	2f
1:
	movb	6(sp),(sp)
2:
	mov	(sp)+,r0
	jsr	pc,pword
	clr	r0
	rts	pc

_fuiword:
	mov	2(sp),r1
fuiword:
	jsr	pc,giword
	rts	pc

_fuword:
	mov	2(sp),r1
fuword:
	jsr	pc,gword
	rts	pc

giword:
	mov	PS,-(sp)
	spl	HIGH
	mov	nofault,-(sp)
	mov	$err,nofault
	mfpi	(r1)
	mov	(sp)+,r0
	br	1f

gword:
	mov	PS,-(sp)
	spl	HIGH
	mov	nofault,-(sp)
	mov	$err,nofault
	mfpd	(r1)
	mov	(sp)+,r0
	br	1f

_suiword:
	mov	2(sp),r1
	mov	4(sp),r0
suiword:
	jsr	pc,piword
	rts	pc

_suword:
	mov	2(sp),r1
	mov	4(sp),r0
suword:
	jsr	pc,pword
	rts	pc

piword:
	mov	PS,-(sp)
	spl	HIGH
	mov	nofault,-(sp)
	mov	$err,nofault
	mov	r0,-(sp)
	mtpi	(r1)
	br	1f

pword:
	mov	PS,-(sp)
	spl	HIGH
	mov	nofault,-(sp)
	mov	$err,nofault
	mov	r0,-(sp)
	mtpd	(r1)
1:
	mov	(sp)+,nofault
	mov	(sp)+,PS
	rts	pc

err:
	mov	(sp)+,nofault
	mov	(sp)+,PS
	tst	(sp)+
	mov	$-1,r0
	rts	pc

.globl	_copyin, _copyout
.globl	_copyiin, _copyiout
_copyiin:
	jsr	pc,copsu
1:
	mfpi	(r0)+
	mov	(sp)+,(r1)+
	sob	r2,1b
	br	2f

_copyin:
	jsr	pc,copsu
1:
	mfpd	(r0)+
	mov	(sp)+,(r1)+
	sob	r2,1b
	br	2f

_copyiout:
	jsr	pc,copsu
1:
	mov	(r0)+,-(sp)
	mtpi	(r1)+
	sob	r2,1b
	br	2f

_copyout:
	jsr	pc,copsu
1:
	mov	(r0)+,-(sp)
	mtpd	(r1)+
	sob	r2,1b
2:
	mov	(sp)+,nofault
	mov	(sp)+,r2
	clr	r0
	rts	pc

copsu:
	mov	(sp)+,r0
	mov	r2,-(sp)
	mov	nofault,-(sp)
	mov	r0,-(sp)
	mov	10(sp),r0
	mov	12(sp),r1
	mov	14(sp),r2
	asr	r2
	mov	$1f,nofault
	rts	pc

1:
	mov	(sp)+,nofault
	mov	(sp)+,r2
	mov	$-1,r0
	rts	pc

.globl	_idle, _waitloc
_idle:
	mov	PS,-(sp)
	spl	0
	wait
waitloc:
	mov	(sp)+,PS
	rts	pc

	.data
_waitloc:
	waitloc
	.text

.globl	_save
_save:
	mov	(sp)+,r1
	mov	(sp),r0
	mov	r2,(r0)+
	mov	r3,(r0)+
	mov	r4,(r0)+
	mov	r5,(r0)+
	mov	sp,(r0)+
	mov	r1,(r0)+
	clr	r0
	jmp	(r1)

	.globl	_resume
_resume:
	mov	2(sp),r0		/ new process
	mov	4(sp),r1		/ new stack
	spl	7
	mov	r0,KDSA6		/ In new process
	mov	(r1)+,r2
	mov	(r1)+,r3
	mov	(r1)+,r4
	mov	(r1)+,r5
	mov	(r1)+,sp
	mov	$1,r0
	spl	0
	jmp	*(r1)+

.globl	_spl0, _spl1, _spl4, _spl5, _spl6, _spl7, _splx
_spl0:
	mov	PS,r0
	spl	0
	rts	pc

_spl1:
	mov	PS,r0
	spl	1
	rts	pc

_spl4:
	mov	PS,r0
	spl	4
	rts	pc

_spl5:
	mov	PS,r0
	spl	5
	rts	pc

_spl6:
	mov	PS,r0
	spl	6
	rts	pc

_spl7:
	mov	PS,r0
	spl	HIGH
	rts	pc

_splx:
	mov	2(sp),PS
	rts	pc

.globl	_copyseg
_copyseg:
	mov	PS,-(sp)
	mov	4(sp),SISA0
	mov	6(sp),SISA1
	mov	$10000+HIPRI,PS
	mov	r2,-(sp)
	clr	r0
	mov	$8192.,r1
	mov	$32.,r2
1:
	mfpd	(r0)+
	mtpd	(r1)+
	sob	r2,1b
	mov	(sp)+,r2
	mov	(sp)+,PS
	rts	pc

.globl	_clearseg
_clearseg:
	mov	PS,-(sp)
	mov	4(sp),SISA0
	mov	$10000+HIPRI,PS
	clr	r0
	mov	$32.,r1
1:
	clr	-(sp)
	mtpd	(r0)+
	sob	r1,1b
	mov	(sp)+,PS
	rts	pc

/ Long quotient

	.globl	ldiv
ldiv:
	jsr	r5,csv
	mov	10.(r5),r3
	sxt	r4
	bpl	1f
	neg	r3
1:
	cmp	r4,8.(r5)
	bne	hardldiv
	mov	6.(r5),r2
	mov	4.(r5),r1
	bge	1f
	neg	r1
	neg	r2
	sbc	r1
	com	r4
1:
	mov	r4,-(sp)
	clr	r0
	div	r3,r0
	mov	r0,r4		/high quotient
	mov	r1,r0
	mov	r2,r1
	div	r3,r0
	bvc	1f
	sub	r3,r0		/ this is the clever part
	div	r3,r0
	tst	r1
	sxt	r1
	add	r1,r0		/ cannot overflow!
1:
	mov	r0,r1
	mov	r4,r0
	tst	(sp)+
	bpl	9f
	neg	r0
	neg	r1
	sbc	r0
9:
	jmp	cret

hardldiv:
	4

/ Long remainder

	.globl	lrem
lrem:
	jsr	r5,csv
	mov	10.(r5),r3
	sxt	r4
	bpl	1f
	neg	r3
1:
	cmp	r4,8.(r5)
	bne	hardlrem
	mov	6.(r5),r2
	mov	4.(r5),r1
	mov	r1,r4
	bge	1f
	neg	r1
	neg	r2
	sbc	r1
1:
	clr	r0
	div	r3,r0
	mov	r1,r0
	mov	r2,r1
	div	r3,r0
	bvc	1f
	sub	r3,r0
	div	r3,r0
	tst	r1
	beq	9f
	add	r3,r1
1:
	tst	r4
	bpl	9f
	neg	r1
9:
	sxt	r0
	jmp	cret

/ The divisor is known to be >= 2^15.  Only 16 cycles are
/ needed to get a remainder.
hardlrem:
	4

/.globl	lmul
/lmul:
/	mov	r2,-(sp)
/	mov	r3,-(sp)
/	mov	8(sp),r2
/	sxt	r1
/	sub	6(sp),r1
/	mov	12.(sp),r0
/	sxt	r3
/	sub	10.(sp),r3
/	mul	r0,r1
/	mul	r2,r3
/	add	r1,r3
/	mul	r2,r0
/	sub	r3,r0
/	mov	(sp)+,r3
/	mov	(sp)+,r2
/	rts	pc

.globl	csv
csv:
	mov	r5,r0
	mov	sp,r5
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	jsr	pc,(r0)

.globl	cret
cret:
	mov	r5,r2
	mov	-(r2),r4
	mov	-(r2),r3
	mov	-(r2),r2
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc

.globl	_u
_u	= 140000
usize	= 16.

CSW	= 177570
PS	= 177776
SSR0	= 177572
SSR1	= 177574
SSR2	= 177576
SSR3	= 172516
KISA0	= 172340
KISA1	= 172342
KISA7	= 172356
KISD0	= 172300
KISD7	= 172316
KDSA0	= 172360
KDSA6	= 172374
KDSA7	= 172376
KDSD0	= 172320
KDSD5	= 172332
SISA0	= 172240
SISA1	= 172242
SISA2	= 172244
SISD0	= 172200
SISD1	= 172202
SISD2	= 172204
MSCR	= 017777746	/ 11/70 memory control register
IO	= 177600

SWR	= 177570
.data
.globl	_ka6
.globl	_cputype

_ka6:	KDSA6
_cputype:45.
stk:	0

.bss
nofault:.=.+2
fpp:	.=.+2
ssr:	.=.+6
dispdly:.=.+2
saveps:	.=.+2

.text
/ system profiler
/  Expects to have a KW11-P in addition to the line-frequency
/  clock, and it should be set to BR7.
/  Uses supervisor I space register 2&3 (40000-100000)
/  to maintain the profile.

	.if	.PROFIL
CCSB	= 172542
CCSR	= 172540

.globl	_sprof, _xprobuf, _probsiz, _mode
_probsiz = 37777

_isprof:
	mov	$1f,nofault
	mov	$_sprof,104	/ interrupt
	mov	$340,106	/ pri
	mov	$100.,CCSB	/ count set = 100
	mov	$113,CCSR	/ count down, 10kHz, repeat
1:
	clr	nofault
	rts	pc

_sprof:
	mov	r0,-(sp)
	mov	PS,r0
	ash	$-10.,r0
	bic	$!14,r0
	add	$1,_mode+2(r0)
	adc	_mode(r0)
	cmp	r0,$14		/ user
	beq	done
	mov	2(sp),r0	/ pc
	asr	r0
	asr	r0
	bic	$140001,r0
	cmp	r0,$_probsiz
	blo	1f
	inc	_outside
	br	done
1:
	mov	$10340,PS		/ prev = super
	mfpi	40000(r0)
	inc	(sp)
	mtpi	40000(r0)
	bne	done
	mov	r1,-(sp)
	mov	$_xprobuf,r1
2:
	cmp	(r1)+,r0
	bne	3f
	inc	(r1)
	br	4f
3:
	tst	(r1)+
	bne	2b
	sub	$4,r1
	mov	r0,(r1)+
	mov	$1,(r1)+
4:
	mov	(sp)+,r1
done:
	mov	(sp)+,r0
	mov	$113,CCSR
	rtt

/ count subroutine calls during profiling
/  of the system.

.globl	mcount, _profcnts, _profsize

.bss
_profcnts:
	.=.+[6*340.]

.globl countbase
.data
countbase:
	_profcnts
_profsize:
	340.
.text

mcount:
	mov	(r0),r1
	bne	1f
	mov	countbase,r1
	beq	2f
	add	$6,countbase
	cmp	countbase,$_profcnts+[6*340.]
	blo	3f
	clr	countbase
	rts	pc
3:
	mov	(sp),(r1)+
	mov	r1,(r0)
1:
	inc	2(r1)
	bne	2f
	inc	(r1)
2:
	rts	pc

.bss
_xprobuf:.=.+512.
_proloc:.=.+2
_mode:	.=.+16.
_outside: .=.+2

	.endif

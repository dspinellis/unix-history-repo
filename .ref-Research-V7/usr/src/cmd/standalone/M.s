/ Startup code for two-stage bootstrap

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
halt	= 0
reset	= 5
trap	= 104400

.globl	_end
.globl	_main
start:
	reset
	mov	$340,PS
	mov	$140100,sp

/ set kernel I+D to physical 0 and IO page
	clr	r1
	mov	$77406,r2
	mov	$KISA0,r3
	mov	$KISD0,r4
	jsr	pc,setseg
	mov	$IO,-(r3)
	clr	r1
	mov	$KDSA0,r3
	mov	$KDSD0,r4
	jsr	pc,setseg
	mov	$IO,-(r3)

/ set user I+D to physical 64K (words) and IO page
	mov	$4000,r1
	mov	$UISA0,r3
	mov	$UISD0,r4
	jsr	pc,setseg
	mov	$IO,-(r3)
	mov	$4000,r1
	mov	$UDSA0,r3
	mov	$UDSD0,r4
	jsr	pc,setseg
	mov	$IO,-(r3)

/ enable map
	mov	$65,SSR3	/ 22-bit map
	bit	$20,SSR3
	beq	1f
	mov	$3,MSCR
1:
	mov	$30340,PS
	inc	SSR0


/ copy program to user I space
	mov	$_end,r0
	asr	r0
	clr	r1
1:
	mov	(r1),-(sp)
	mtpi	(r1)+
	sob	r0,1b


/ continue execution in user space copy
	mov	$140004,sp
	clr	*$KDSA6
	mov	$140340,-(sp)
	mov	$user,-(sp)
	rtt
user:
	mov	$_end+512.,sp
	mov	sp,r5

	jsr	pc,_main

	trap

	br	user

setseg:
	mov	$8,r0
1:
	mov	r1,(r3)+
	add	$200,r1
	mov	r2,(r4)+
	sob	r0,1b
	rts	pc

.globl	_setseg
_setseg:
	mov	2(sp),r1
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	mov	$77406,r2
	mov	$KISA0,r3
	mov	$KISD0,r4
	jsr	pc,setseg
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc

/ clrseg(addr,count)
.globl	_clrseg
_clrseg:
	mov	4(sp),r0
	beq	2f
	asr	r0
	bic	$!77777,r0
	mov	2(sp),r1
1:
	clr	-(sp)
	mtpi	(r1)+
	sob	r0,1b
2:
	rts	pc


/ mtpi(word,addr)
.globl	_mtpi
_mtpi:
	mov	4(sp),r0
	mov	2(sp),-(sp)
	mtpi	(r0)+
	rts	pc

.globl	__rtt
__rtt:
	halt

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
SISD0	= 172200
SISD1	= 172202
UISA0	= 177640
UISD0	= 177600
UDSA0	= 177660
UDSD0	= 177620
MSCR	= 017777746	/ 11/70 memory control register
IO	= 177600
SWR	= 177570

.data


/ acct -- time accounting

.globl	mesg, qsort, fopen, getw

	mov	(sp)+,r5
	tst	(sp)+

1:
	cmp	r5,$2
	blt	1f
	mov	(sp),r0
	cmpb	(r0),$'-
	bne	1f
	cmpb	1(r0),$'d
	bne	2f
	inc	byday
	dec	r5
	tst	(sp)+
	br	1b
2:
	cmpb	1(r0),$'p
	bne	2f
	inc	pflg
	dec	r5
	tst	(sp)+
	br	1b
2:
	cmpb	1(r0),$'w
	bne	1f
	dec	r5
	tst	(sp)+
	cmp	r5,$2
	blt	1b
	dec	r5
	mov	(sp)+,filnam
	br	1b

1:
	dec	r5
	mov	r5,argc
	mov	sp,argp
	mov	filnam,r0
	jsr	r5,fopen; fbuf
	bec	1f
	jsr	r5,mesg
		<Cannot open wtmp\n\0>; .even
	sys	exit

1:
	setd

1:
	mov	$ibuf,r1
2:
	jsr	r5,getw; fbuf
	bes	1f
	mov	r0,(r1)+
	cmp	r1,$ibuf+16.
	blo	2b
	jsr	pc,loop
	br	1b
1:
	sys	time
	mov	r0,i.time
	mov	r1,i.time+2
	clr	ibuf
	mov	$'x,i.tty
	jsr	pc,loop
	jsr	pc,print
	sys	exit

loop:
	clc
	ror	i.time
	ror	i.time+2
	setl
	movif	i.time,fr0
	tst	byday
	beq	1f
	cmpf	midnight,fr0
	cfcc
	bgt	1f
	movf	fr0,-(sp)
	movf	midnight,fr0
	jsr	pc,upall
	jsr	pc,print
	jsr	pc,clru
	movf	(sp)+,fr0
	jsr	pc,newday
1:
	movb	i.tty,r0
	mov	$ttyf,r1
	cmp	r0,$'x
	bne	1f
	jsr	pc,upall
	jsr	pc,clrt
	rts	pc
1:
	cmp	r0,(r1)
	beq	1f
	add	$18.,r1
	cmp	r1,$et
	blo	1b
	br	2f
1:
	jsr	pc,update
	clr	(r1)
2:
	cmp	ibuf,$"\0\0
	bne	1f
	rts	pc
1:
	mov	$ttyf,r1
1:
	tst	(r1)
	beq	1f
	add	$18.,r1
	br	1b
1:
	mov	r0,(r1)+
	mov	ibuf,(r1)+
	mov	ibuf+2,(r1)+
	mov	ibuf+4,(r1)+
	mov	ibuf+6,(r1)+
	movf	fr0,(r1)+
	rts	pc

clrt:
	mov	$ttyf,r1
1:
	clr	(r1)+
	cmp	r1,$et
	blo	1b
	rts	pc

clru:
	mov	$usrf,r1
1:
	clr	(r1)+
	cmp	r1,$eu
	blo	1b
	rts	pc

print:
	jsr	pc,upall
	mov	$usrf,r1
	mov	r1,r2
	mov	$16.,r3
1:
	tst	(r2)
	beq	1f
	add	r3,r2
	br	1b
1:
	jsr	pc,qsort
	clrf	fr0
	jsr	r5,select; 1f
	tstf	fr0
	cfcc
	beq	3f
	jsr	pc,pdate
	mov	$total,r2
	jsr	pc,ptime
	tst	pflg
	beq	3f
	jsr	r5,select; 2f
3:
	rts	pc

1:
	addf	8(r2),fr0
	rts	pc

2:
	movf	8(r2),fr0
	jsr	pc,pblank
	jsr	pc,ptime
	rts	pc

pdate:
	tst	byday
	bne	1f
	rts	pc
1:
	mov	r2,-(sp)
	movf	fr0,-(sp)
	movf	midnight,fr0
	divf	day,fr0
	seti
	movfi	fr0,r0
	mov	$montab,r1
1:
	sub	(r1)+,r0
	bgt	1b
	add	-(r1),r0
	cmp	r1,$montab+24.
	blt	1f
	sub	$24.,r1
1:
	sub	$montab,r1
	mov	r1,-(sp)
	asr	r1
	add	(sp)+,r1
	add	$monasc,r1
	mov	$obuf,r2
	movb	(r1)+,(r2)+
	movb	(r1)+,(r2)+
	movb	(r1)+,(r2)+
	movb	$' ,(r2)+
	mov	r0,r1
	clr	r0
	dvd	$10.,r0
	tst	r0
	beq	1f
	add	$'0,r0
	movb	r0,(r2)+
1:
	add	$'0,r1
	movb	r1,(r2)+
	movb	$'\t,(r2)+
	sub	$obuf,r2
	mov	r2,0f
	mov	$1,r0
	sys	write; obuf; 0:..
	movf	(sp)+,fr0
	mov	(sp)+,r2
	rts	pc

pblank:
	tst	byday
	beq	1f
	mov	$1,r0
	sys	write; blank; 1
1:
	rts	pc

upall:
	mov	$ttyf,r1
1:
	mov	(r1),r0
	beq	1f
	jsr	pc,update
	add	$18.,r1
	br	1b
1:
	rts	pc

update:
	mov	r1,-(sp)
	mov	$usrf,r2
1:
	mov	(sp),r1
	tst	(r1)+
	tst	(r2)
	beq	1f
	cmp	(r1)+,(r2)
	bne	2f
	cmp	(r1)+,2(r2)
	bne	2f
	cmp	(r1)+,4(r2)
	bne	2f
	cmp	(r1)+,6(r2)
	beq	1f
2:
	add	$16.,r2
	br	1b
1:
	mov	(sp),r1
	tst	(r1)+
	mov	(r1)+,(r2)+
	mov	(r1)+,(r2)+
	mov	(r1)+,(r2)+
	mov	(r1)+,(r2)+
	movf	fr0,fr1
	subf	(r1),fr1
	cfcc
	blt	1f
	cmpf	maxpd,fr1
	cfcc
	blt	1f
	addf	(r2),fr1
	movf	fr1,(r2)+
1:
	movf	fr0,(r1)+
	mov	(sp)+,r1
	rts	pc

newday:
	movf	midnight,fr1
1:
	cmpf	fr0,fr1
	cfcc
	blt	1f
	addf	day,fr1
	br	1b
1:
	movf	fr1,midnight
	rts	pc

select:
	mov	$usrf,r2
1:
	tst	(r2)
	beq	1f
	mov	argc,r1
	beq	2f
	mov	argp,r3
3:
	mov	r2,r0
	mov	(r3)+,r4
4:
	cmpb	(r0)+,(r4)+
	beq	4b
	tstb	-(r4)
	bne	4f
	cmpb	-(r0),$' /
	beq	2f
4:
	sob	r1,3b
	br	3f
2:
	jsr	pc,*(r5)
3:
	add	$16.,r2
	br	1b
1:
	tst	(r5)+
	rts	r5

ptime:
	mov	$obuf,r1
	mov	r2,r0
1:
	movb	(r0)+,(r1)
	cmpb	(r1)+,$' /
	bne	1b
	movb	$'\t,-1(r1)
	mov	r1,-(sp)
	seti
	divf	thous,fr0
	jsr	pc,digit
	jsr	pc,digit
	jsr	pc,digit
	jsr	pc,digit
	movb	$':,(r1)+
	mulf	sixth,fr0
	jsr	pc,digit
	jsr	pc,digit
	movb	$'\n,(r1)+
	sub	$obuf,r1
	mov	r1,0f
	mov	(sp)+,r1
1:
	cmpb	(r1),$'0
	bne	1f
	movb	$' ,(r1)+
	br	1b
1:
	cmpb	(r1),$':
	bne	1f
	movb	$'0,-(r1)
1:
	mov	$1,r0
	sys	write; obuf; 0:..
	rts	pc

digit:
	modf	ten,fr0
	movfi	fr1,r0
	add	$'0,r0
	movb	r0,(r1)+
	rts	pc

filnam:	wtmp
montab:
	31.; 28.; 31.; 30.; 31.; 30.; 31.; 31.; 30.; 31.; 30.; 31.
	31.; 29.; 31.; 30.; 31.; 30.; 31.; 31.; 30.; 31.; 30.; 31.
monasc:
	<Jan>
	<Feb>
	<Mar>
	<Apr>
	<May>
	<Jun>
	<Jul>
	<Aug>
	<Sep>
	<Oct>
	<Nov>
	<Dec>
day:	45436;32000;0;0
thous:	47600;137374;0;0
maxpd:	45636;032001;0;0
sixth:	40031;114631;114631;114631
ten:	41040;0;0;0
total:	<total   >
blank:	<	>
wtmp:	</usr/adm/wtmp\0>
.even
.bss
midnight:.=.+8.
byday:	.=.+2
argc:	.=.+2
argp:	.=.+2
ibuf:	.=.+16.
fbuf:	.=.+520.
i.tty	= ibuf+8.
i.time	= ibuf+10.
fi:	.=.+2
ttyf:	.=.+[20.*18.]; et:
usrf:	.=.+[200.*16.]; eu:
obuf:	.=.+20.
pflg:	.=.+2
0.; 31.; 31.; 30.; 31.; 30.; 31.
monasc:
	<Jan>
	<Feb>
	<Mar>
	<Apr>
	<May>
	<Jun>
	<Jul>
	<Aug>
	<Sep
/ init -- process control initialization



	sys	intr; 0
	sys	quit; 0
	sys	38. / get console switches
	cmp	r0,$173030
	bne	1f
help:
	clr	r0
	sys	close
	mov	$1,r0
	sys	close
	sys	open; ctty; 0
	sys	open; ctty; 1
	sys	exec; shell; shellp
	br	help
1:
	sys	mount; rk1; usr
	sys	mount; rk2; ssys
	sys	mount; rk3; crp
	mov	$'0,r1
1:
	movb	r1,tapx+8
	sys	chmod; tapx; 17
	inc	r1
	cmp	r1,$'8
	blo	1b
	sys	creat; utmp; 16
	sys	close
	sys	unlink; dpdlock
	sys	fork
		br daemon
	sys	fork
		br dirass
	sys	fork
		br dds
	movb	$'x,zero+8.
	jsr	pc,wtmprec
	mov	$itab,r1
	br	1f

daemon:
	sys	exec; etcdpd; etcdpdp
	sys	exit

dirass:
	sys	chdir; usrmel
	sys	exec; melda; meldap
	sys	exit

dds:
	sys	exec; usrdd; usrddp
	sys	exit

/ create shell processes

1:
	mov	(r1)+,r0
	beq	pwait
	movb	r0,ttyx+8
	jsr	pc,dfork
	mov	r0,(r1)+
	br	1b

/ wait for process to die

pwait:
	sys	wait
	mov	$itab,r1

/ search for process id

2:
	tst	(r1)+
	beq	pwait
	cmp	r0,(r1)+
	bne	2b

/ take name out of utmp

	sub	$4,r1
	mov	r1,-(sp)
	mov	(r1),r1
	sub	$'0,r1
	cmp	r1,$'a-'0
	blo	2f
	sub	$'a-'0-10.,r1	/ map a-z into 10. on
2:
	asl	r1
	asl	r1
	asl	r1
	asl	r1
	mov	r1,0f
	mov	$zero,r1
2:
	clr	(r1)+
	cmp	r1,$zero+16.
	blo	2b
	sys	open; utmp; 1
	bes	2f
	mov	r0,r1
	sys	seek; 0:..; 0
	mov	r1,r0
	sys	write; zero; 16.
	mov	r1,r0
	sys	close

/ re-create user process

2:
	mov	(sp)+,r1
	mov	(r1)+,r0
	movb	r0,ttyx+8
	movb	r0,zero+8.
	jsr	pc,wtmprec
	jsr	pc,dfork
	mov	r0,(r1)+
	br	pwait

dfork:
	sys	fork
		br 1f
	bes	dfork
	rts	pc
1:
	sys	quit; 0
	sys	intr; 0
	sys	chown; ttyx; 0
	sys	chmod; ttyx; 15
	sys	open; ttyx; 0
	bes	help1
	sys	open; ttyx; 1
	bes	help1
	sys	exec; getty; gettyp
	sys	exit			/ HELP!

help1:
	jmp	help

wtmprec:
	mov	r1,-(sp)
	sys	time
	mov	r0,zero+10.
	mov	r1,zero+12.
	sys	open; wtmp; 1
	bes	2f
	mov	r0,r2
	sys	seek; 0; 2
	mov	r2,r0
	sys	write; zero; 16.
	mov	r2,r0
	sys	close
2:
	mov	(sp)+,r1
	rts	pc

etcdpdp:
	etcdpd; 0
meldap:
	melda; 0
usrddp:
	usrdd; 0
usrdd:	</usr/demo/dds\0>
melda:	</usr/mel/da\0>
usrmel:	</usr/mel\0>
rk1:	</dev/rk1\0>
rk2:	</dev/rk2\0>
rk3:	</dev/rk3\0>
usr:	</usr\0>
ssys:	</sys\0>
crp:	</crp\0>
ctty:	</dev/tty\0>
shell:	</bin/sh\0>
shellm:	<-\0>
dpdlock:
	</usr/dpd/lock\0>
etcdpd:
	</etc/dpd\0>
tapx:	</dev/tapx\0>
utmp:	</tmp/utmp\0>
wtmp:	</tmp/wtmp\0>
ttyx:	</dev/ttyx\0>
getty:	</etc/getty\0>
	.even

shellp:	shellm
	0
gettyp:	getty
	0
itab:
	'0; ..
	'1; ..
	'2; ..
	'3; ..
	'4; ..
	'5; ..
	'6; ..
	'7; ..
	'8; ..
	'a; ..
	'b; ..
	 0

	.bss
offset:	.=.+2
zero:	.=.+8.; .=.+6; .=.+2.

srmel:	</usr/mel\0>
rk1:	</dev/rk1\0>
rk2:	</dev/rk2\0
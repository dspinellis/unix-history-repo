/ init -- process control initialization

mount = 21.

	sys	intr; 0 / turn off interrupts
	sys 	quit; 0
	cmp	csw,$73700 / single user?
	bne	1f / no
help:
	clr	r0 / yes
	sys	close / close current read
	mov	$1,r0 / and write
	sys	close / files
	sys	open; ctty; 0 / open control tty
	sys	open; ctty; 1 / for read and write
	sys	exec; shell; shellp / execute shell
	br	help / keep trying
1:
	mov	$'0,r1 / prepare to change
1 :
	movb	r1,tapx+8 / mode of dec tape drive x, where
	sys	chmod; tapx; 17 / x=0 to 7, to read/write by owner or
	inc	r1 / non-owner mode
	cmp	r1,$'8 / finished?
	blo	1b / no
	sys	mount; rk0; usr / yes, root file on mounted rko5
				/ disk ls /usr
	sys	creat; utmp; 16 / truncate /tmp/utmp
	sys	close / close it
	movb	$'x,zero+8. / put identifier in output buffer
	jsr	pc,wtmprec / go to write accting info
	mov	$itab,r1 / address of table to r1

/ create shell processes

1:
	mov	(r1)+,r0 / 'x, x=0, 1... to r0
	beq	1f / branch if table end
	movb	r0,ttyx+8 / put symbol in ttyx
	jsr	pc,dfork / go to make new init for this ttyx
	mov	r0,(r1)+ / save child id in word offer '0, '1,...etc.
	br	1b / set up next child

/ wait for process to die

1:
	sys	wait / wait for user to terminate process
	mov	$itab,r1 / initialize for search

/ search for process id

2:
	tst	(r1)+ / bump r1 to child id location
	beq	1b / ? something silly
	cmp	r0,(r1)+ / which process has terminated
	bne	2b / not this one

/ take name out of utmp

	sub	$4, r1 / process is found, point x' to 'x
		       / for it
	mov	r1,-(sp) / save address on stack
	mov	(r1),r1 / move 'x to r1
	sub	$'0,r1 / remove zone bits from character
	asl	r1 / generate proper
	asl	r1 / offset
	asl	r1 / for
	asl	r1 / seek
	mov	r1,0f / move it to offset loc for seek
	mov	$zero,r1
2:
	clr	(r1)+ / ccear-
	cmp	r1,$zero+16. / output buffer
	blo	2b / area
	sys	open; utmp; 1 / open file for writing
	bes	2f / if can't open, create user anyway
	mov	r0,r1 / save file desc
	sys	seek; 0:..; 0 / move to proper pointer position
	mov	r1,r0 / not required
	sys	write; zero; 16. / zero this position in
	mov	r1,r0 / restore file descriptor
	sys	close / close file

/ re-create user process

2:
	mov	(sp)+,r1 / restore 'x to r1
	mov	(r1)+,r0 / move it to r0
	movb	r0,ttyx+8 / get correct ttyx
	movb	r0,zero+8 / move identifier to output buffer
	jsr	pc,wtmprec / go to write accting into
	jsr	pc,dfork / fork
	mov	r0,(r1)+ / save id of child
	br	1b / go to wait for next process end

dfork:
	mov	r1,r2
	sub	$itab+2,r2 / left over
	asl	r2 / from previous
	asl	r2 / version of code
	mov	r2,offset
	sys	fork
		br 1f / to new copy of init
	bes	dfork / try again
	rts	pc / return
1 :
	sys	quit; 0 / new init turns off
	sys	intr; 0 / interrupts
	sys	chown; ttyx; 0 / change owner to super user
	sys	chmod; ttyx; 15 / changemode to read/write owner,
				/ write non-owner
	sys	open; ttyx; 0 / open this ttyx for reading
			      / and wait until someone calls
	bes	help1 / branch if trouble
	sys	open; ttyx; 1 / open this ttyx for writing after
			      / user call
	bes	help1 / branch if trouble
	sys	exec; getty; gettyp / getty types <login> and
				    / executes login which logs user
				    / in and executes sh-
	sys	exit / HELP!

help1:
	jmp	help / trouble

wtmprec:
	sys	time / get time
	mov	ac,zero+10. / more to output
	mov	mq,zero+12. / buffer
	sys	open; wtmp; 1 / open accounting file
	bes	2f
	mov	r0,r2 / save file descriptor
	sys	seek; 0; 2 / move pointer to end of file
	mov	r2,r0 / not required
	sys	write; zero; 16. / write accting info
	mov	r2,r0 / restore file descriptor
	sys	close / close file
2:
	rts	pc

ctty:	</dev/tty\0>
shell:	</bin/sh\0>
shellm:	<-\0>
tapx:	</dev/tapx\0>
rk0:	</dev/rk0\0>
utmp:	</tmp/utmp\0>
wtmp:	</tmp/wtmp\0>
ttyx:	</dev/ttyx\0>
getty:	</etc/getty\0>
usr:	</usr\0>
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
	0

offset:	.=.+2
zero:	.=.+8; .=.+6; .=.+2

/ u9 -- unix

trcv:
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
1:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	clockp,-(sp)
	mov	$s.syst+2,clockp
	sub	$trcv+4,r0 / 0%4 / calculate offset for tty causing
	asl	r0 / 0%8 / this interrupt
	mov	rcsr(r0),r2
	mov	rcbr(r0),r1
	tst	r2
	blt	1f / error
	tst	tty+6(r0)
	beq	1f
	bit	$40,r2 / parity
	bne	3f / branch if set
	tstb	tty+4(r0)
	blt	4f / 37 parity not allowed
	br	2f
3:
	bitb	$100,tty+4(r0)
	beq	2f / non-37 parity not allowed
4:
	bic	$!177,r1
	bit	$40,tty+4(r0)
	bne	3f / raw
	cmp	r1,$177
	beq	5f
	cmp	r1,$34
	bne	3f
5:
	mov	tty+6(r0),r0
	beq	2f
	movb	r1,6(r0) / interrupt or quit
	jsr	r0,wakeall
	br	2f
3:
	cmp	r1,$15 / or
	bne	3f
	bit	$20,tty+4(r0)
	beq	3f
	mov	$12,r1
3:
	bitb	$4,tty+4(r0)
	beq	3f
	cmp	r1,$'A
	blo	3f
	cmp	r1,$'Z
	bhi	3f
	add	$40,r1
3:
	movb	tty+3(r0),0f
	jsr	r0,putc; 0:.. / put char on input clist
		br 2f
	bitb	$10,tty+4(r0) / echo
	bne	4f / branch echo bit set
	cmp	r1,$12
	bne	3f
	bitb	$20,tty+4(r0) / cr
	beq	3f
4:
	cmp	r1,$4 / is char input an eot
	beq	1f
	mov	r1,-(sp) / put char on stack
	movb	tty+3(r0),0f
	inc	0f
	jsr	r0,putc; 0:.. / put char just input on output clist
		br .+2
	jsr	r0,starxmt
	mov	(sp)+,r1
3:
	bitb	$40,tty+4(r0) / raw
	bne	1f / branch if raw bit set
	cmp	r1,$12
	beq	1f
	movb	tty+3(r0),r1
	cmpb	cc(r1),$15.
	blo	2f
1:
	movb	tty+3(r0),0f
	jsr	r0,wakeup; runq; 0:.. / call wakeup for process
2:
	jmp	retisp  
txmt:
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
	jsr	r0,1f
1:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	clockp,-(sp)
	mov	$s.syst+2,clockp
	sub	$txmt+4,r0 / 0%4 / offset in cc
	asl	r0 / 0%8
	jsr	r0,starxmt
	jmp	retisp

xmtto:
	mov	r0,-(sp)
	mov	2(sp),r0 / 0%2+6
	sub	$6,r0
	asl	r0
	asl	r0 / 0%8
	jsr	r0,starxmt
	mov	(sp)+,r0
	rts	r0

starxmt:
	mov	(sp),r1 / 0%8 r1 contains 8xtty number
	movb	tty+3(r1),r1 / place contents of 4th byte of "tty"
			     / buf in r1 (cc,cf,cl offset)
	cmpb	cc+1(r1),$10. / is char count for tty output greater
			      / than or equal to 10
	bhi	1f / yes
	mov	r1,0f / no, make offset an arg of "wakeup"
	inc	0f / increment arg of wakeup
	jsr	r0,wakeup; runq+2; 0:.. / wakeup process identified
					/ by wlist
1: / entry specified by argument in 0:
	mov	(sp),r1 / 0%8 / r1 contains tty number
	asr	r1
	asr	r1
	asr	r1 / 0%1 r1 contains tty number
	tstb	toutt+3(r1) / is tout entry for tty output = 0
	bne	1f / no, return to calling routine
	mov	(sp),r2 / yes, place (8xtty number) into r2
	tstb	tcsr(r2) / does tty's tcsr register = 0 (is ready
			 / bit = 0)
	bge	1f / yes, return to calling routine
	movb	tty+2(r2),r1 / no, place third byte of "tty" buf
			     / into r1 (char left over after lf)
	clrb	tty+2(r2) / clear third byte
	tst	r1 / is third byte = 0
	bne	3f / no, r1 contains a non nul character
	movb	tty+3(r2),0f / yes, make byte 4 arg of "getc"
	inc	0f / increment arg to make it tty output list of
		   / clist
	jsr	r0,getc; 0:.. / obtain next character in clist for tty
			      / out and place in r1
		br 1f / if no entry in clist to be output, return to
		      / calling routine
3:
	bic	$!177,r1 / zero out bits 7-15 of r1
	movb	partab(r1),r3 / move "partab" entry (identified by
			      / r1) into r3
	bge	3f / if entry is greater than or equal to 0 (digit
		   / 2, far left digit = 0) branch
	bisb	200,r1 / if entry is less than 0 add 128 to ASCII
		       / code for char to be output
	bic	$!177,r3 / to make it teletype code and then clear
			 / bits 7-15 of r3

3:
	mov	(sp),r2 / r2 contains 8xtty number
	bit	$4,rcsr(r2) / is carrier present for tty
	beq	starxmt / no carrier flush
	mov	r1,-(sp) / yes, place character to be output on stack
	cmp	r1,$11 / is character "ht"
	bne	3f / no
	bitb	$2,tty+4(r2) / is tab to space flag for tty set
			     / (bit 1 of byte 5 in "tty" buffer area)
	beq	3f / no
	mov	$240,(sp) / yes, change character to space
3:
	mov	(sp)+,tcbr(r2) / place char to be output in tty output
			       / buffer
	add	$tty+1,r2 / place addr of 2nd byte of "tty" buf
	jmp	1f-2(r3) / area in r2 (which is the column count) and
			 / then
	incb	(r2) / normal / jmp to location determined by digits
		     / 0 and 1 of character's entry in "partab" which
		     / is now in r3
1:	rts	r0 / non-printing
	br	1f / bs  
	br	2f / nl (line feed)  
	br	3f / tab (horizontal tab)
	br	4f / vert (vertical tab)
	br	5f / cr

1:
	decb	(r2) / col decrement column count in byte 2 of "tty"
		     / area
	bge	1f / if count >=0 return to calling routine
	clrb	(r2) / col set column count = 0
	br	1f
2:
	bit	$1,r1 / is bit 0 of ASCII char = 1 (char = lf)
	bne	2f / yes
	bitb	$20,3(r2) / cr flag is bit 4 of 5th byte of "tty"
			  / area = 1
	beq	2f / no (only lf to be handled)
	movb	$15,1(r2) / place "cr" in 3rd byte of "tty" area
			  / (character leftover after "lf" )
2:
	movb	(r2),r3 / place present column count in r3
	beq	1f / return to calling routine if count = 0
	clrb	(r2) / col clear column count
	asr	r3
	asr	r3
	asr	r3
	asr	r3 / delay = col/16
	add	$3,r3 / start to determine tout entry for tty output
	br	2f
3:
	bitb	$2,3(r2) / is bit 1 of 5th byte of "tty" area = 1
			 / (tab to space bit set)
	beq	3f / no
	incb	(r2) / increment column count
	bitb	$7,(r2) / are bits 0, 1 and 2 set at col 0%8
	beq	1f / no
	movb	$11,1(r2) / yes, place ht in another tab next time
	br	1f / 3rd byte of tty area (character left over after
		   / "lf")
3:
	movb	(r2),r3 / place column count in r3
	bisb	$7,(r2) / make bits 0, 1 and 2 of column count = 1
	incb	(r2) / increment column count
	bis	$!7,r3 / clear bits 3-15 of r3
	neg	r3 / delay = dcol start to determine tout entry for
		   / tty out
	br	2f / by neg r3
4:
	mov	$176.,r3 / delay = lots start to determine tout entry
	br	2f
5:
	mov	$10.,r3 / cr delay 160ms for tn300 start to determine
		        / tout
	clrb	(r2) / set column count = 0 entry
2:
	add	$5,r3 / time for this char,increment value for tout
		     / entry by 5
	mov	(sp),r2 / 0%8 r2 contains 8xtty number
	asr	r2
	asr	r2
	asr	r2 / 0%1 r2 contains tty number
	movb	r3,toutt+3(r2) / place value for tout entry into tout
			       / table
1:
	rts	r0 / return

partab:	/ contains 3 digits for each character; digit 2 is used
	/ to determine if 200 is to added to ASCII code digits 0
	/ and 1 are used to determine value for jump table.
	.byte 002,202,202,002,202,002,002,202
	.byte 204,010,006,212,012,214,202,002
	.byte 202,002,002,202,002,202,202,002
	.byte 002,202,202,002,202,002,002,202
	.byte 200,000,000,200,000,200,200,000
	.byte 000,200,200,000,200,000,000,200
	.byte 000,200,200,000,200,000,000,200
	.byte 200,000,000,200,000,200,200,000
	.byte 200,000,000,200,000,200,200,000
	.byte 000,200,200,000,200,000,000,200
	.byte 000,200,200,000,200,000,000,200
	.byte 200,000,000,200,000,200,200,000
	.byte 000,200,200,000,200,000,000,200
	.byte 200,000,000,200,000,200,200,000
	.byte 200,000,000,200,000,200,200,000
	.byte 000,200,200,000,200,000,000,202

xmtt:
	jsr	r0,cpass / get next character from user buffer area
	tst	r1 / is character nul
	beq	xmtt / yes, get next character
1:
	mov	$240,*$ps / set processor priority equal to 5
	mov	(sp),r2 / r2 contains i node number of file
	asl	r2 / 0%2+28 / multlply inode number by 2
	sub	$21.,r2 / 0%2+7 / subtract 21 from 2x inumber to
			/ get cc, cf, cl offset
	mov	r2,0f / make offset arg of putc
	cmpb	cc(r2),$50. / is char count for device greater than
			    / or equal to 50
	bhis	2f / yes
	jsr	r0,putc; 0:.. / find location in freelist to assign to
			      / device and
		br 2f / place char in list, if none available branch
		      / to put process to sleep
	mov	r0,-(sp) / place calling routines return address on
			 / stack
	mov	0b,r0 / place offset into cc, cl and cf tables in r0
	sub	$7,r0 / subtract seven from offset
	asl	r0 / multiply by 2
	asl	r0 / 0%8 / multiply by 2 (r0 contains 8xtty number)
	jsr	r0,starxmt / attempt to output character
	mov	(sp)+,r0 / pop stack
	br	xmtt / get next character
2:
	mov	r1,-(sp) / place character on stack
	mov	0b,0f / make offset into cc, cf, cl table arg of
		      / sleep (identifies location in wlist)
	jsr	r0,sleep; 0:.. / put process to sleep
	mov	(sp)+,r1 / remove character from stack
	br	1b / try again

rcvt: / read tty
	sub	$28.,r1 / 0%2 r1 contains 2xtty number
	asl	r1
	asl	r1 / r1 contains 8xtty number
	mov	r1,-(sp)
	mov	tty+6(r1),r5 / r5 contains address of 4th word in
			     / tty area
	tst	2(r5) / is char count = 0
	bne	1f / no
	bitb	$40,tty+4(r1) / raw flag set?
	beq	2f / no
	tst	-(sp) / yes, decrement sp
	jsr	r0,rcvch / get character from clist
	tst	(sp)+ / increment sp
	mov	(sp)+,r2 / r2 contains 8xtty number
	bitb	$4,rcsr(r2) / is carrier detect bit on
	beq	3f / no
	jsr	r0,passc / yes, place character in users buffer area
3:
	jmp	ret
2:
	jsr	r0,canon; rcvch / process a line of characters in
				/ clist and place results in tty buffer
				/ area
1:
	tst	(sp)+ / increment sp
1:
	tst	2(r5) / is char count for tty buffer = 0
	beq	1f / yes
	movb	*4(r5),r1 / no, move character pointer to r1
	inc	4(r5) / increment character pointer
	dec	2(r5) / decrement character count
	jsr	r0,passc / place character, whose address is in
			 / r1, in
	br	1b / user buffer area. Then get next character.
1:
	jmp	ret

rcvch:
	mov	4(sp),r2 / 0%8 r2 contains 8xtty number
	mov	$4,r1
	bit	r1,rcsr(r2) / is carrier detection bit on
	bne	1f / yes
	bic	$1,rcsr(r2) / no, clear data terminal ready bit
	rts	r0
1:
	movb	tty+3(r2),0f / make cc offset arg for "getc"
	mov	$240,*$ps / set processor priority = 5
	jsr	r0,getc; 0:.. / get next character off clist
		br 2f / clist empty
	clr	*$ps / set processor priority = 0
	rts	r0
2:
	mov	0b,0f / make "getc" arg an arg for "sleep"
	mov	r5,-(sp) / save tty buffer address on stack
	jsr	r0,sleep; 0:..
	mov	(sp)+,r5
	br	rcvch

ocvt:
	sub	$28.,r1 / 0%2 calculate tty table offset
	mov	r1 ,r2
	asl	r1 / 0%4
	asl	r1 / 0%8
	mov	r1,-(sp)
	add	$6,r2 / calculate clist id  clist offset
	movb	r2,tty+3(r1) / put clist id in tty table
1:
	mov	(sp),r1
	bit	$4,rcsr(r1) / carrier detect bit set
	bne	1f / if so, branch
	mov	$511,rcsr(r1) / set ready, speed, interrupt enable,
			      / supervisor transmit
	movb	tty+3(r1),0f / put clist id in sleep argument
	jsr	r0,sleep; 0:..
	br	1b
1:
	mov	tty+6(r1),r5 / put tty buffer address in r5
	tstb	(r5) / first byte of tty buffer = 0
	bne	1f / if not, branch
	mov	$511,rcsr(r1) / set control bits for receiver
	mov	$511,tcsr(r1) / set control bits for transmitter
	movb	$210,tty+4(r1) / put 210 in tty table word 3 / set flags
1:
	incb	(r5) / inc first byte of tty buffer
	tst	(sp)+
	tst	u.ttyp / is there a process control tty
	bne	1f / yes, then branch
	mov	r5,u.ttyp / no, make this tty the process control tty
	br	1f / return

ccvt:
	sub	$28.,r1
	asl	r1 / 0%4
	asl	r1
	mov	tty+6(r1),r5
	decb	(r5)
1:
	jmp	sret


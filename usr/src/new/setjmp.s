    # C library -- setjmp, longjmp

    #	longjmp(a,v)
    # will generate a "return(v)" from
    # the last call to
    #	setjmp(a)
    # by restoring registers from the stack
    # and doing a return.
    #

    .globl	_setjmp
    .globl	_longjmp

	    .align	1
    _setjmp:
	    .word	0x0000
	    movl	4(ap),r0
	    movl	12(fp),(r0)		# save frame pointer of caller
	    movl	16(fp),4(r0)		# save pc of caller
	    clrl	r0
	    ret

	    .align	1
    _longjmp:
	    .word	0x0000
	    movl	8(ap),r0		# return(v)
	    beql	L1
	    movzbl	$1,r0
    L1:
	    movl	4(ap),r1		# fetch buffer
	    tstl	(r1)
	    beql	botch
    loop:
	    bitw	$1,6(fp)		# r0 saved?
	    beql	L10
	    movl	r0,20(fp)
	    bitw	$2,6(fp)		#was r1 saved?
	    beql	L11
	    movl	r1,24(fp)
	    brb	L11
    L10:
	    bitw	$2,6(fp)		#was r1 saved?
	    beql	L11
	    movl	r1,20(fp)
    L11:
	    cmpl	(r1),12(fp)
	    beql	done
	    blssu	botch
	    movl	$loop,16(fp)
	    ret				# pop another frame

    done:
	    cmpb	*16(fp),reiins		# returning to an "rei"?
	    bneq	L20
	    movab	L21,16(fp)
	    ret
    L21:	addl2	$8,sp			# compensate for PSL-PC push
	    jmp	*4(r1)
    L20:
	    movl	4(r1),16(fp)
	    ret

    botch:
	    pushl	$14
	    pushl	$msg
	    pushl	$2
	    calls	$3,_write
	    halt

    msg:	.byte	'l, 'o, 'n, 'g, 'j, 'm, 'p, ' , 'b, 'o, 't, 'c, 'h, 012
    reiins:	rei

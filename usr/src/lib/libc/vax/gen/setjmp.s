/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
	.asciz	"@(#)setjmp.s	5.2 (Berkeley) %G%"
#endif not lint

/*
 * C library -- setjmp, longjmp
 *
 *	longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	setjmp(a)
 * by restoring registers from the stack,
 * and a struct sigcontext, see <signal.h>
 */

#include "DEFS.h"

ENTRY(setjmp, R6)
	movl	4(ap),r6		# construct sigcontext
	subl2	$8,sp			# space for current struct sigstack
	pushl	sp			# get current values
	pushl	$0			# no new values
	calls	$3,_sigstack		# pop args plus signal stack value
	movl	*(sp)+,(r6)+		# save onsigstack status of caller
	pushl	$0
	calls	$1,_sigblock		# get signal mask
	movl	r0,(r6)+		# save signal mask of caller
	movl	(ap),r0
	moval	4(ap)[r0],(r6)+		# save sp of caller
	movl	12(fp),(r6)+		# save frame pointer of caller
	movl	8(fp),(r6)+		# save argument pointer of caller
	movl	16(fp),(r6)+		# save pc of caller
	movpsl	(r6)			# save psl of caller
	movw	4(fp),(r6)
	clrl	r0
	ret

ENTRY(longjmp, 0)
	movl	8(ap),r0		# return(v)
	movl	4(ap),r1		# fetch buffer
	tstl	12(r1)
	beql	botch
loop:
	cmpl	12(r1),fp		# are we there yet?
	beql	done
	blssu	botch
	moval	20(fp),r2
	blbc	6(fp),1f		# was r0 saved?
	movl	r0,(r2)+
1:
	bbc	$1,6(fp),2f		# was r1 saved?
	movl	r1,(r2)
2:
	movl	$loop,16(fp)
	ret				# pop another frame

done:

#ifndef NOCOMPAT
/*
 * This code checks to see if it can use the new sigreturn.
 * If it finds that sigtramp is using the new system call,
 * it will also use it. Otherwise it uses the old system call
 * to preserve compatibility.
 */
#include <vax/machparam.h>
#define U (0x80000000-UPAGES*NBPG)
#define PCB_SIGC 0x6c
#define CHMKINS 7
	cmpl	3f,U+PCB_SIGC+CHMKINS	# check to see how sigtramp returns
	beql	4f			# sigtramp uses the new system call
	pushl	r1			# must use the old signal return
	chmk	$139			# restore previous context
	jmp	*20(r1)			# done, return
3:
	chmk	$103			# the new system call for sigreturn
4:
#endif NOCOMPAT

	pushl	r1			# pointer to sigcontext
	calls	$1,_sigreturn		# restore previous context
					# we should never return
botch:
	pushl	$msgend-msg
	pushl	$msg
	pushl	$2
	calls	$3,_write
	halt

	.data
msg:	.ascii	"longjmp botch\n"
msgend:

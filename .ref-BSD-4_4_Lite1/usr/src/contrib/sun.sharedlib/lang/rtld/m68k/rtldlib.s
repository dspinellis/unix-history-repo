/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/* @(#)rtldlib.s 1.8 69/12/31 SMI */

/*
 * Copyright (c) 1987, 1991 by Sun Microsystems, Inc.
 */

/*
 * MC680x0-dependent code for run-time link editor.
 */

#include "assym.h"
#include <sys/syscall.h>

	.text
	.globl	_binder, _rtbinder

/*
 * Stack frame and calling sequence: version #, ptr to interface structure.
 */
#define FRAME	0xc			/* Size of stack frame */
#define version	a6@(8)			/* Version # of crt0/ld.so intfce */
#define ip	a6@(FRAME)		/* Pointer to interface struct */

/*
 * Obsolete interface: kept for developmental compatibility
 */
#define BA	(-FRAME+0x0)		/* Base address offset */
#define ba	a6@(BA)
#define EP	(-FRAME+0x4)		/* Environment Pointer offset */
#define ep	a6@(EP)
#define UDP	(-FRAME+0x8)		/* Offset to main's __DYNAMIC */
#define udp	a6@(UDP)

#define arg_udp a6@(0x8)
#define arg_ep	a6@(0xc)

_rtl:					| crt0 calls us here
	link	a6,#-FRAME		| Allocate stack frame
	moveml	d2-d3/a5,sp@-		| Be transparent
	movl	#__GLOBAL_OFFSET_TABLE_,a5	| Get pointer to our linkage
	lea	pc@(0, a5:L),a5		|  tables

| If version is greater than a page size, then this is the obsolete
| interface, where "version" is really a pointer to the dynamic 
| structure.  N.B. If we ever get more than 2047 revisions to this,
| we need to do "something".  (Like get rid of the idiot who took
| 2048 tries to get it right.)  Note that all table addresses are
| still "relative", thus we must take pains to make them absolute.

	movl	version,d0		| Interrogate version
	andl	#PAGEMASK,d0		| See if bigger than page size
	jne	oldversion		|  It is, old interface
	movl	ip,a1			| Extract data from interface structure
	movl	a1@,d2			| Base address of ld.so
	movl	d2,d3			| Build arguments to rtld
	addl	a5@,d3			| Determine address of rtld's
	movl	d3,sp@-			|   GLOBAL_OFFSET_TABLE
	movl	ip,sp@-			| Pass rest of arguments
	movl	version,sp@-		| 
	movl	a5@(_rtld:w),a0		| Relocate address of function
	addl	d2,a0			| 
	jsr	a0@			| _rtld(version, ip, GLOBAL_OFFSET_...)
	addw	#0xc,sp			| Pop arguments
	jra	done			|   and leave

| Here if we're processing the old interface.

oldversion:
	lea	pc@(_rtl-(oldversion+2)),a0
	addl	#-TXTOFF,a0		| Derive base address of link editor
	movl	a5@,a1			| Calculate address of it's
	addl	a0,a1			|   GLOBAL_OFFSET_TABLE
	movl	a1,sp@-			| Pass it along
	movl	arg_udp,udp		| Dummy up an
	movl	arg_ep,ep		|   "interface structure"
	movl	a0,ba	
	pea	ba
	movl	arg_udp,sp@-		| Pass "version #"
	movl	a5@(_rtld:w),a1		| Relocate address of function
	addl	a1,a0	
	jsr	a0@			| _rtld(udp, &[ba, ep, udp], 
	addw	#0xc,sp			|   GLOBAL_OFFSET_...)


| Common exit

done:	moveml	sp@+,d2-d3/a5		| Be transparent
	unlk	a6			| Delete frame,
	rts				|   and return

| First call to a procedure generally comes through here for
| binding.

_rtbinder:				| jsr <unbound symbol>
	link	a6,#0			| Make a frame
	moveml	d0-d1/a0-a1,sp@-	| Be transparent to caller
	movl	a6@(4),a0		| Get return PC
	movw	a0@,d0			| Get pointer to relocation record
	extl	d0			| Stretch out to full word
	movl	d0,sp@-			| Stack
	movl	a0,sp@-			|   along with return PC
	jbsr	_binder,a0		| _binder(rpc, index)
	addql	#8,sp			| Pop arguments
	movl	d0,a6@(4)		| return address = _binder()
#ifdef mc68020
	trap	#2			| flush 68020 instruction cache
#endif
	moveml	sp@+,d0-d1/a0-a1	| Become transparent
	unlk	a6			| Delete frame
	rts				|   and get on with the call

| Special system call stubs which return real and effective user and group
| id's.  Saves overhead of making separate calls for each.

	.globl	_getreuid, _getregid
_getreuid:
	pea	SYS_getuid		| getuid()
	trap	#0			| Do the call, pop stack
	bcss	out			| Handle error (!)
	movl	sp@(4),a0		| Pick up addresses of result
	movl	sp@(8),a1		|   storage
	movl	d0,a0@			| Deposit the answers
	movl	d1,a1@			|   for both values
	rts				| Go home
out:	jbsr	cerror,a0		| Call common error routine
	rts

_getregid:
	pea	SYS_getgid		| getgid()
	trap	#0
	bcss	out
	movl	sp@(4),a0
	movl	sp@(8),a1
	movl	d0,a0@
	movl	d1,a1@
	rts

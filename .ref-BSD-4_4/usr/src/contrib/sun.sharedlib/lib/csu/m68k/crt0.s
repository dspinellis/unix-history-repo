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

/* @(#)crt0.s 1.25 88/03/21 SMI */

/*
 * "C" run-time program bootstrap.
 */

/*
 * Copyright (c) 1987, 1991 Sun Microsystems, Inc.
 */

#include <sys/syscall.h>
#include <sys/mman.h>

/*
 * Start a program by calling its "main" with argument list and environment
 * strings.  In addition, if program requires dynamic link editing, obtain
 * and hand control off to the link editor to do so.
 */

	.text
	.globl	start
start:					| Execution begins here
	lea	0,a6			| Initial stack frame link is 0
	movl	sp@,d2			| argc
	lea	sp@(4),a3		| argv
	movl	d2,d1			| Find end of argv[] as
	asll	#2,d1			|   sizeof(argv[0]) * argc
	lea	a3@(4,d1:l),a4		| Yields environment
	movl	a4,_environ		| Save for program's use
	tstl	__dp:l			| Does this program require editing?
	beq	1f			|   No
	bsr	__getrtld		| getrtld()
1:
	pea	a4@			| main(argc, argv, envp)
	pea	a3@
	movl	d2,sp@-
	jsr	start_float		| Initialize floating point
__main:
	jsr	_main			| Do the program
	addw	#12,sp			| Clean up
#ifdef OLD
	movl	#0,sp-			| Ignore main's return value
#else
	movl	d0,sp@-			| Use main's return value
#endif OLD
	jsr	_exit			| exit(status)
	addql	#4,sp			| In the event exit doesn't
	movl	d0,sp@-			|   then _exit(exit(status))
	jsr	__exit
	/*NOTREACHED*/

/*
 * __getrtld: load and call the link editor.  When a program is link edited
 * by "ld", a structure is created if the program is "incomplete".  Such
 * programs must "complete themselves" by getting the link editor involved
 * again.
 */

/* XXX Derive these constants */
#define	_RTLD_INTERFACE_	1	/* Interface version */
#define	FRAME	0x38			/* sizeof (struct exec) + locals */
#define	A_MACH	(-FRAME+0x0)		/* Offsets into struct exec */
#define	A_MAGIC	(-FRAME+0x2)
#define	A_TEXT	(-FRAME+0x4)
#define	A_DATA	(-FRAME+0x8)
#define	A_BSS	(-FRAME+0xc)
#define	A_ENTRY	(-FRAME+0x14)
#define	exec	a6@(-FRAME)		/* Symbols for stack offsets */
#define	a_mach	a6@(A_MACH)
#define	a_magic	a6@(A_MAGIC)
#define	a_text	a6@(A_TEXT)
#define	a_data	a6@(A_DATA)
#define	a_bss	a6@(A_BSS)
#define	a_entry	a6@(A_ENTRY)
#define	ldba	a6@(-0x18)		/* ld.so base address */
#define	fd_dz	a6@(-0x14)		/* fd on /dev/zero */
#define	fd_ld	a6@(-0x10)		/* fd on link editor */
#define	udp	a6@(-0xc)		/* &__DYNAMIC */
#define	ep	a6@(-0x8)		/* environ */
#define	break	a6@(-0x4)		/* &main */
#define	SEGSIZ	0x20000
#define	PAGSIZ	0x2000
#define	ZMAGIC	0413

__getrtld:
	link	a6,#-FRAME		| Allocate frame
	moveml	d2-d3,sp@-		| Save some registers

| Find and map in the link editor

	pea	__e_ld			| Find the link editor
	pea	0			| No flags
	pea	__link_editor		| File name
	jsr	__open			| fd_ld = open(link_editor, 0)
	addl	#0x8,sp
	movl	d0,fd_ld

| Read in its a.out header

	pea	0x20			| Size of buffer
	pea	exec			| Address of buffer
	movl	d0,sp@-			| fd_ld
	jsr	__read			| count = read(fd_ld, &exec, sizeof)
	addl	#0x10,sp		| Clean
	cmpl	#0x20,d0		| Get full count?
	jeq	1f			|   Yes
2:	lea	__e_ld,a0		| Otherwise
	jra	__errxit		|   dispatch error

| Determine if we've got a reasonable executable

1:	cmpw	#ZMAGIC,a_magic		| Demand-paged executable?
	jne	2b			|   Nope, this is fatal

| Determine how much of the address space the link editor will occupy

	movl	#(PAGSIZ-1),d2		| Starts one page up
	addl	a_text,d2		| Now figure out how long the text is
	andl	#~(SEGSIZ-1),d2		| Figure out last text segment and
	addl	#SEGSIZ,d2		|   determine data segment start
	movl	d2,d0			| d0 == amount for text + gap
	addl	a_data,d0		| d0 == text + gap + data
	addl	a_bss,d0		| d0 == text + gap + data + bss

| Map in the link editor

	pea	__e_mmap		| Set up for mapping
	pea	0			| No offset
	movl	fd_ld,sp@-		| fd on link editor
	movl	__mmap_assign,sp@- 	| Specify mapping options
	movl	__mmap_ro,sp@-		| Access protection
	movl	d0,sp@-			| How much we're mapping
	pea	0			| We don't care where it goes
	jsr	__mmap			| mmap(all of link editor)
	addl	#0x18,sp		| Clean up (but not error string)
	movl	d0,ldba			| Save the address

| Now, map in the data segment in its proper position

	movl	a_text,sp@-		| Offset in file to data segment
	movl	fd_ld,sp@-		| fd on link editor
	movl	__mmap_fixed,sp@-	| Mapping options
	movl	__mmap_prot,sp@-	| Protection
	movl	a_data,sp@-		| Map in the whole data segment
	movl	d2,d0			| Get relative start of data segment
	addl	ldba,d0			| Calculate absolute address
	movl	d0,sp@-
	jsr	__mmap			| mmap(abs, data segment)
	addl	#0x18,sp		| Clean up all but error string
	movl	d0,d3			| Save address where data segment went

| Unmap the gap between the text and data segments

	subl	a_text,d2		| Calculate size of gap
	movl	d2,sp@-			| Use that as length of unmapping
	movl	ldba,d0			| Set starting address as base +
	addl	a_text,d0		|   size of the text segment
	movl	d0,sp@-
	jsr	__munmap		| Unmap the gap
	addl	#0xc,sp			| Clear arguments + error string

| Obtain /dev/zero to build the bss from

	pea	__e_zero		| Set up for failure
	pea	0			| No flags
	pea	__dev_zero		| File name
	jsr	__open			| fd_dz = open(/dev/zero, 0)
	addql	#0x8,sp			| Pop non-error arguments
	movl	d0,fd_dz		|   and save file descriptor

| Map in enough to build bss from

	tstl	a_bss			| Any bss?
	jne	1f			|   No, skip all this
	addql	#4,sp			| Pop off error string
	jra	2f			|   and join common code
1:	pea	0			| Otherwise, map from offset 0
	movl	d0,sp@-			| Using /dev/zero fd
	movl	__mmap_fixed,sp@-	| PRIVATE, FIXED
	movl	__mmap_prot,sp@-	| All access
	movl	a_bss,sp@-		| Use a bss's worth
	addl	a_data,d3		| Starting at the end of the
	movl	d3,sp@-			|   data segment
	jsr	__mmap			| mmap(/dev/zero, a_bss)
	addl	#0x1c,sp		| Clear all arguments

| Fill in rest of argument list and call link editor

2:	lea	__main,a0		| Pass
	movl	a0,break		|   breakpoint address
	movl	_environ,ep		| Pass environment
	movl	__dp,udp		|   and this program's __DYNAMIC
	pea	ldba			| Prepare to call link editor
	pea	_RTLD_INTERFACE_	| Tell interface number
	movl	ldba,a0			| Get link editor base address
	addl	a_entry,a0		| Offset by entry point
	jsr	a0@			| Call it
	addql	#8,sp			| Clean
	moveml	sp@+,d2-d3		| Restore registers
	unlk	a6			|   and clean frame
	rts				|     and return

| System call interfaces

#define	STDERR	2			/* File descriptor for errors */
#define	STATUS	0x7f			/* Exit status */
#define	eo_open	sp@(0xc)		/* open() error pointer offset */
#define	eo_mmap sp@(0x1c)		/* mmap() */
#define	eo_munmap sp@(0xc)		/* munmap() */
#define	eo_read sp@(0x10)		/* read */

__open:	movl	eo_open,a0		| Get pointer to error structure
	pea	SYS_open		| Push trap code
	jra	__syscom		| Execute common code

__mmap:	movl	eo_mmap,a0		| See above
	pea	SYS_mmap
	jra	__syscom

__munmap:
	movl	eo_munmap,a0
	pea	SYS_munmap
	jra	__syscom

__read:	movl	eo_read,a0
	pea	SYS_read

__syscom:
	trap	#0			| Do the system call
	bcss	__errxit		| Error?
	rts				|   No, return success

__errxit:
	movl	a0@+,sp@-		| Push character count
	pea	a0@			|   and buffer address
	pea	STDERR
	pea	0			| Dummy
	pea	SYS_write
	trap	#0			| write(stderr, &msg, count)
	movl	#STATUS,sp@(4)
	pea	SYS_exit
	trap	#0			| _exit(status)
	/* NOTREACHED */

| Initialized, read-only data

	.even
__dp:	.long	__DYNAMIC		| ld makes this 0 or a pointer
__link_editor:				| Name of file containing link editor
	.asciz	"/usr/lib/ld.so"
__dev_zero:				| Name of source of zero'ed memory
	.asciz	"/dev/zero"
	.even
__e_ld:	.long	0x18			| Missing/trashed link editor
	.ascii	"crt0: no /usr/lib/ld.so\012"
	.even
__e_mmap:				| mmap/munmap don't work
	.long	0x25
	.ascii	"crt0: /usr/lib/ld.so mapping failure\012"
	.even
__e_zero:				| /dev/zero missing, unmappable
	.long	0x13
	.ascii	"crt0: no /dev/zero\012"
	.even
__mmap_assign:				| mmap flags for system assignment
	.long	_MAP_NEW+MAP_PRIVATE
__mmap_fixed:				| mmap flags for program assignment
	.long	_MAP_NEW+MAP_FIXED+MAP_PRIVATE
__mmap_prot:				| protection flags
	.long	PROT_READ+PROT_WRITE+PROT_EXEC
__mmap_ro:				| read only
	.long	PROT_READ+PROT_EXEC

| Writeable data

	.data
	.globl	_environ
_environ:
	.long	0

! This source code is a product of Sun Microsystems, Inc. and is provided
! for unrestricted use provided that this legend is included on all tape
! media and as a part of the software program in whole or part.  Users
! may copy or modify this source code without charge, but are not authorized
! to license or distribute it to anyone else except as part of a product or
! program developed by the user.
!
! THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
! SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
! OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
! EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
! ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
! NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
! INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
! FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
! 
! This source code is provided with no support and without any obligation on
! the part of Sun Microsystems, Inc. to assist in its use, correction, 
! modification or enhancement.
!
! SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
! INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
! SOURCE CODE OR ANY PART THEREOF.
!
! Sun Microsystems, Inc.
! 2550 Garcia Avenue
! Mountain View, California 94043

!
!	.asciz "@(#)crt0.s 1.14 89/06/13 SMI"
!
! Copyright (c) 1987, 1991 by Sun Microsystems, Inc.
!
!	crt0.s for the Sparc Architecture
!

#include <machine/asm_linkage.h>
#include <sys/syscall.h>
#include <sys/mman.h>

#ifdef	FIX_ALIGN
#include <machine/trap.h>
#endif	FIX_ALIGN

	.seg	"data"
	.global	_environ
_environ:
	.word	0
	.seg	"text"
	.align	4
	.global	start

!
! Start up a C program.
! On entry the stack frame looks like:
!
!	 _______________________  <- USRSTACK
!	|	    :		|
!	|  arg and env strings	|
!	|	    :		|
!	|-----------------------|
!	|	    0		|
!	|-----------------------|
!	|	    :		|
!	|  ptrs to env strings	|
!	|-----------------------|
!	|	    0		|
!	|-----------------------|
!	|	    :		|
!	|  ptrs to arg strings  |
!	|   (argc = # of wds)	|
!	|-----------------------|
!	|	   argc		|
!	|-----------------------|
!	|    window save area	|
!	|	 (16 wds)	|
!	|_______________________| <- %sp
!
start:
	mov	0, %fp			! stack frame link 0 in main -- for dbx
#ifdef	FIX_ALIGN
	ta	ST_FIX_ALIGN		! enable kernel alignment trap handler
#endif	FIX_ALIGN
	ld	[%sp + WINDOWSIZE], %o0	! argc
	add	%sp, WINDOWSIZE + 4, %o1! argv
	sll	%o0, 2, %o2		! argc * sizeof (int)
	add	%o2, 4, %o2		! skip 0 at end of arg ptrs
	add	%o1, %o2, %o2		! environment ptr
	sethi	%hi(_environ), %o3
	st	%o2, [%o3 + %lo(_environ)] ! store in _environ
	!
	! If the program requires dynamic link editing, call getrtld
	! to setup the run time loader.
	!
	sethi	%hi(__dp), %g1
	ld	[%g1 + %lo(__dp)], %g1 ! dynamically loaded?
	tst	%g1
	bz	1f			! no
	nop

	call	__getrtld		! yes, call getrtld
	nop
1:
	call	start_float		! initialize floating point
	nop

__main:
	call	_main			! main(argc, argv, envp)
	sub	%sp, SA(ARGPUSHSIZE+4), %sp ! leave room to push args

	call	_exit			! exit(main(...))
	nop

	call	__exit			! _exit(exit(0))
	nop
	/*NOTREACHED*/

/*
 * __getrtld: load and call the link editor.  When a program is link edited
 * by "ld", a structure is created if the program is "incomplete".  Such
 * programs must "complete themselves" by getting the link editor involved
 * again.
 */

/* struct exec */
/* XXX need to generate these symbols from a.out.h */
#define EXECSIZE	0x20
#define A_MAGIC		0x2
#define A_TEXT		0x4
#define A_DATA		0x8
#define A_BSS		0xc
#define A_ENTRY		0x14
#define ZMAGIC		0413

/* run time loader (ld.so) interface */
#define RTLD_VERS	1
#define VERS1NARGS	6
#define RTLDSIZE	(VERS1NARGS*4)

#define BASEADDR	0x0
#define DZFD		0x4
#define RLFD		0x8
#define UDP		0xc
#define ENV		0x10
#define BRKADDR		0x14

/* stack offsets */
#define FRAME		(MINFRAME+EXECSIZE+RTLDSIZE)
#define EXECOFF		(MINFRAME)		/* struct exec */
#define EXECP		%sp + EXECOFF
#define RTLDOFF		(MINFRAME+EXECSIZE)	/* run time loader interface */
#define RTLDP		%sp + RTLDOFF

/* mmap constants */
#define MMAP_RO		(PROT_READ|PROT_EXEC)
#define MMAP_PROT	(PROT_READ|PROT_WRITE|PROT_EXEC)
#define MMAP_FIXED	(_MAP_NEW|MAP_FIXED|MAP_PRIVATE)
#define MMAP_ASSIGN	(_MAP_NEW|MAP_PRIVATE)

/* registers */
#define errmp		l7
#define rlfd		l6
#define dzfd		l5
#define txt		l4
#define baseaddr	l3
#define td		l2
#define bss		l1

__getrtld:
	!
	! Allocate enough stack for MINFRAME, size of struct exec
	! and args passed to the runtime linker.
	!
	save	%sp, -SA(FRAME), %sp
	!
	! Find the link editor
	!
	set	Lerrmsg1, %errmp	! setup error message
	set	__link_editor, %o0
	call	__open, 2		! rlfd = open(".../ld.so",0)
	mov	0, %o1

	mov	%o0, %rlfd		! result
	!
	! Read in its exec header.
	!
	add	%sp, EXECOFF, %o1	! exec on stack
	call	__read, 3		! read(rlfd,&exec,sizeof(struct exec))
	mov	EXECSIZE, %o2
	cmp	%o0, EXECSIZE		! got full header?
	bne	__err_exit		! no
	nop
	!
	! Check out exec header.
	!
	lduh	[EXECP + A_MAGIC], %g1	! check magic number
	cmp	%g1, ZMAGIC
	bne	__err_exit		! wrong
	nop
	!
	! Determine how much of the address space the link editor will occupy.
	!
	ld	[EXECP + A_TEXT], %txt	! text size
	ld	[EXECP + A_DATA], %g1	! data size
	ld	[EXECP + A_BSS], %bss	! bss size
	add	%txt, %g1, %td		! size (o1) =
	add	%td, %bss, %o1		!   exec.a_text+exec.a_data+exec.a_bss
	!
	! Map in the link editor.
	! We map the whole thing in including bss so that we get an address
	! assignment which will accomodate the whole thing.
	!
	! baseaddr = mmap(0, size, MMAP_PROT, MMAP_ASSIGN, rlfd, 0)
	!
	set	Lerrmsg2, %errmp	! setup error message
	mov	0, %o0			! let system assign addr
					! how much we're mapping is in %o1
	set	MMAP_RO, %o2		! access protection
	set	MMAP_ASSIGN, %o3	! mapping flags
	mov	%rlfd, %o4		! fd for link editor
	call	__mmap, 6
	mov	0, %o5			! no offset

	mov	%o0, %baseaddr		! save addr
	add	%o0, %txt, %o0		! now map data segment read/write
	ld	[EXECP + A_DATA], %o1
	set	MMAP_PROT, %o2		! access protection
	set	MMAP_FIXED, %o3		! mapping flags
	mov	%rlfd, %o4		! fd for link editor
	call	__mmap, 6
	mov	%txt, %o5		! data  offset

	!
	! Obtain /dev/zero to map in for bss.
	!
	set	Lerrmsg3, %errmp	! setup error message
	set	Ldevzero, %o0		! dzfd = open("/dev/zero", 0)
	call	__open, 2
	mov	0, %o1

	mov	%o0, %dzfd		! result
	!
	! Map in /dev/zero to form bss for link editor.
	!
	ld	[EXECP + A_BSS], %g1	! if there is no bss skip this
	tst	%g1
	bz	1f
	.empty				! don't care about delay slot

	set	Lerrmsg2, %errmp
	!
	! mmap(baseaddr+exec.a_text+exec.a_data, exec.a_bss,
	!	MMAP_PROT, MMAP_FIXED, dzfd, 0)
	!
	add	%baseaddr, %td, %o0	! &bss = baseaddr+text+data
	mov	%bss, %o1		! map a bss's worth
	set	MMAP_PROT, %o2
	set	MMAP_FIXED, %o3		! use the address specified
	mov	%dzfd, %o4		! /dev/zero
	call	__mmap, 6
	mov	0, %o5			! map from offset zero
	!
	! Fill in the interface struct and call the link editor.
	!
1:
	ld	[EXECP + A_ENTRY], %g1	! compute ld.so entry point
	add	%baseaddr, %g1, %l0
	st	%baseaddr, [RTLDP + BASEADDR]	! pass base address
	st	%dzfd, [RTLDP + DZFD]		!  and /dev/zero
	st	%rlfd, [RTLDP + RLFD]		!  and ld.so file
	sethi	%hi(__dp), %g1
	ld	[%g1 + %lo(__dp)], %g1
	st	%g1, [RTLDP + UDP]		!  and program's DYNAMIC
	sethi	%hi(_environ), %g1
	ld	[%g1 + %lo(_environ)], %g1
	st	%g1, [RTLDP + ENV]		!  and _environ
	set	__main, %g1
	st	%g1, [RTLDP + BRKADDR]		! and breakpoint addr
	mov	RTLD_VERS, %o0
	jmpl	%l0, %o7		! ld.so(version number, intfc offset)
	mov	RTLDOFF, %o1
	!
	! Done.
	!
	ret
	restore

/*
 * System call interfaces.
 */

#define STDERR		2
#define ERRSTATUS	0x7f

__open:
	ba	__syscall
	mov	SYS_open, %g1

__mmap:
	ba	__syscall
	mov	SYS_mmap, %g1

__read:
	ba	__syscall
	mov	SYS_read, %g1

__syscall:
	t	0			! call the system call
	bcs	__err_exit		! test for error
	nop
	retl				! return
	nop

__err_exit:
	mov	STDERR, %o0
	add	%errmp, 0x4, %o1
	ld	[%errmp], %o2
	mov	SYS_write, %g1		! write(stderr, errmp+4, *errmp)
	t	0

	mov	ERRSTATUS, %o0		! exit(ERRSTATUS)
	mov	SYS_exit, %g1
	t	0
	/*NOTREACHED*/

/*
 * initialized, read only data.
 */
	.align	4
__dp:
	.word	__DYNAMIC
__link_editor:
	.ascii	"/usr/lib/ld.so\0"
Ldevzero:
	.ascii	"/dev/zero\0"
	.align	4
Lerrmsg1:
	.word	0x19
	.ascii	"crt0: no /usr/lib/ld.so\12\0"
	.align	4
Lerrmsg2:
	.word	0x26
	.ascii	"crt0: /usr/lib/ld.so mapping failure\12\0"
	.align	4
Lerrmsg3:
	.word	0x14
	.ascii	"crt0: no /dev/zero\12\0"
	.align	4

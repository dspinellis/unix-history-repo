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

!	.asciz "@(#)rtldlib.s 1.11 88/02/08 SMI"
!
!Copyright (c) 1987, 1991 by Sun Microsystems, Inc.
!
#include <machine/asm_linkage.h>
#include <sys/syscall.h>

#define VERS1		1
#define BASEADDR	0
#define version		%i0
#define ip		%l0
#define baseaddr	%l2
#define RELOCOFF	0xc

	.seg    "text"
	.global	_binder,_rtbinder


    ! rtl(versionnumber)
    ! for version number 1 the following argument are passed on the stack:
    !		- base address where ld.so is mapped to
    !		- file descriptor for /dev/zero
    !		- file descriptor for ld.so
    !		- user dynamic pointer
    !		- environment pointer
    !		- break address for adb/dbx
_rtl:
	save	%sp,-SA(MINFRAME),%sp
L1:
	call    1f
        nop
1:
        sethi	%hi(__GLOBAL_OFFSET_TABLE_ - (L1 - 1b)), %l7
L2:
	or	%l7, %lo(__GLOBAL_OFFSET_TABLE_ - (L1 - L2)), %l7
	add	%l7, %o7, %l7
version1:
	mov	version,%o0
	add	%fp,%i1,ip		! get interface pointer
	mov	ip,%o1			! ptr to interface structure
	ld	[ip+BASEADDR],baseaddr	! address where ld.so is mapped in
	ld	[%l7],%l1		! ptr to ld.so first entry in globtable
	add	baseaddr,%l1,%o2	! push runtime linker dynamic pointer
	ld	[%l7+_rtld],%g1		! manually fix pic reference to rtld
	add	%g1,baseaddr,%g1
	jmpl	%g1,%o7
	nop
	mov	0,%o0
	mov	%o0,%i0
	ret
	restore

_rtbinder:
	save	%sp,-SA(MINFRAME),%sp
	mov	%i7,%o0			! %o0 has the address of the second
					! instruction of the jump slot
	ld	[%o0+RELOCOFF],%o1	! relocation index in %o1
	call	_binder
	nop
	mov	%o0,%g1			! save address of routine binded
	restore				! pop frame used for rtbinder
	restore				! pop frame used for jump slot
	jmp	%g1			! jump to it
	nop

! Special system call stubs to save system call overhead

	.global	_getreuid, _getregid
_getreuid:
	save	%sp,-SA(MINFRAME),%sp	! Build a frame
	mov	SYS_getuid,%g1		! Set system call
	t	0			! Do it
	st	%o0,[%i0]		! Store real uid
	st	%o1,[%i1]		!   and store effective uid
	ret				! Pipeline the return
	restore				!   with the frame pop

_getregid:
	save	%sp,-SA(MINFRAME),%sp	! Build a frame
	mov	SYS_getgid,%g1		! Set system call
	t	0			! Do it
	st	%o0,[%i0]		! Store real gid
	st	%o1,[%i1]		!   and store effective gid
	ret				! Pipeline the return
	restore				!   with the frame pop

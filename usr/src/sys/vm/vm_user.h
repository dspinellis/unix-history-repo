/* 
 * Copyright (c) 1986, Avadis Tevanian, Jr., Michael Wayne Young
 * Copyright (c) 1987 Carnegie-Mellon University
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * The CMU software License Agreement specifies the terms and conditions
 * for use and redistribution.
 *
 *	@(#)vm_user.h	7.1 (Berkeley) %G%
 */

/*
 *	Kernel memory management definitions.
 */

#ifndef	_VM_USER_
#define	_VM_USER_

int	vm_allocate();
int	vm_deallocate();
int	vm_inherit();
int	vm_protect();
int	vm_statistics();

#endif	_VM_USER_

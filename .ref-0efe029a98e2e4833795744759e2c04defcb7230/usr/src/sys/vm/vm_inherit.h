/* 
 * Copyright (c) 1985, Avadis Tevanian, Jr., Michael Wayne Young
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
 *	@(#)vm_inherit.h	7.1 (Berkeley) %G%
 */

/*
 *	Virtual memory map inheritance definitions.
 */

#ifndef	_VM_INHERIT_
#define	_VM_INHERIT_

/*
 *	Types defined:
 *
 *	vm_inherit_t	inheritance codes.
 */

typedef int		vm_inherit_t;	/* might want to change this */

/*
 *	Enumeration of valid values for vm_inherit_t.
 */

#define	VM_INHERIT_SHARE	((vm_inherit_t) 0)	/* share with child */
#define	VM_INHERIT_COPY		((vm_inherit_t) 1)	/* copy into child */
#define VM_INHERIT_NONE		((vm_inherit_t) 2)	/* absent from child */
#define	VM_INHERIT_DONATE_COPY	((vm_inherit_t) 3)	/* copy and delete */

#define VM_INHERIT_DEFAULT	VM_INHERIT_COPY

#endif	_VM_INHERIT_

/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)kern_exec.c	7.43 (Berkeley) 5/9/91
 */

#include "param.h"
#include "systm.h"
#include "filedesc.h"
#include "kernel.h"
#include "proc.h"
#include "mount.h"
#include "malloc.h"
#include "namei.h"
#include "vnode.h"
#include "seg.h"
#include "file.h"
#include "acct.h"
#include "exec.h"
#include "ktrace.h"
#include "resourcevar.h"

#include "machine/cpu.h"
#include "machine/reg.h"

#include "mman.h"
#include "vm/vm.h"
#include "vm/vm_param.h"
#include "vm/vm_map.h"
#include "vm/vm_kern.h"
#include "vm/vm_pager.h"

#include "signalvar.h"
#include "kinfo_proc.h"

#ifdef HPUXCOMPAT
#include "user.h"			/* for pcb */
#include "hp300/hpux/hpux_exec.h"
#endif

#ifdef COPY_SIGCODE
extern char sigcode[], esigcode[];
#define	szsigcode	(esigcode - sigcode)
#else
#define	szsigcode	0
#endif

/*
 * exec system call
 */
/* ARGSUSED */
execve(p, uap, retval)
	register struct proc *p;
	register struct args {
		char	*fname;
		char	**argp;
		char	**envp;
	} *uap;
	int *retval;
{

	/*
	 * Body deleted.
	 */
	return (ENOSYS);
}

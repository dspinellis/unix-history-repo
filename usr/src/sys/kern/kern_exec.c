/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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

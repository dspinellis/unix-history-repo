/*
 * Copyright (c) 1992 William Jolitz. All rights reserved.
 * Written by William Jolitz 1/92
 *
 * Redistribution and use in source and binary forms are freely permitted
 * provided that the above copyright notice and attribution and date of work
 * and this paragraph are duplicated in all such forms.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This procedure implements a minimal program execution facility for
 * 386BSD. It interfaces to the BSD kernel as the execve system call.
 * Significant limitations and lack of compatiblity with POSIX are
 * present with this version, to make its basic operation more clear.
 *
 */

#include "param.h"
#include "systm.h"
#include "proc.h"
#include "mount.h"
#include "namei.h"
#include "vnode.h"
#include "file.h"
#include "exec.h"
#include "stat.h"
#include "wait.h"
#include "signalvar.h"
#include "mman.h"

#include "vm/vm.h"
#include "vm/vm_param.h"
#include "vm/vm_map.h"
#include "vm/vm_kern.h"

#include "machine/reg.h"

static char rcsid[] = "$Header: /usr/bill/working/sys/kern/RCS/kern_execve.c,v 1.3 92/01/21 21:29:13 william Exp $";

/*
 * Bill's first-cut execve() system call. Puts hair on your chest.
 */

/* ARGSUSED */
execve(p, uap, retval)
	struct proc *p;
	register struct args {
		char	*fname;
		char	**argp;
		char	**envp;
	} *uap;
	int *retval;
{
	register struct nameidata *ndp;
	int rv, amt;
	struct nameidata nd;
	struct exec hdr;
	char **kargbuf, **kargbufp, *kstringbuf, *kstringbufp;
	char **org, **vectp, *ep;
	u_int	needsenv, limitonargs, stringlen;
	int addr, size;
	int argc;
	char *cp;
	struct stat statb;
	struct vmspace *vs;
	int tsize, dsize, bsize, cnt, foff;

	/*
	 * Step 1. Lookup filename to see if we have something to execute.
	 */
	ndp = &nd;
	ndp->ni_nameiop = LOOKUP | LOCKLEAF | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;

	/* is it there? */
	if (rv = namei(ndp, p))
		return (rv);

	/* is it a regular file? */
	if (ndp->ni_vp->v_type != VREG) {
		vput(ndp->ni_vp);
		return(ENOEXEC);
	}

	/* is it executable? */
	rv = VOP_ACCESS(ndp->ni_vp, VEXEC, p->p_ucred, p);
	if (rv)
		goto exec_fail;
	
	rv = vn_stat(ndp->ni_vp, &statb, p);
	if (rv)
		goto exec_fail;

	/*
	 * Step 2. Does the file contain a format we can
	 * understand and execute
	 */
	rv = vn_rdwr(UIO_READ, ndp->ni_vp, (caddr_t)&hdr, sizeof(hdr),
		0, UIO_SYSSPACE, IO_NODELOCKED, p->p_ucred, &amt, p);

	/* big enough to hold a header? */
	if (rv)
		goto exec_fail;
	
	/* that we recognize? */
	rv = ENOEXEC;
	if (hdr.a_magic != ZMAGIC)
		goto exec_fail;

	/* sanity check  "ain't not such thing as a sanity clause" -groucho */
	if (/*hdr.a_text == 0 || */ hdr.a_text > MAXTSIZ
		|| hdr.a_text % NBPG || hdr.a_text > statb.st_size)
		goto exec_fail;

	if (hdr.a_data == 0 || hdr.a_data > DFLDSIZ
		|| hdr.a_data > statb.st_size
		|| hdr.a_data + hdr.a_text > statb.st_size)
		goto exec_fail;

	if (hdr.a_bss > MAXDSIZ)
		goto exec_fail;
	
	if (hdr.a_text + hdr.a_data + hdr.a_bss > MAXTSIZ + MAXDSIZ)
		goto exec_fail;
	
	/*
	 * Step 3.  File and header are valid. Now, dig out the strings
	 * out of the old process image.
	 */

	/* assumption: most execve's have less than 256 arguments, with a
	 * total of string storage space not exceeding 2K. It is more
	 * frequent that when this fails, string space falls short first
	 * (e.g. as when a large termcap environment variable is present).
	 * It is infrequent when more than 256 arguments are used that take
	 * up less than 2K of space (e.g. args average more than 8 chars).
	 *
	 * What we give up in this implementation is a dense encoding of
	 * the data structure in the receiving program's address space.
	 * This means that there is plenty of wasted space (up to 6KB)
	 * as the price we pay for a fast, single pass algorithm.
	 *
	 * Our alternative would be to accumulate strings and pointers
	 * in the first pass, then, knowing the sizes and number of the
	 * strings, pack them neatly and tightly togeither in the second
	 * pass. This means two copies of the strings, and string copying
	 * is much of the cost of exec.
	 */

	/* allocate string buffer and arg buffer */
	org = kargbuf = (char **) kmem_alloc_wait(exec_map,
		(NCARGS + PAGE_SIZE)/PAGE_SIZE);
	kstringbuf = kstringbufp = ((char *)kargbuf) + NBPG/2;
	kargbuf += NBPG/(4*sizeof(int));
	kargbufp = kargbuf;

	/* first, do args */
	needsenv = 1;
	vectp = uap->argp;

do_env_as_well:
	cnt = 0;
	/* for each envp, copy in string */
	limitonargs = NCARGS;
	if(vectp == 0) goto dont_bother;
	do {
		/* did we outgrow initial argbuf, if so, die */
		if (kargbufp == (char **)kstringbuf)
			goto exec_fail;
	
		/* get an string pointer */
		ep = (char *)fuword(vectp++);
		if (ep == (char *)-1) {
			rv = EFAULT;
			goto exec_fail;
		}

		/* if not null pointer, copy in string */
		if (ep) {
			if (rv = copyinstr(ep, kstringbufp, limitonargs,
				&stringlen)) goto exec_fail;
			/* assume that strings usually all fit in last page */
			*kargbufp = (char *)(kstringbufp - kstringbuf
				+ USRSTACK - NBPG + NBPG/2);
			kargbufp++;
			cnt++;
			kstringbufp += stringlen;
			limitonargs -= stringlen + sizeof(long);
		} else {
			*kargbufp++ = 0;
			limitonargs -= sizeof(long);
			break;
		}
	} while (limitonargs > 0);

dont_bother:
	if (limitonargs <= 0) {
		rv = E2BIG;
		goto exec_fail;
	}

	if (needsenv) {
		argc = cnt;
		vectp = uap->envp;
		needsenv = 0;
		goto do_env_as_well;
	}
 
	kargbuf[-1] = (char *)argc;

	/*
	 * Step 4. Build the new processes image.
	 */

	/* At this point, we are committed -- destroy old executable */
	vs = p->p_vmspace;
	addr = 0;
	size = USRSTACK - addr;
	/* blow away all address space */
	rv = vm_deallocate(&vs->vm_map, addr, size, FALSE);
	if (rv)
		goto exec_abort;

	/* build a new address space */
	addr = 0;
	if (hdr.a_text == 0) {
		/* screwball mode */
		foff = tsize = 0;
		hdr.a_data += hdr.a_text;
	} else {
		tsize = roundup(hdr.a_text, NBPG);
		foff = NBPG;
	}
	dsize = roundup(hdr.a_data, NBPG);
	bsize = roundup(hdr.a_bss + dsize, NBPG);
	bsize -= dsize;

	/* map text & data*/
	rv = vm_mmap(&vs->vm_map, &addr, tsize+dsize, VM_PROT_ALL,
	MAP_FILE|MAP_COPY|MAP_FIXED, (caddr_t)ndp->ni_vp, foff);
	if (rv)
		goto exec_abort;

	/* r/w data, ro text */
	if (tsize) {
		addr = 0;
		rv = vm_protect(&vs->vm_map, addr, tsize, FALSE, VM_PROT_READ|VM_PROT_EXECUTE);
		if (rv)
			goto exec_abort;
	}

	/* create anonymous memory region for bss */
	addr = dsize + tsize;
	rv = vm_allocate(&vs->vm_map, &addr, bsize, FALSE);
	if (rv)
		goto exec_abort;

	/* create anonymous memory region for stack */
	addr = USRSTACK - MAXSSIZ;
	rv = vm_allocate(&vs->vm_map, &addr, MAXSSIZ, FALSE);
	if (rv)
		goto exec_abort;

	/*
	 * Step 5. Prepare process for execution.
	 */

	/* touchup process information */
	vs->vm_tsize = tsize/NBPG;		/* text size (pages) XXX */
	vs->vm_dsize = (dsize+bsize)/NBPG;	/* data size (pages) XXX */
	vs->vm_ssize = MAXSSIZ/NBPG;	/* stack size (pages) */
	vs->vm_taddr = 0;	/* user virtual address of text XXX */
	vs->vm_daddr = (caddr_t)tsize;	/* user virtual address of data XXX */
	/* user VA at max stack growth */
	vs->vm_maxsaddr = (caddr_t)(USRSTACK - MAXSSIZ);

	/* everything fits in a single page, no fixups, no more work */
	/* (groan) due to bug in vm_map_copy, can't remap. copy for now. */
	rv = copyout((caddr_t)org, (caddr_t)USRSTACK - NBPG, NBPG);
	if(rv)
		goto exec_abort;

	/* close files on exec, fixup signals */
	fdcloseexec(p);
	execsigs(p);

	p->p_regs[SP] = USRSTACK - NBPG + NBPG/4 - 4;
	vs->vm_ssize = 1;	/* stack size (pages) */
	setregs(p, hdr.a_entry);
	kmem_free_wakeup(exec_map, org, (NCARGS + PAGE_SIZE)/PAGE_SIZE);
	vput(ndp->ni_vp);
	return (0);

exec_fail:
	vput(ndp->ni_vp);
	return(rv);

exec_abort:
	/* untested and probably bogus */
	kmem_free_wakeup(exec_map, org, (NCARGS + PAGE_SIZE)/PAGE_SIZE);
	vput(ndp->ni_vp);
	exit(p, W_EXITCODE(0, SIGABRT));
	return(0);

}

/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)kvm_hp300.c	5.23 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * hp300 machine depedent routines for kvm.  Hopefully, the forthcoming 
 * vm code will one day obsolete this module.
 */

#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/stat.h>
#include <nlist.h>
#include <kvm.h>

#include <vm/vm.h>
#include <vm/vm_param.h>

#include <limits.h>
#include <ndbm.h>

#include "kvm_private.h"

#include <hp300/hp300/pte.h>

#ifndef btop
#define	btop(x)		(((unsigned)(x)) >> PGSHIFT)	/* XXX */
#define	ptob(x)		((caddr_t)((x) << PGSHIFT))	/* XXX */
#endif

struct vmstate {
	u_long lowram;
	struct ste *Sysseg;
};

#define KREAD(kd, addr, p)\
	(kvm_read(kd, addr, (char *)(p), sizeof(*(p))) != sizeof(*(p)))

void
_kvm_freevtop(kd)
	kvm_t *kd;
{
	if (kd->vmst != 0)
		free(kd->vmst);
}

int
_kvm_initvtop(kd)
	kvm_t *kd;
{
	struct vmstate *vm;
	struct nlist nlist[3];

	vm = (struct vmstate *)_kvm_malloc(kd, sizeof(*vm));
	if (vm == 0)
		return (-1);
	kd->vmst = vm;

	nlist[0].n_name = "_lowram";
	nlist[1].n_name = "_Sysseg";
	nlist[2].n_name = 0;

	if (kvm_nlist(kd, nlist) != 0) {
		_kvm_err(kd, kd->program, "bad namelist");
		return (-1);
	}
	vm->Sysseg = 0;
	if (KREAD(kd, (u_long)nlist[0].n_value, &vm->lowram)) {
		_kvm_err(kd, kd->program, "cannot read lowram");
		return (-1);
	}
	if (KREAD(kd, (u_long)nlist[1].n_value, &vm->Sysseg)) {
		_kvm_err(kd, kd->program, "cannot read segment table");
		return (-1);
	}
	return (0);
}

static int
_kvm_vatop(kd, sta, va, pa)
	kvm_t *kd;
	struct ste *sta;
	u_long va;
	u_long *pa;
{
	register struct vmstate *vm;
	register u_long addr;
	int p, ste, pte;
	int offset;
	register u_long lowram;

	vm = kd->vmst;
	/*
	 * If processing a post-mortem and we are initializing
	 * (kernel segment table pointer not yet set) then return
	 * pa == va to avoid infinite recursion.
	 */
	if (!ISALIVE(kd) && vm->Sysseg == 0) {
		*pa = va;
		return (NBPG - (va & PGOFSET));
	}
	addr = (u_long)&sta[va >> SEGSHIFT];
	/*
	 * Can't use KREAD to read kernel segment table entries.
	 * Fortunately it is 1-to-1 mapped so we don't have to. 
	 */
	if (sta == vm->Sysseg) {
		if (lseek(kd->pmfd, (off_t)addr, 0) == -1 ||
		    read(kd->pmfd, (char *)&ste, sizeof(ste)) < 0)
			goto invalid;
	} else if (KREAD(kd, addr, &ste))
		goto invalid;
	if ((ste & SG_V) == 0) {
		_kvm_err(kd, 0, "invalid segment (%x)", ste);
		return((off_t)0);
	}
	p = btop(va & SG_PMASK);
	addr = (ste & SG_FRAME) + (p * sizeof(struct pte));
	lowram = vm->lowram;

	/*
	 * Address from STE is a physical address so don't use kvm_read.
	 */
	if (lseek(kd->pmfd, (off_t)addr - lowram, 0) == -1 || 
	    read(kd->pmfd, (char *)&pte, sizeof(pte)) < 0)
		goto invalid;
	addr = pte & PG_FRAME;
	if (pte == PG_NV || addr < lowram) {
		_kvm_err(kd, 0, "page not valid");
		return (0);
	}
	offset = va & PGOFSET;
	*pa = addr - lowram + offset;
	
	return (NBPG - offset);
invalid:
	_kvm_err(kd, 0, "invalid address (%x)", va);
	return (0);
}

int
_kvm_kvatop(kd, va, pa)
	kvm_t *kd;
	u_long va;
	u_long *pa;
{
	if (va >= KERNBASE)
		return (_kvm_vatop(kd, (u_long)kd->vmst->Sysseg, va, pa));
	_kvm_err(kd, 0, "invalid address (%x)", va);
	return (0);
}

/*
 * Translate a user virtual address to a physical address.
 */
int
_kvm_uvatop(kd, p, va, pa)
	kvm_t *kd;
	const struct proc *p;
	u_long va;
	u_long *pa;
{
	register struct vmspace *vms = p->p_vmspace;
	int kva;

	kva = (int)&vms->vm_pmap.pm_stab;
	if (kvm_read(kd, kva, (char *)&kva, 4) != 4) {
		_kvm_err(kd, 0, "invalid address (%x)", va);
		return (0);
	}
	return (_kvm_vatop(kd, kva, va, pa));
}

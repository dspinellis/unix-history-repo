/*-
 * Copyright (c) 1989, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software developed by the Computer Systems
 * Engineering group at Lawrence Berkeley Laboratory under DARPA contract
 * BG 91-66 and contributed to Berkeley. Modified for MIPS by Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)kvm_mips.c	1.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */
/*
 * MIPS machine dependent routines for kvm.  Hopefully, the forthcoming 
 * vm code will one day obsolete this module.
 */

#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/stat.h>
#include <unistd.h>
#include <nlist.h>
#include <kvm.h>

#include <vm/vm.h>
#include <vm/vm_param.h>

#include <limits.h>
#include <db.h>

#include "kvm_private.h"

#include <machine/machConst.h>
#include <machine/pte.h>
#include <machine/pmap.h>

#define KREAD(kd, addr, p)\
	(kvm_read(kd, addr, (char *)(p), sizeof(*(p))) != sizeof(*(p)))

void
_kvm_freevtop(kd)
	kvm_t *kd;
{
}

int
_kvm_initvtop(kd)
	kvm_t *kd;
{

	return (0);
}

/*
 * Translate a kernel virtual address to a physical address.
 */
int
_kvm_kvatop(kd, va, pa)
	kvm_t *kd;
	u_long va;
	u_long *pa;
{
	u_long pte, addr, offset;

	if (va < KERNBASE ||
	    va >= VM_MIN_KERNEL_ADDRESS + PMAP_HASH_KPAGES * NPTEPG * NBPG)
		goto invalid;
	if (va < VM_MIN_KERNEL_ADDRESS) {
		*pa = MACH_CACHED_TO_PHYS(va);
		return (NBPG - (va & PGOFSET));
	}
	addr = PMAP_HASH_KADDR + ((va - VM_MIN_KERNEL_ADDRESS) >> PGSHIFT);
	/*
	 * Can't use KREAD to read kernel segment table entries.
	 * Fortunately it is 1-to-1 mapped so we don't have to. 
	 */
	if (lseek(kd->pmfd, (off_t)addr, 0) < 0 ||
	    read(kd->pmfd, (char *)&pte, sizeof(pte)) < 0)
		goto invalid;
	offset = va & PGOFSET;
	*pa = (pte & PG_FRAME) | offset;
	return (NBPG - offset);

invalid:
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
#if 0
	register struct vmspace *vms = p->p_vmspace;
	u_long stab_kva, kva;

	kva = (u_long)&vms->vm_pmap.pm_stab;
	if (kvm_read(kd, kva, (char *)&kva, 4) != 4) {
		_kvm_err(kd, 0, "invalid address (%x)", va);
		return (0);
	}
	return (_kvm_vatop(kd, kva, va, pa));
#else
	_kvm_err(kd, 0, "invalid address (%x)", va);
	return (0);
#endif
}

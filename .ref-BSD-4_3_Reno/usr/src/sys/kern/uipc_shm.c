/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department. Originally from University of Wisconsin.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: uipc_shm.c 1.9 89/08/14$
 *
 *	@(#)uipc_shm.c	7.9 (Berkeley) 6/28/90
 */

/*
 * System V shared memory routines.
 * TEMPORARY, until mmap is in place;
 * needed now for HP-UX compatibility and X server (yech!).
 */

#ifdef SYSVSHM

#include "machine/pte.h"

#include "param.h"
#include "systm.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "vm.h"
#include "shm.h"
#include "mapmem.h"
#include "malloc.h"

#ifdef HPUXCOMPAT
#include "../hpux/hpux.h"
#endif

int	shmat(), shmctl(), shmdt(), shmget();
int	(*shmcalls[])() = { shmat, shmctl, shmdt, shmget };
int	shmtot = 0;

int	shmfork(), shmexit();
struct	mapmemops shmops = { shmfork, (int (*)())0, shmexit, shmexit };

shminit()
{
	register int i;

	if (shminfo.shmmni > SHMMMNI)
		shminfo.shmmni = SHMMMNI;
	for (i = 0; i < shminfo.shmmni; i++) {
		shmsegs[i].shm_perm.mode = 0;
		shmsegs[i].shm_perm.seq = 0;
	}
}

/*
 * Entry point for all SHM calls
 */
shmsys(p, uap, retval)
	struct proc *p;
	struct args {
		u_int which;
	} *uap;
	int *retval;
{

	if (uap->which >= sizeof(shmcalls)/sizeof(shmcalls[0]))
		return (EINVAL);
	return ((*shmcalls[uap->which])(p, &uap[1], retval));
}

/*
 * Get a shared memory segment
 */
shmget(p, uap, retval)
	struct proc *p;
	register struct args {
		key_t key;
		int size;
		int shmflg;
	} *uap;
	int *retval;
{
	register struct shmid_ds *shp;
	register struct ucred *cred = u.u_cred;
	register int i;
	int error, size, rval = 0;
	caddr_t kva;

	/* look up the specified shm_id */
	if (uap->key != IPC_PRIVATE) {
		for (i = 0; i < shminfo.shmmni; i++)
			if ((shmsegs[i].shm_perm.mode & SHM_ALLOC) &&
			    shmsegs[i].shm_perm.key == uap->key) {
				rval = i;
				break;
			}
	} else
		i = shminfo.shmmni;

	/* create a new shared segment if necessary */
	if (i == shminfo.shmmni) {
		if ((uap->shmflg & IPC_CREAT) == 0)
			return (ENOENT);
		if (uap->size < shminfo.shmmin || uap->size > shminfo.shmmax)
			return (EINVAL);
		for (i = 0; i < shminfo.shmmni; i++)
			if ((shmsegs[i].shm_perm.mode & SHM_ALLOC) == 0) {
				rval = i;
				break;
			}
		if (i == shminfo.shmmni)
			return (ENOSPC);
		size = clrnd(btoc(uap->size));
		if (shmtot + size > shminfo.shmall)
			return (ENOMEM);
		shp = &shmsegs[rval];
		/*
		 * We need to do a couple of things to ensure consistency
		 * in case we sleep in malloc().  We mark segment as
		 * allocated so that other shmgets() will not allocate it.
		 * We mark it as "destroyed" to insure that shmvalid() is
		 * false making most operations fail (XXX).  We set the key,
		 * so that other shmget()s will fail.
		 */
		shp->shm_perm.mode = SHM_ALLOC | SHM_DEST;
		shp->shm_perm.key = uap->key;
		kva = (caddr_t) malloc((u_long)ctob(size), M_SHM, M_WAITOK);
		if (kva == NULL) {
			shp->shm_perm.mode = 0;
			return (ENOMEM);
		}
		if (!claligned(kva))
			panic("shmget: non-aligned memory");
		bzero(kva, (u_int)ctob(size));
		shmtot += size;
		shp->shm_perm.cuid = shp->shm_perm.uid = cred->cr_uid;
		shp->shm_perm.cgid = shp->shm_perm.gid = cred->cr_gid;
		shp->shm_perm.mode = SHM_ALLOC | (uap->shmflg&0777);
		shp->shm_handle = (void *) kvtopte(kva);
		shp->shm_segsz = uap->size;
		shp->shm_cpid = p->p_pid;
		shp->shm_lpid = shp->shm_nattch = 0;
		shp->shm_atime = shp->shm_dtime = 0;
		shp->shm_ctime = time.tv_sec;
	} else {
		shp = &shmsegs[rval];
		/* XXX: probably not the right thing to do */
		if (shp->shm_perm.mode & SHM_DEST)
			return (EBUSY);
		if (error = ipcaccess(&shp->shm_perm, uap->shmflg&0777, cred))
			return (error);
		if (uap->size && uap->size > shp->shm_segsz)
			return (EINVAL);
		if ((uap->shmflg&IPC_CREAT) && (uap->shmflg&IPC_EXCL))
			return (EEXIST);
	}
	*retval = shp->shm_perm.seq * SHMMMNI + rval;
	return (0);
}

/*
 * Shared memory control
 */
/* ARGSUSED */
shmctl(p, uap, retval)
	struct proc *p;
	register struct args {
		int shmid;
		int cmd;
		caddr_t buf;
	} *uap;
	int *retval;
{
	register struct shmid_ds *shp;
	register struct ucred *cred = u.u_cred;
	struct shmid_ds sbuf;
	int error;

	if (error = shmvalid(uap->shmid))
		return (error);
	shp = &shmsegs[uap->shmid % SHMMMNI];
	switch (uap->cmd) {
	case IPC_STAT:
		if (error = ipcaccess(&shp->shm_perm, IPC_R, cred))
			return (error);
		return (copyout((caddr_t)shp, uap->buf, sizeof(*shp)));

	case IPC_SET:
		if (cred->cr_uid && cred->cr_uid != shp->shm_perm.uid &&
		    cred->cr_uid != shp->shm_perm.cuid)
			return (EPERM);
		if (error = copyin(uap->buf, (caddr_t)&sbuf, sizeof sbuf))
			return (error);
		shp->shm_perm.uid = sbuf.shm_perm.uid;
		shp->shm_perm.gid = sbuf.shm_perm.gid;
		shp->shm_perm.mode = (shp->shm_perm.mode & ~0777)
			| (sbuf.shm_perm.mode & 0777);
		shp->shm_ctime = time.tv_sec;
		break;

	case IPC_RMID:
		if (cred->cr_uid && cred->cr_uid != shp->shm_perm.uid &&
		    cred->cr_uid != shp->shm_perm.cuid)
			return (EPERM);
		/* set ctime? */
		shp->shm_perm.key = IPC_PRIVATE;
		shp->shm_perm.mode |= SHM_DEST;
		if (shp->shm_nattch <= 0)
			shmfree(shp);
		break;

#ifdef HPUXCOMPAT
	case SHM_LOCK:
	case SHM_UNLOCK:
		/* don't really do anything, but make them think we did */
		if ((p->p_flag & SHPUX) == 0)
			return (EINVAL);
		if (cred->cr_uid && cred->cr_uid != shp->shm_perm.uid &&
		    cred->cr_uid != shp->shm_perm.cuid)
			return (EPERM);
		break;
#endif

	default:
		return (EINVAL);
	}
	return (0);
}

/*
 * Attach to shared memory segment.
 */
shmat(p, uap, retval)
	struct proc *p;
	register struct args {
		int	shmid;
		caddr_t	shmaddr;
		int	shmflg;
	} *uap;
	int *retval;
{
	register struct shmid_ds *shp;
	register int size;
	struct mapmem *mp;
	caddr_t uva;
	int error, prot, shmmapin();

	if (error = shmvalid(uap->shmid))
		return (error);
	shp = &shmsegs[uap->shmid % SHMMMNI];
	if (shp->shm_handle == NULL)
		panic("shmat NULL handle");
	if (error = ipcaccess(&shp->shm_perm,
			(uap->shmflg&SHM_RDONLY) ? IPC_R : IPC_R|IPC_W, u.u_cred))
		return (error);
	uva = uap->shmaddr;
	if (uva && ((int)uva & (SHMLBA-1))) {
		if (uap->shmflg & SHM_RND)
			uva = (caddr_t) ((int)uva & ~(SHMLBA-1));
		else
			return (EINVAL);
	}
	/*
	 * Make sure user doesn't use more than their fair share
	 */
	size = 0;
	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_ops == &shmops)
			size++;
	if (size >= shminfo.shmseg)
		return (EMFILE);
	/*
	 * Allocate a mapped memory region descriptor and
	 * attempt to expand the user page table to allow for region
	 */
	prot = (uap->shmflg & SHM_RDONLY) ? MM_RO : MM_RW;
#if defined(hp300)
	prot |= MM_CI;
#endif
	size = ctob(clrnd(btoc(shp->shm_segsz)));
	error = mmalloc(p, uap->shmid, &uva, (segsz_t)size, prot, &shmops, &mp);
	if (error)
		return (error);
	if (error = mmmapin(p, mp, shmmapin)) {
		(void) mmfree(p, mp);
		return (error);
	}
	/*
	 * Fill in the remaining fields
	 */
	shp->shm_lpid = p->p_pid;
	shp->shm_atime = time.tv_sec;
	shp->shm_nattch++;
	*retval = (int) uva;
	return (0);
}

/*
 * Detach from shared memory segment.
 */
/* ARGSUSED */
shmdt(p, uap, retval)
	struct proc *p;
	struct args {
		caddr_t	shmaddr;
	} *uap;
	int *retval;
{
	register struct mapmem *mp;

	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_ops == &shmops && mp->mm_uva == uap->shmaddr)
			break;
	if (mp == MMNIL)
		return (EINVAL);
	shmsegs[mp->mm_id % SHMMMNI].shm_lpid = p->p_pid;
	return (shmufree(p, mp));
}

shmmapin(mp, off)
	struct mapmem *mp;
{
	register struct shmid_ds *shp;

	shp = &shmsegs[mp->mm_id % SHMMMNI];
	if (off >= ctob(clrnd(btoc(shp->shm_segsz))))
		return(-1);
	return(((struct pte *)shp->shm_handle)[btop(off)].pg_pfnum);
}

/*
 * Increment attach count on fork
 */
/* ARGSUSED */
shmfork(mp, ischild)
	register struct mapmem *mp;
{
	if (!ischild)
		shmsegs[mp->mm_id % SHMMMNI].shm_nattch++;
}

/*
 * Detach from shared memory segment on exit (or exec)
 */
shmexit(mp)
	struct mapmem *mp;
{
	struct proc *p = u.u_procp;		/* XXX */

	return (shmufree(p, mp));
}

shmvalid(id)
	register int id;
{
	register struct shmid_ds *shp;

	if (id < 0 || (id % SHMMMNI) >= shminfo.shmmni)
		return(EINVAL);
	shp = &shmsegs[id % SHMMMNI];
	if (shp->shm_perm.seq == (id / SHMMMNI) &&
	    (shp->shm_perm.mode & (SHM_ALLOC|SHM_DEST)) == SHM_ALLOC)
		return(0);
	return(EINVAL);
}

/*
 * Free user resources associated with a shared memory segment
 */
shmufree(p, mp)
	struct proc *p;
	struct mapmem *mp;
{
	register struct shmid_ds *shp;
	int error;

	shp = &shmsegs[mp->mm_id % SHMMMNI];
	mmmapout(p, mp);
	error = mmfree(p, mp);
	shp->shm_dtime = time.tv_sec;
	if (--shp->shm_nattch <= 0 && (shp->shm_perm.mode & SHM_DEST))
		shmfree(shp);
	return (error);
}

/*
 * Deallocate resources associated with a shared memory segment
 */
shmfree(shp)
	register struct shmid_ds *shp;
{
	caddr_t kva;

	if (shp->shm_handle == NULL)
		panic("shmfree");
	kva = (caddr_t) ptetokv(shp->shm_handle);
	free(kva, M_SHM);
	shp->shm_handle = NULL;
	shmtot -= clrnd(btoc(shp->shm_segsz));
	shp->shm_perm.mode = 0;
	/*
	 * Increment the sequence number to ensure that outstanding
	 * shmids for this segment will be invalid in the event that
	 * the segment is reallocated.  Note that shmids must be
	 * positive as decreed by SVID.
	 */
	shp->shm_perm.seq++;
	if ((int)(shp->shm_perm.seq * SHMMMNI) < 0)
		shp->shm_perm.seq = 0;
}

/*
 * XXX This routine would be common to all sysV style IPC
 *     (if the others were implemented).
 */
ipcaccess(ipc, mode, cred)
	register struct ipc_perm *ipc;
	int mode;
	register struct ucred *cred;
{
	register int m;

	if (cred->cr_uid == 0)
		return(0);
	/*
	 * Access check is based on only one of owner, group, public.
	 * If not owner, then check group.
	 * If not a member of the group, then check public access.
	 */
	mode &= 0700;
	m = ipc->mode;
	if (cred->cr_uid != ipc->uid && cred->cr_uid != ipc->cuid) {
		m <<= 3;
		if (!groupmember(ipc->gid, cred) &&
		    !groupmember(ipc->cgid, cred))
			m <<= 3;
	}
	if ((mode&m) == mode)
		return (0);
	return (EACCES);
}

#endif /* SYSVSHM */

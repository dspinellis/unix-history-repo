/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department. Originally from University of Wisconsin.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: uipc_shm.c 1.9 89/08/14$
 *
 *	@(#)sysv_shm.c	7.5 (Berkeley) %G%
 */

/*
 * System V shared memory routines.
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

/* entry point for all SHM calls */
shmsys()
{
	struct a {
		int which;
	} *uap = (struct a *)u.u_ap;

	if (uap->which >= sizeof(shmcalls)/sizeof(shmcalls[0])) {
		u.u_error = EINVAL;
		return;
	}
	(*shmcalls[uap->which])(u.u_ap+1);
}

/* get a shared memory segment */
shmget(ap)
	int *ap;
{
	register struct a {
		key_t key;
		int size;
		int shmflg;
	} *uap = (struct a *)ap;
	struct proc *p = u.u_procp;
	register struct shmid_ds *shp;
	register int i;
	int rval = 0, size;
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
		if ((uap->shmflg & IPC_CREAT) == 0) {
			u.u_error = ENOENT;
			return;
		}
		if (uap->size < shminfo.shmmin || uap->size > shminfo.shmmax) {
			u.u_error = EINVAL;
			return;
		}
		for (i = 0; i < shminfo.shmmni; i++)
			if ((shmsegs[i].shm_perm.mode & SHM_ALLOC) == 0) {
				rval = i;
				break;
			}
		if (i == shminfo.shmmni) {
			u.u_error = ENOSPC;
			return;
		}
		size = clrnd(btoc(uap->size));
		if (shmtot + size > shminfo.shmall) {
			u.u_error = ENOMEM;
			return;
		}
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
			u.u_error = ENOMEM;
			return;
		}
		if (!claligned(kva))
			panic("shmget: non-aligned memory");
		bzero(kva, (u_int)ctob(size));
		shmtot += size;
		shp->shm_perm.cuid = shp->shm_perm.uid = u.u_uid;
		shp->shm_perm.cgid = shp->shm_perm.gid = u.u_gid;
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
		if (shp->shm_perm.mode & SHM_DEST) {
			u.u_error = EBUSY;
			return;
		}
		if (!ipcaccess(&shp->shm_perm, uap->shmflg&0777))
			return;
		if (uap->size && uap->size > shp->shm_segsz) {
			u.u_error = EINVAL;
			return;
		}
		if ((uap->shmflg&IPC_CREAT) && (uap->shmflg&IPC_EXCL)) {
			u.u_error = EEXIST;
			return;
		}
	}
	u.u_r.r_val1 = shp->shm_perm.seq * SHMMMNI + rval;
}

/* shared memory control */
shmctl(ap)
	int *ap;
{
	register struct a {
		int shmid;
		int cmd;
		caddr_t buf;
	} *uap = (struct a *)ap;
	struct proc *p = u.u_procp;
	register struct shmid_ds *shp;
	struct shmid_ds sbuf;

	if (!shmvalid(uap->shmid))
		return;
	shp = &shmsegs[uap->shmid % SHMMMNI];
	switch (uap->cmd) {
	case IPC_STAT:
		if (ipcaccess(&shp->shm_perm, IPC_R))
			u.u_error =
				copyout((caddr_t)shp, uap->buf, sizeof(*shp));
		break;

	case IPC_SET:
		if (u.u_uid && u.u_uid != shp->shm_perm.uid &&
		    u.u_uid != shp->shm_perm.cuid) {
			u.u_error = EPERM;
			break;
		}
		u.u_error = copyin(uap->buf, (caddr_t)&sbuf, sizeof sbuf);
		if (!u.u_error) {
			shp->shm_perm.uid = sbuf.shm_perm.uid;
			shp->shm_perm.gid = sbuf.shm_perm.gid;
			shp->shm_perm.mode = (shp->shm_perm.mode & ~0777)
				| (sbuf.shm_perm.mode & 0777);
			shp->shm_ctime = time.tv_sec;
		}
		break;

	case IPC_RMID:
		if (u.u_uid && u.u_uid != shp->shm_perm.uid &&
		    u.u_uid != shp->shm_perm.cuid) {
			u.u_error = EPERM;
			break;
		}
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
			u.u_error = EINVAL;
		else if (u.u_uid && u.u_uid != shp->shm_perm.uid &&
			 u.u_uid != shp->shm_perm.cuid)
			u.u_error = EPERM;
		break;
#endif

	default:
		u.u_error = EINVAL;
		break;
	}
}

shmat(ap)
	int *ap;
{
	struct a {
		int	shmid;
		caddr_t	shmaddr;
		int	shmflg;
	} *uap = (struct a *)ap;
	struct proc *p = u.u_procp;
	register struct shmid_ds *shp;
	register int size;
	struct mapmem *mp;
	caddr_t uva;
	int error, prot, shmmapin();

	if (!shmvalid(uap->shmid))
		return;
	shp = &shmsegs[uap->shmid % SHMMMNI];
	if (shp->shm_handle == NULL)
		panic("shmat NULL handle");
	if (!ipcaccess(&shp->shm_perm,
		      (uap->shmflg&SHM_RDONLY) ? IPC_R : IPC_R|IPC_W))
		return;
	uva = uap->shmaddr;
	if (uva && ((int)uva & (SHMLBA-1))) {
		if (uap->shmflg & SHM_RND)
			uva = (caddr_t) ((int)uva & ~(SHMLBA-1));
		else {
			u.u_error = EINVAL;
			return;
		}
	}
	/*
	 * Make sure user doesn't use more than their fair share
	 */
	size = 0;
	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_ops == &shmops)
			size++;
	if (size >= shminfo.shmseg) {
		u.u_error = EMFILE;
		return;
	}
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
	if (error) {
		u.u_error = error;
		return;
	}
	if (u.u_error = mmmapin(p, mp, shmmapin)) {
		if (error = mmfree(p, mp))
			u.u_error = error;
		return;
	}
	/*
	 * Fill in the remaining fields
	 */
	shp->shm_lpid = p->p_pid;
	shp->shm_atime = time.tv_sec;
	shp->shm_nattch++;
	u.u_r.r_val1 = (int) uva;
}

shmdt(ap)
	int *ap;
{
	register struct a {
		caddr_t	shmaddr;
	} *uap = (struct a *)ap;
	struct proc *p = u.u_procp;
	register struct mapmem *mp;

	for (mp = u.u_mmap; mp; mp = mp->mm_next)
		if (mp->mm_ops == &shmops && mp->mm_uva == uap->shmaddr)
			break;
	if (mp == MMNIL) {
		u.u_error = EINVAL;
		return;
	}
	shmsegs[mp->mm_id % SHMMMNI].shm_lpid = p->p_pid;
	u.u_error = shmufree(p, mp);
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
	register struct mapmem *mp;
{
	struct proc *p = u.u_procp;		/* XXX */

	u.u_error = shmufree(p, mp);
}

shmvalid(id)
	register int id;
{
	register struct shmid_ds *shp;

	if (id < 0 || (id % SHMMMNI) >= shminfo.shmmni)
		return(0);
	shp = &shmsegs[id % SHMMMNI];
	if (shp->shm_perm.seq == (id / SHMMMNI) &&
	    (shp->shm_perm.mode & (SHM_ALLOC|SHM_DEST)) == SHM_ALLOC)
		return(1);
	u.u_error = EINVAL;
	return(0);
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
ipcaccess(ipc, mode)
	register struct ipc_perm *ipc;
{
	register int m;

	if (u.u_uid == 0)
		return(0);
	/*
	 * Access check is based on only one of owner, group, public.
	 * If not owner, then check group.
	 * If not a member of the group, then check public access.
	 */
	mode &= 0700;
	m = ipc->mode;
	if (u.u_uid != ipc->uid && u.u_uid != ipc->cuid) {
		m <<= 3;
		if (!groupmember(ipc->gid, u.u_cred) &&
		    !groupmember(ipc->cgid, u.u_cred))
			m <<= 3;
	}
	if ((mode&m) == mode)
		return (1);
	u.u_error = EACCES;
	return (0);
}

#endif /* SYSVSHM */

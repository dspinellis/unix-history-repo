/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: vm_mmap.c 1.6 91/10/21$
 *
 *	@(#)vm_mmap.c	7.30 (Berkeley) %G%
 */

/*
 * Mapped file (mmap) interface to VM
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/filedesc.h>
#include <sys/resourcevar.h>
#include <sys/proc.h>
#include <sys/vnode.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/conf.h>

#include <miscfs/specfs/specdev.h>

#include <vm/vm.h>
#include <vm/vm_pager.h>
#include <vm/vm_prot.h>

#ifdef DEBUG
int mmapdebug = 0;
#define MDB_FOLLOW	0x01
#define MDB_SYNC	0x02
#define MDB_MAPIT	0x04
#endif

struct sbrk_args {
	int	incr;
};
/* ARGSUSED */
int
sbrk(p, uap, retval)
	struct proc *p;
	struct sbrk_args *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

struct sstk_args {
	int	incr;
};
/* ARGSUSED */
int
sstk(p, uap, retval)
	struct proc *p;
	struct sstk_args *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

struct mmap_args {
	caddr_t	addr;
	size_t	len;
	int	prot;
	int	flags;
	int	fd;
	long	pad;
	off_t	pos;
};

#if defined(COMPAT_43) || defined(COMPAT_SUNOS)
struct getpagesize_args {
	int	dummy;
};
/* ARGSUSED */
int
ogetpagesize(p, uap, retval)
	struct proc *p;
	struct getpagesize_args *uap;
	int *retval;
{

	*retval = PAGE_SIZE;
	return (0);
}
#endif /* COMPAT_43 || COMPAT_SUNOS */

#ifdef COMPAT_43
struct osmmap_args {
	caddr_t	addr;
	int	len;
	int	prot;
	int	flags;
	int	fd;
	long	pos;
};
int
osmmap(p, uap, retval)
	struct proc *p;
	register struct osmmap_args *uap;
	int *retval;
{
	struct mmap_args nargs;
	static const char cvtbsdprot[8] = {
		0,
		PROT_EXEC,
		PROT_WRITE,
		PROT_EXEC|PROT_WRITE,
		PROT_READ,
		PROT_EXEC|PROT_READ,
		PROT_WRITE|PROT_READ,
		PROT_EXEC|PROT_WRITE|PROT_READ,
	};
#define	OMAP_ANON	0x0002
#define	OMAP_COPY	0x0020
#define	OMAP_SHARED	0x0010
#define	OMAP_FIXED	0x0100
#define	OMAP_INHERIT	0x0800

	nargs.addr = uap->addr;
	nargs.len = uap->len;
	nargs.prot = cvtbsdprot[uap->prot&0x7];
	nargs.flags = 0;
	if (uap->flags & OMAP_ANON)
		nargs.flags |= MAP_ANON;
	if (uap->flags & OMAP_COPY)
		nargs.flags |= MAP_COPY;
	if (uap->flags & OMAP_SHARED)
		nargs.flags |= MAP_SHARED;
	else
		nargs.flags |= MAP_PRIVATE;
	if (uap->flags & OMAP_FIXED)
		nargs.flags |= MAP_FIXED;
	if (uap->flags & OMAP_INHERIT)
		nargs.flags |= MAP_INHERIT;
	nargs.fd = uap->fd;
	nargs.pos = uap->pos;
	return (smmap(p, &nargs, retval));
}
#endif

int
smmap(p, uap, retval)
	struct proc *p;
	register struct mmap_args *uap;
	int *retval;
{
	register struct filedesc *fdp = p->p_fd;
	register struct file *fp;
	struct vnode *vp;
	vm_offset_t addr;
	vm_size_t size;
	vm_prot_t prot, maxprot;
	caddr_t handle;
	int flags, error;

	prot = uap->prot & VM_PROT_ALL;
	flags = uap->flags;
#ifdef DEBUG
	if (mmapdebug & MDB_FOLLOW)
		printf("mmap(%d): addr %x len %x pro %x flg %x fd %d pos %x\n",
		       p->p_pid, uap->addr, uap->len, prot,
		       flags, uap->fd, (vm_offset_t)uap->pos);
#endif
	/*
	 * Address (if FIXED) must be page aligned.
	 * Size is implicitly rounded to a page boundary.
	 */
	addr = (vm_offset_t) uap->addr;
	if (((flags & MAP_FIXED) && (addr & PAGE_MASK)) ||
	    (ssize_t)uap->len < 0 || ((flags & MAP_ANON) && uap->fd != -1))
		return (EINVAL);
	size = (vm_size_t) round_page(uap->len);
	/*
	 * Check for illegal addresses.  Watch out for address wrap...
	 * Note that VM_*_ADDRESS are not constants due to casts (argh).
	 */
	if (flags & MAP_FIXED) {
		if (VM_MAXUSER_ADDRESS > 0 && addr + size >= VM_MAXUSER_ADDRESS)
			return (EINVAL);
		if (VM_MIN_ADDRESS > 0 && addr < VM_MIN_ADDRESS)
			return (EINVAL);
		if (addr > addr + size)
			return (EINVAL);
	}
	/*
	 * XXX if no hint provided for a non-fixed mapping place it after
	 * the end of the largest possible heap.
	 *
	 * There should really be a pmap call to determine a reasonable
	 * location.
	 */
	if (addr == 0 && (flags & MAP_FIXED) == 0)
		addr = round_page(p->p_vmspace->vm_daddr + MAXDSIZ);
	if (flags & MAP_ANON) {
		/*
		 * Mapping blank space is trivial.
		 */
		handle = NULL;
		maxprot = VM_PROT_ALL;
	} else {
		/*
		 * Mapping file, get fp for validation.
		 * Obtain vnode and make sure it is of appropriate type.
		 */
		if (((unsigned)uap->fd) >= fdp->fd_nfiles ||
		    (fp = fdp->fd_ofiles[uap->fd]) == NULL)
			return (EBADF);
		if (fp->f_type != DTYPE_VNODE)
			return (EINVAL);
		vp = (struct vnode *)fp->f_data;
		if (vp->v_type != VREG && vp->v_type != VCHR)
			return (EINVAL);
		/*
		 * Ensure that file and memory protections are compatible.
		 * Note that we only worry about writability if mapping is
		 * shared; in this case, current and max prot are dictated
		 * by the open file.
		 * XXX use the vnode instead?  Problem is: what credentials
		 * do we use for determination?  What if proc does a setuid?
		 */
		maxprot = VM_PROT_EXECUTE;	/* ??? */
		if (fp->f_flag & FREAD)
			maxprot |= VM_PROT_READ;
		else if (prot & PROT_READ)
			return (EACCES);
		if (flags & MAP_SHARED) {
			if (fp->f_flag & FWRITE)
				maxprot |= VM_PROT_WRITE;
			else if (prot & PROT_WRITE)
				return (EACCES);
		} else
			maxprot |= VM_PROT_WRITE;
		handle = (caddr_t)vp;
	}
	error = vm_mmap(&p->p_vmspace->vm_map, &addr, size, prot, maxprot,
	    flags, handle, (vm_offset_t)uap->pos);
	if (error == 0)
		*retval = (int)addr;
	return (error);
}

struct msync_args {
	caddr_t	addr;
	int	len;
};
int
msync(p, uap, retval)
	struct proc *p;
	struct msync_args *uap;
	int *retval;
{
	vm_offset_t addr, objoff, oaddr;
	vm_size_t size, osize;
	vm_prot_t prot, mprot;
	vm_inherit_t inherit;
	vm_object_t object;
	boolean_t shared;
	int rv;

#ifdef DEBUG
	if (mmapdebug & (MDB_FOLLOW|MDB_SYNC))
		printf("msync(%d): addr %x len %x\n",
		       p->p_pid, uap->addr, uap->len);
#endif
	if (((int)uap->addr & PAGE_MASK) || uap->len < 0)
		return(EINVAL);
	addr = oaddr = (vm_offset_t)uap->addr;
	osize = (vm_size_t)uap->len;
	/*
	 * Region must be entirely contained in a single entry
	 */
	if (!vm_map_is_allocated(&p->p_vmspace->vm_map, addr, addr+osize,
	    TRUE))
		return(EINVAL);
	/*
	 * Determine the object associated with that entry
	 * (object is returned locked on KERN_SUCCESS)
	 */
	rv = vm_region(&p->p_vmspace->vm_map, &addr, &size, &prot, &mprot,
		       &inherit, &shared, &object, &objoff);
	if (rv != KERN_SUCCESS)
		return(EINVAL);
#ifdef DEBUG
	if (mmapdebug & MDB_SYNC)
		printf("msync: region: object %x addr %x size %d objoff %d\n",
		       object, addr, size, objoff);
#endif
	/*
	 * Do not msync non-vnoded backed objects.
	 */
	if ((object->flags & OBJ_INTERNAL) || object->pager == NULL ||
	    object->pager->pg_type != PG_VNODE) {
		vm_object_unlock(object);
		return(EINVAL);
	}
	objoff += oaddr - addr;
	if (osize == 0)
		osize = size;
#ifdef DEBUG
	if (mmapdebug & MDB_SYNC)
		printf("msync: cleaning/flushing object range [%x-%x)\n",
		       objoff, objoff+osize);
#endif
	if (prot & VM_PROT_WRITE)
		vm_object_page_clean(object, objoff, objoff+osize, FALSE);
	/*
	 * (XXX)
	 * Bummer, gotta flush all cached pages to ensure
	 * consistency with the file system cache.
	 */
	vm_object_page_remove(object, objoff, objoff+osize);
	vm_object_unlock(object);
	return(0);
}

struct munmap_args {
	caddr_t	addr;
	int	len;
};
int
munmap(p, uap, retval)
	register struct proc *p;
	register struct munmap_args *uap;
	int *retval;
{
	vm_offset_t addr;
	vm_size_t size;

#ifdef DEBUG
	if (mmapdebug & MDB_FOLLOW)
		printf("munmap(%d): addr %x len %x\n",
		       p->p_pid, uap->addr, uap->len);
#endif

	addr = (vm_offset_t) uap->addr;
	if ((addr & PAGE_MASK) || uap->len < 0)
		return(EINVAL);
	size = (vm_size_t) round_page(uap->len);
	if (size == 0)
		return(0);
	/*
	 * Check for illegal addresses.  Watch out for address wrap...
	 * Note that VM_*_ADDRESS are not constants due to casts (argh).
	 */
	if (VM_MAXUSER_ADDRESS > 0 && addr + size >= VM_MAXUSER_ADDRESS)
		return (EINVAL);
	if (VM_MIN_ADDRESS > 0 && addr < VM_MIN_ADDRESS)
		return (EINVAL);
	if (addr > addr + size)
		return (EINVAL);
	if (!vm_map_is_allocated(&p->p_vmspace->vm_map, addr, addr + size,
	    FALSE))
		return(EINVAL);
	/* returns nothing but KERN_SUCCESS anyway */
	(void) vm_map_remove(&p->p_vmspace->vm_map, addr, addr+size);
	return(0);
}

void
munmapfd(fd)
	int fd;
{
#ifdef DEBUG
	if (mmapdebug & MDB_FOLLOW)
		printf("munmapfd(%d): fd %d\n", curproc->p_pid, fd);
#endif

	/*
	 * XXX -- should vm_deallocate any regions mapped to this file
	 */
	curproc->p_fd->fd_ofileflags[fd] &= ~UF_MAPPED;
}

struct mprotect_args {
	caddr_t	addr;
	int	len;
	int	prot;
};
int
mprotect(p, uap, retval)
	struct proc *p;
	struct mprotect_args *uap;
	int *retval;
{
	vm_offset_t addr;
	vm_size_t size;
	register vm_prot_t prot;

#ifdef DEBUG
	if (mmapdebug & MDB_FOLLOW)
		printf("mprotect(%d): addr %x len %x prot %d\n",
		       p->p_pid, uap->addr, uap->len, uap->prot);
#endif

	addr = (vm_offset_t)uap->addr;
	if ((addr & PAGE_MASK) || uap->len < 0)
		return(EINVAL);
	size = (vm_size_t)uap->len;
	prot = uap->prot & VM_PROT_ALL;

	switch (vm_map_protect(&p->p_vmspace->vm_map, addr, addr+size, prot,
	    FALSE)) {
	case KERN_SUCCESS:
		return (0);
	case KERN_PROTECTION_FAILURE:
		return (EACCES);
	}
	return (EINVAL);
}

struct madvise_args {
	caddr_t	addr;
	int	len;
	int	behav;
};
/* ARGSUSED */
int
madvise(p, uap, retval)
	struct proc *p;
	struct madvise_args *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

struct mincore_args {
	caddr_t	addr;
	int	len;
	char	*vec;
};
/* ARGSUSED */
int
mincore(p, uap, retval)
	struct proc *p;
	struct mincore_args *uap;
	int *retval;
{

	/* Not yet implemented */
	return (EOPNOTSUPP);
}

struct mlock_args {
	caddr_t	addr;
	size_t	len;
};
int
mlock(p, uap, retval)
	struct proc *p;
	struct mlock_args *uap;
	int *retval;
{
	vm_offset_t addr;
	vm_size_t size;
	int error;
	extern int vm_page_max_wired;

#ifdef DEBUG
	if (mmapdebug & MDB_FOLLOW)
		printf("mlock(%d): addr %x len %x\n",
		       p->p_pid, uap->addr, uap->len);
#endif
	addr = (vm_offset_t)uap->addr;
	if ((addr & PAGE_MASK) || uap->len < 0)
		return (EINVAL);
	size = round_page((vm_size_t)uap->len);
	if (atop(size) + cnt.v_wire_count > vm_page_max_wired)
		return (EAGAIN);
#ifdef pmap_wired_count
	if (size + ptoa(pmap_wired_count(vm_map_pmap(&p->p_vmspace->vm_map))) >
	    p->p_rlimit[RLIMIT_MEMLOCK].rlim_cur)
		return (EAGAIN);
#else
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
#endif

	error = vm_map_pageable(&p->p_vmspace->vm_map, addr, addr+size, FALSE);
	return (error == KERN_SUCCESS ? 0 : ENOMEM);
}

struct munlock_args {
	caddr_t	addr;
	size_t	len;
};
int
munlock(p, uap, retval)
	struct proc *p;
	struct munlock_args *uap;
	int *retval;
{
	vm_offset_t addr;
	vm_size_t size;
	int error;

#ifdef DEBUG
	if (mmapdebug & MDB_FOLLOW)
		printf("munlock(%d): addr %x len %x\n",
		       p->p_pid, uap->addr, uap->len);
#endif
	addr = (vm_offset_t)uap->addr;
	if ((addr & PAGE_MASK) || uap->len < 0)
		return (EINVAL);
#ifndef pmap_wired_count
	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
#endif
	size = round_page((vm_size_t)uap->len);

	error = vm_map_pageable(&p->p_vmspace->vm_map, addr, addr+size, TRUE);
	return (error == KERN_SUCCESS ? 0 : ENOMEM);
}

/*
 * Internal version of mmap.
 * Currently used by mmap, exec, and sys5 shared memory.
 * Handle is either a vnode pointer or NULL for MAP_ANON.
 */
int
vm_mmap(map, addr, size, prot, maxprot, flags, handle, foff)
	register vm_map_t map;
	register vm_offset_t *addr;
	register vm_size_t size;
	vm_prot_t prot, maxprot;
	register int flags;
	caddr_t handle;		/* XXX should be vp */
	vm_offset_t foff;
{
	register vm_pager_t pager;
	boolean_t fitit;
	vm_object_t object;
	struct vnode *vp;
	int type;
	int rv = KERN_SUCCESS;

	if (size == 0)
		return (0);

	if ((flags & MAP_FIXED) == 0) {
		fitit = TRUE;
		*addr = round_page(*addr);
	} else {
		fitit = FALSE;
		(void)vm_deallocate(map, *addr, size);
	}

	/*
	 * Lookup/allocate pager.  All except an unnamed anonymous lookup
	 * gain a reference to ensure continued existance of the object.
	 * (XXX the exception is to appease the pageout daemon)
	 */
	if (flags & MAP_ANON)
		type = PG_DFLT;
	else {
		vp = (struct vnode *)handle;
		if (vp->v_type == VCHR) {
			type = PG_DEVICE;
			handle = (caddr_t)vp->v_rdev;
		} else
			type = PG_VNODE;
	}
	pager = vm_pager_allocate(type, handle, size, prot);
	if (pager == NULL)
		return (type == PG_DEVICE ? EINVAL : ENOMEM);
	/*
	 * Find object and release extra reference gained by lookup
	 */
	object = vm_object_lookup(pager);
	vm_object_deallocate(object);

	/*
	 * Anonymous memory.
	 */
	if (flags & MAP_ANON) {
		rv = vm_allocate_with_pager(map, addr, size, fitit,
					    pager, foff, TRUE);
		if (rv != KERN_SUCCESS) {
			if (handle == NULL)
				vm_pager_deallocate(pager);
			else
				vm_object_deallocate(object);
			goto out;
		}
		/*
		 * Don't cache anonymous objects.
		 * Loses the reference gained by vm_pager_allocate.
		 * Note that object will be NULL when handle == NULL,
		 * this is ok since vm_allocate_with_pager has made
		 * sure that these objects are uncached.
		 */
		(void) pager_cache(object, FALSE);
#ifdef DEBUG
		if (mmapdebug & MDB_MAPIT)
			printf("vm_mmap(%d): ANON *addr %x size %x pager %x\n",
			       curproc->p_pid, *addr, size, pager);
#endif
	}
	/*
	 * Must be a mapped file.
	 * Distinguish between character special and regular files.
	 */
	else if (vp->v_type == VCHR) {
		rv = vm_allocate_with_pager(map, addr, size, fitit,
					    pager, foff, FALSE);
		/*
		 * Uncache the object and lose the reference gained
		 * by vm_pager_allocate().  If the call to
		 * vm_allocate_with_pager() was sucessful, then we
		 * gained an additional reference ensuring the object
		 * will continue to exist.  If the call failed then
		 * the deallocate call below will terminate the
		 * object which is fine.
		 */
		(void) pager_cache(object, FALSE);
		if (rv != KERN_SUCCESS)
			goto out;
	}
	/*
	 * A regular file
	 */
	else {
#ifdef DEBUG
		if (object == NULL)
			printf("vm_mmap: no object: vp %x, pager %x\n",
			       vp, pager);
#endif
		/*
		 * Map it directly.
		 * Allows modifications to go out to the vnode.
		 */
		if (flags & MAP_SHARED) {
			rv = vm_allocate_with_pager(map, addr, size,
						    fitit, pager,
						    foff, FALSE);
			if (rv != KERN_SUCCESS) {
				vm_object_deallocate(object);
				goto out;
			}
			/*
			 * Don't cache the object.  This is the easiest way
			 * of ensuring that data gets back to the filesystem
			 * because vnode_pager_deallocate() will fsync the
			 * vnode.  pager_cache() will lose the extra ref.
			 */
			if (prot & VM_PROT_WRITE)
				pager_cache(object, FALSE);
			else
				vm_object_deallocate(object);
		}
		/*
		 * Copy-on-write of file.  Two flavors.
		 * MAP_COPY is true COW, you essentially get a snapshot of
		 * the region at the time of mapping.  MAP_PRIVATE means only
		 * that your changes are not reflected back to the object.
		 * Changes made by others will be seen.
		 */
		else {
			vm_map_t tmap;
			vm_offset_t off;

			/* locate and allocate the target address space */
			rv = vm_map_find(map, NULL, (vm_offset_t)0,
					 addr, size, fitit);
			if (rv != KERN_SUCCESS) {
				vm_object_deallocate(object);
				goto out;
			}
			tmap = vm_map_create(pmap_create(size), VM_MIN_ADDRESS,
					     VM_MIN_ADDRESS+size, TRUE);
			off = VM_MIN_ADDRESS;
			rv = vm_allocate_with_pager(tmap, &off, size,
						    TRUE, pager,
						    foff, FALSE);
			if (rv != KERN_SUCCESS) {
				vm_object_deallocate(object);
				vm_map_deallocate(tmap);
				goto out;
			}
			/*
			 * (XXX)
			 * MAP_PRIVATE implies that we see changes made by
			 * others.  To ensure that we need to guarentee that
			 * no copy object is created (otherwise original
			 * pages would be pushed to the copy object and we
			 * would never see changes made by others).  We
			 * totally sleeze it right now by marking the object
			 * internal temporarily.
			 */
			if ((flags & MAP_COPY) == 0)
				object->flags |= OBJ_INTERNAL;
			rv = vm_map_copy(map, tmap, *addr, size, off,
					 FALSE, FALSE);
			object->flags &= ~OBJ_INTERNAL;
			/*
			 * (XXX)
			 * My oh my, this only gets worse...
			 * Force creation of a shadow object so that
			 * vm_map_fork will do the right thing.
			 */
			if ((flags & MAP_COPY) == 0) {
				vm_map_t tmap;
				vm_map_entry_t tentry;
				vm_object_t tobject;
				vm_offset_t toffset;
				vm_prot_t tprot;
				boolean_t twired, tsu;

				tmap = map;
				vm_map_lookup(&tmap, *addr, VM_PROT_WRITE,
					      &tentry, &tobject, &toffset,
					      &tprot, &twired, &tsu);
				vm_map_lookup_done(tmap, tentry);
			}
			/*
			 * (XXX)
			 * Map copy code cannot detect sharing unless a
			 * sharing map is involved.  So we cheat and write
			 * protect everything ourselves.
			 */
			vm_object_pmap_copy(object, foff, foff + size);
			vm_object_deallocate(object);
			vm_map_deallocate(tmap);
			if (rv != KERN_SUCCESS)
				goto out;
		}
#ifdef DEBUG
		if (mmapdebug & MDB_MAPIT)
			printf("vm_mmap(%d): FILE *addr %x size %x pager %x\n",
			       curproc->p_pid, *addr, size, pager);
#endif
	}
	/*
	 * Correct protection (default is VM_PROT_ALL).
	 * If maxprot is different than prot, we must set both explicitly.
	 */
	rv = KERN_SUCCESS;
	if (maxprot != VM_PROT_ALL)
		rv = vm_map_protect(map, *addr, *addr+size, maxprot, TRUE);
	if (rv == KERN_SUCCESS && prot != maxprot)
		rv = vm_map_protect(map, *addr, *addr+size, prot, FALSE);
	if (rv != KERN_SUCCESS) {
		(void) vm_deallocate(map, *addr, size);
		goto out;
	}
	/*
	 * Shared memory is also shared with children.
	 */
	if (flags & MAP_SHARED) {
		rv = vm_inherit(map, *addr, size, VM_INHERIT_SHARE);
		if (rv != KERN_SUCCESS) {
			(void) vm_deallocate(map, *addr, size);
			goto out;
		}
	}
out:
#ifdef DEBUG
	if (mmapdebug & MDB_MAPIT)
		printf("vm_mmap: rv %d\n", rv);
#endif
	switch (rv) {
	case KERN_SUCCESS:
		return (0);
	case KERN_INVALID_ADDRESS:
	case KERN_NO_SPACE:
		return (ENOMEM);
	case KERN_PROTECTION_FAILURE:
		return (EACCES);
	default:
		return (EINVAL);
	}
}

/*
 * Internal bastardized version of MACHs vm_region system call.
 * Given address and size it returns map attributes as well
 * as the (locked) object mapped at that location. 
 */
int
vm_region(map, addr, size, prot, max_prot, inheritance, shared, object, objoff)
	vm_map_t	map;
	vm_offset_t	*addr;		/* IN/OUT */
	vm_size_t	*size;		/* OUT */
	vm_prot_t	*prot;		/* OUT */
	vm_prot_t	*max_prot;	/* OUT */
	vm_inherit_t	*inheritance;	/* OUT */
	boolean_t	*shared;	/* OUT */
	vm_object_t	*object;	/* OUT */
	vm_offset_t	*objoff;	/* OUT */
{
	vm_map_entry_t	tmp_entry;
	register
	vm_map_entry_t	entry;
	register
	vm_offset_t	tmp_offset;
	vm_offset_t	start;

	if (map == NULL)
		return(KERN_INVALID_ARGUMENT);
	
	start = *addr;

	vm_map_lock_read(map);
	if (!vm_map_lookup_entry(map, start, &tmp_entry)) {
		if ((entry = tmp_entry->next) == &map->header) {
			vm_map_unlock_read(map);
		   	return(KERN_NO_SPACE);
		}
		start = entry->start;
		*addr = start;
	} else
		entry = tmp_entry;

	*prot = entry->protection;
	*max_prot = entry->max_protection;
	*inheritance = entry->inheritance;

	tmp_offset = entry->offset + (start - entry->start);
	*size = (entry->end - start);

	if (entry->is_a_map) {
		register vm_map_t share_map;
		vm_size_t share_size;

		share_map = entry->object.share_map;

		vm_map_lock_read(share_map);
		(void) vm_map_lookup_entry(share_map, tmp_offset, &tmp_entry);

		if ((share_size = (tmp_entry->end - tmp_offset)) < *size)
			*size = share_size;

		vm_object_lock(tmp_entry->object);
		*object = tmp_entry->object.vm_object;
		*objoff = tmp_entry->offset + (tmp_offset - tmp_entry->start);

		*shared = (share_map->ref_count != 1);
		vm_map_unlock_read(share_map);
	} else {
		vm_object_lock(entry->object);
		*object = entry->object.vm_object;
		*objoff = tmp_offset;

		*shared = FALSE;
	}

	vm_map_unlock_read(map);

	return(KERN_SUCCESS);
}

/*
 * Yet another bastard routine.
 */
int
vm_allocate_with_pager(map, addr, size, fitit, pager, poffset, internal)
	register vm_map_t	map;
	register vm_offset_t	*addr;
	register vm_size_t	size;
	boolean_t		fitit;
	vm_pager_t		pager;
	vm_offset_t		poffset;
	boolean_t		internal;
{
	register vm_object_t	object;
	register int		result;

	if (map == NULL)
		return(KERN_INVALID_ARGUMENT);

	*addr = trunc_page(*addr);
	size = round_page(size);

	/*
	 *	Lookup the pager/paging-space in the object cache.
	 *	If it's not there, then create a new object and cache
	 *	it.
	 */
	object = vm_object_lookup(pager);
	cnt.v_lookups++;
	if (object == NULL) {
		object = vm_object_allocate(size);
		/*
		 * From Mike Hibler: "unnamed anonymous objects should never
		 * be on the hash list ... For now you can just change
		 * vm_allocate_with_pager to not do vm_object_enter if this
		 * is an internal object ..."
		 */
		if (!internal)
			vm_object_enter(object, pager);
	} else
		cnt.v_hits++;
	if (internal)
		object->flags |= OBJ_INTERNAL;
	else
		object->flags &= ~OBJ_INTERNAL;

	result = vm_map_find(map, object, poffset, addr, size, fitit);
	if (result != KERN_SUCCESS)
		vm_object_deallocate(object);
	else if (pager != NULL)
		vm_object_setpager(object, pager, (vm_offset_t) 0, TRUE);
	return(result);
}

/*
 * XXX: this routine belongs in vm_map.c.
 *
 * Returns TRUE if the range [start - end) is allocated in either
 * a single entry (single_entry == TRUE) or multiple contiguous
 * entries (single_entry == FALSE).
 *
 * start and end should be page aligned.
 */
boolean_t
vm_map_is_allocated(map, start, end, single_entry)
	vm_map_t map;
	vm_offset_t start, end;
	boolean_t single_entry;
{
	vm_map_entry_t mapent;
	register vm_offset_t nend;

	vm_map_lock_read(map);

	/*
	 * Start address not in any entry
	 */
	if (!vm_map_lookup_entry(map, start, &mapent)) {
		vm_map_unlock_read(map);
		return (FALSE);
	}
	/*
	 * Find the maximum stretch of contiguously allocated space
	 */
	nend = mapent->end;
	if (!single_entry) {
		mapent = mapent->next;
		while (mapent != &map->header && mapent->start == nend) {
			nend = mapent->end;
			mapent = mapent->next;
		}
	}

	vm_map_unlock_read(map);
	return (end <= nend);
}

/*
 * Copyright (c) 1985, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)namei.h	7.4 (Berkeley) %G%
 */

#ifndef _NAMEI_
#define	_NAMEI_

#ifdef KERNEL
#include "../ufs/dir.h"
#include "uio.h"
#else
#include <sys/uio.h>
#include <ufs/dir.h>
#endif

/*
 * Encapsulation of namei parameters.
 * One of these is located in the u. area to
 * minimize space allocated on the kernel stack.
 */
struct nameidata {
		/* arguments to namei and related context: */
	caddr_t	ni_dirp;		/* pathname pointer */
	enum	uio_seg ni_segflg;	/* location of pathname */
	short	ni_nameiop;		/* see below */
	struct	vnode *ni_cdir;		/* current directory */
	struct	vnode *ni_rdir;		/* root directory, if not normal root */
	struct	ucred *ni_cred;		/* credentials */

		/* shared between namei, lookup routines and commit routines: */
	caddr_t	ni_pnbuf;		/* pathname buffer */
	char	*ni_ptr;		/* current location in pathname */
	char	*ni_next;		/* next location in pathname */
	u_int	ni_pathlen;		/* remaining chars in path */
	short	ni_namelen;		/* length of current component */
	short	ni_loopcnt;		/* count of symlinks encountered */
	char	ni_makeentry;		/* 1 => add entry to name cache */
	char	ni_isdotdot;		/* 1 => current component name is .. */

		/* results: */
	struct	vnode *ni_vp;		/* vnode of result */
	struct	vnode *ni_dvp;		/* vnode of intermediate directory */

		/* side effects: */
	struct	direct ni_dent;		/* current directory entry */

	/* BEGIN UFS SPECIFIC */
	off_t	ni_endoff;		/* end of useful directory contents */
	struct ndirinfo {		/* saved info for new dir entry */
		struct	iovec nd_iovec;		/* pointed to by ni_iov */
		struct	uio nd_uio;		/* directory I/O parameters */
		u_long	nd_hash;		/* hash value of nd_dent */
	} ni_nd;
	/* END UFS SPECIFIC */
};

#define	ni_base		ni_nd.nd_iovec.iov_base
#define	ni_count	ni_nd.nd_iovec.iov_len
#define	ni_segflg	ni_nd.nd_uio.uio_segflg
#define	ni_iov		ni_nd.nd_uio.uio_iov
#define	ni_iovcnt	ni_nd.nd_uio.uio_iovcnt
#define	ni_offset	ni_nd.nd_uio.uio_offset
#define	ni_resid	ni_nd.nd_uio.uio_resid
#define ni_hash		ni_nd.nd_hash

#ifdef KERNEL
/*
 * namei operations and modifiers
 */
#define	LOOKUP		0	/* perform name lookup only */
#define	CREATE		1	/* setup for file creation */
#define	DELETE		2	/* setup for file deletion */
#define	RENAME		3	/* setup for file renaming */
#define	OPFLAG		3	/* mask for operation */
#define	LOCKLEAF	0x04	/* lock inode on return */
#define	LOCKPARENT	0x08	/* want parent vnode returned locked */
#define	WANTPARENT	0x10	/* want parent vnode returned unlocked */
#define NOCACHE		0x20	/* name must not be left in cache */
#define FOLLOW		0x40	/* follow symbolic links */
#define	NOFOLLOW	0x0	/* don't follow symbolic links (pseudo) */
#define	NOMOUNT		0x80	/* don't cross mount points */
#endif

/*
 * This structure describes the elements in the cache of recent
 * names looked up by namei.
 */

#define	NCHNAMLEN	15	/* maximum name segment length we bother with */

struct	namecache {
	struct	namecache *nc_forw;	/* hash chain, MUST BE FIRST */
	struct	namecache *nc_back;	/* hash chain, MUST BE FIRST */
	struct	namecache *nc_nxt;	/* LRU chain */
	struct	namecache **nc_prev;	/* LRU chain */
	struct	vnode *nc_vp;		/* vnode the name refers to */
	struct	vnode *nc_dp;		/* vnode of parent of name */
	char	nc_nlen;		/* length of name */
	char	nc_name[NCHNAMLEN];	/* segment name */
	struct	ucred *nc_cred;		/* ??? credentials */
};

#define ANYCRED ((struct ucred *) -1)
#define NOCRED  ((struct ucred *) 0)

#ifdef KERNEL
struct	namecache *namecache;
int	nchsize;
#endif

/*
 * Stats on usefulness of namei caches.
 */
struct	nchstats {
	long	ncs_goodhits;		/* hits that we can reall use */
	long	ncs_badhits;		/* hits we must drop */
	long	ncs_falsehits;		/* hits with id mismatch */
	long	ncs_miss;		/* misses */
	long	ncs_long;		/* long names that ignore cache */
	long	ncs_pass2;		/* names found with passes == 2 */
	long	ncs_2passes;		/* number of times we attempt it */
};
#endif

/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)namei.h	6.11 (Berkeley) 2/20/86
 */

#ifndef _NAMEI_
#define	_NAMEI_

#ifdef KERNEL
#include "uio.h"
#else
#include <sys/uio.h>
#endif

/*
 * Encapsulation of namei parameters.
 * One of these is located in the u. area to
 * minimize space allocated on the kernel stack.
 */
struct nameidata {
		/* arguments to namei and related context: */
	caddr_t	ni_dirp;		/* pathname pointer */
	enum	uio_seg ni_seg;		/* location of pathname */
	short	ni_nameiop;		/* see below */
	struct	vnode *ni_cdir;		/* current directory */
	struct	vnode *ni_rdir;		/* root directory, if not normal root */
	struct	ucred *ni_cred;		/* credentials */

		/* shared between namei, lookup routines and commit routines: */
	caddr_t	ni_pnbuf;		/* pathname buffer (could be local) */
	char	*ni_ptr;		/* current location in pathname */
	int	ni_pathlen;		/* remaining chars in path */
	short	ni_more;		/* more left to translate in pathname */
	short	ni_loopcnt;		/* count of symlinks encountered */

		/* results: */
	struct	vnode *ni_vp;		/* vnode of result */
	struct	vnode *ni_dvp;		/* vnode of intermediate directory */

		/* side effects: */
/* BEGIN UFS SPECIFIC */
	off_t	ni_endoff;		/* end of useful stuff in directory */
	struct diroffcache {		/* last successful directory search */
		struct	vnode *nc_prevdir;	/* terminal directory */
		long	nc_id;			/* directory's unique id */
		struct	timeval nc_time;	/* modification time */
		off_t	nc_prevoffset;		/* where last entry found */
	} ni_nc;
	struct ndirinfo {		/* saved info for new dir entry */
		struct	iovec nd_iovec;		/* pointed to by ni_iov */
		struct	uio nd_uio;		/* directory I/O parameters */
		struct	direct nd_dent;		/* current directory entry */
	} ni_nd;
/* END UFS SPECIFIC */
};

#define	ni_prevdir	ni_nc.nc_prevdir
#define	ni_id		ni_nc.nc_id
#define	ni_time		ni_nc.nc_time
#define	ni_prevoffset	ni_nc.nc_prevoffset

#define	ni_base		ni_nd.nd_iovec.iov_base
#define	ni_count	ni_nd.nd_iovec.iov_len
#define	ni_segflg	ni_nd.nd_uio.uio_segflg
#define	ni_iov		ni_nd.nd_uio.uio_iov
#define	ni_iovcnt	ni_nd.nd_uio.uio_iovcnt
#define	ni_offset	ni_nd.nd_uio.uio_offset
#define	ni_resid	ni_nd.nd_uio.uio_resid

/*
 * namei operations and modifiers
 */
#define	LOOKUP		0	/* perform name lookup only */
#define	CREATE		1	/* setup for file creation */
#define	DELETE		2	/* setup for file deletion */
#define	LOCKPARENT	0x10	/* see the top of namei */
#define NOCACHE		0x20	/* name must not be left in cache */
#define FOLLOW		0x40	/* follow symbolic links */
#define	NOFOLLOW	0x0	/* don't follow symbolic links (pseudo) */

/*
 * This structure describes the elements in the cache of recent
 * names looked up by namei.
 */
struct	namecache {
	struct	namecache *nc_forw;	/* hash chain, MUST BE FIRST */
	struct	namecache *nc_back;	/* hash chain, MUST BE FIRST */
	struct	namecache *nc_nxt;	/* LRU chain */
	struct	namecache **nc_prev;	/* LRU chain */
	struct	vnode *nc_vp;		/* vnode the name refers to */
	fileid	nc_fileid;		/* fileid of parent of name */
	dev_t	nc_dev;			/* dev of parent of name */
	dev_t	nc_idev;		/* dev of the name ref'd */
	long	nc_id;			/* referenced vnode's id */
	char	nc_nlen;		/* length of name */
#define	NCHNAMLEN	15	/* maximum name segment length we bother with */
	char	nc_name[NCHNAMLEN];	/* segment name */
};
struct	namecache *namecache;
int	nchsize;

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

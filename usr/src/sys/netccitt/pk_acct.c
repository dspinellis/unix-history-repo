/*
 * Copyright (c) University of British Columbia, 1984
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Laboratory for Computation Vision and the Computer Science Department
 * of the University of British Columbia.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pk_acct.c	7.2 (Berkeley) %G%
 */

#include "../h/param.h"
#ifdef NFS
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "vnode.h"
#include "vfs.h"
#include "kernel.h"
#include "uio.h"
#else
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/kernel.h"
#ifdef BSD4_3
#include "../h/namei.h"
#else
#include "../h/nami.h"
#endif
#include "../h/uio.h"
#endif

#include "../netccitt/pk.h"
#include "../netccitt/pk_var.h"
#include "../netccitt/x25.h"
#include "../netccitt/x25acct.h"


#ifdef NFS
struct	vnode *pkacctp;
#else
struct	inode *pkacctp;
#endif

/* 
 *  Turn on packet accounting
 */

pk_accton (path)
	char *path;
{
#ifdef NFS
	register int error;
	struct vnode *vp;
#else
#ifdef BSD4_3
	struct nameidata *ndp = &u.u_nd;
#endif
	register struct inode *ip;
#endif

#ifdef NFS
	if (error = lookupname(path, UIO_USERSPACE, FOLLOW_LINK,
		(struct vnode **)0, &vp))
		return (error);
	if (vp->v_type != VREG) {
		VN_RELE(vp);
		return (EACCES);
	}
	if (vp->v_vfsp->vfs_flag & VFS_RDONLY) {
		VN_RELE(vp);
		return (EROFS);
	}
	pkacctp = vp;
#else
#ifdef BSD4_3
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = path;
	ip = namei(ndp);
#else
	u.u_dirp = path;
	ip = namei (schar, LOOKUP, 1);
#endif
	if (ip == NULL)
		return (u.u_error);

	if ((ip -> i_mode & IFMT) != IFREG) {
		iput (ip);
		return (EACCES);
	}
	if (pkacctp)
		if (pkacctp->i_number != ip->i_number ||
		    pkacctp->i_dev != ip->i_dev)
			irele(pkacctp);
	pkacctp = ip;
	iunlock (ip);
#endif
	return (0);
}

/* 
 *  Turn off packet accounting
 */

pk_acctoff ()
{
	if (pkacctp) {
#ifdef NFS
		VN_RELE(pkacctp);
#else
		irele (pkacctp);
#endif
		pkacctp = 0;
	}
}

/* 
 *  Write a record on the accounting file.
 */

pk_acct (lcp)
register struct pklcd *lcp;
{
#ifdef NFS
	register struct vnode *vp;
#else
	register struct inode *ip;
	off_t siz;
#endif
	register struct sockaddr_x25 *sa;
	register char *src, *dst;
	register int len;
#ifndef WATERLOO
	register long etime;
#endif
	static struct x25acct acbuf;

#ifdef NFS
	if ((vp = pkacctp) == 0)
#else
	if ((ip = pkacctp) == 0)
#endif
		return;

	bzero ((caddr_t)&acbuf, sizeof (acbuf));
	if (lcp -> lcd_ceaddr != 0)
		sa = lcp -> lcd_ceaddr;
	else if (lcp -> lcd_craddr != 0) {
		sa = lcp -> lcd_craddr;
		acbuf.x25acct_callin = 1;
	} else
		return;

	if (sa -> x25_opts.op_flags & X25_REVERSE_CHARGE)
		acbuf.x25acct_revcharge = 1;
	acbuf.x25acct_stime = lcp -> lcd_stime;
#ifdef WATERLOO
	acbuf.x25acct_etime = time.tv_sec - acbuf.x25acct_stime;
#else
	etime = time.tv_sec - acbuf.x25acct_stime;
	acbuf.x25acct_etime = etime > 0xffff ? 0xffff : etime;
#endif
	acbuf.x25acct_uid = u.u_uid;
	acbuf.x25acct_psize = sa -> x25_opts.op_psize;
	acbuf.x25acct_net = sa -> x25_net;
	/*
	 * Convert address to bcd
	 */
	src = sa -> x25_addr;
	dst = acbuf.x25acct_addr;
	for (len = 0; *src; len++)
		if (len & 01)
			*dst++ |= *src++ & 0xf;
		else
			*dst = *src++ << 4;
	acbuf.x25acct_addrlen = len;

	bcopy (sa -> x25_udata, acbuf.x25acct_udata,
		sizeof (acbuf.x25acct_udata));
	acbuf.x25acct_txcnt = lcp -> lcd_txcnt;
	acbuf.x25acct_rxcnt = lcp -> lcd_rxcnt;

#ifdef NFS
	(void) vn_rdwr(UIO_WRITE, vp, (caddr_t)&acbuf, sizeof (acbuf),
		(off_t)0, UIO_SYSSPACE, IO_UNIT|IO_APPEND, (int *)0);
#else
	ilock (ip);
	siz = ip -> i_size;
	if (rdwri (UIO_WRITE, ip, (caddr_t)&acbuf, sizeof (acbuf),
	    siz, 1, (int *) 0))
		itrunc (ip, (u_long) siz);
	iunlock (ip);
#endif
}

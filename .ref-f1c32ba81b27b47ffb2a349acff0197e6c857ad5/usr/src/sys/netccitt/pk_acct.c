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
 *	@(#)pk_acct.c	7.5 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "proc.h"
#include "user.h"
#include "vnode.h"
#include "kernel.h"
#include "file.h"
#include "acct.h"
#include "uio.h"
#include "socket.h"
#include "socketvar.h"

#include "../net/if.h"

#include "x25.h"
#include "pk.h"
#include "pk_var.h"
#include "x25acct.h"


struct	vnode *pkacctp;
/* 
 *  Turn on packet accounting
 */

pk_accton (path)
	char *path;
{
	register struct vnode *vp = NULL;
	register struct nameidata *ndp = &u.u_nd;
	struct vnode *oacctp = pkacctp;
	int error;

	ndp -> ni_segflg = UIO_USERSPACE;
	ndp -> ni_dirp = path;
	if (error = vn_open (ndp, FREAD|FWRITE, 0))
		return (error);
	vp = ndp -> ni_vp;
	if (vp -> v_type != VREG) {
		vrele (vp);
		return (EACCES);
	}
	pkacctp = vp;
	if (oacctp)
		vrele (oacctp);
	return (0);
}

/* 
 *  Turn off packet accounting
 */

pk_acctoff ()
{
	if (pkacctp) {
		vrele (pkacctp);
		pkacctp = 0;
	}
}

/* 
 *  Write a record on the accounting file.
 */

pk_acct (lcp)
register struct pklcd *lcp;
{
	register struct vnode *vp;
	register struct sockaddr_x25 *sa;
	register char *src, *dst;
	register int len;
	register long etime;
	static struct x25acct acbuf;

	if ((vp = pkacctp) == 0)
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
	acbuf.x25acct_etime = time.tv_sec - acbuf.x25acct_stime;
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

	(void) vn_rdwr(UIO_WRITE, vp, (caddr_t)&acbuf, sizeof (acbuf),
		(off_t)0, UIO_SYSSPACE, IO_UNIT|IO_APPEND, u.u_cred, (int *)0,
		(struct proc *)0);
}

/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_acct.c	7.10 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "proc.h"
#include "user.h"
#include "vnode.h"
#include "mount.h"
#include "kernel.h"
#include "acct.h"
#include "uio.h"
#include "syslog.h"
#include "ioctl.h"
#include "termios.h"
#include "tty.h"

/*
 * Values associated with enabling and disabling accounting
 */
int	acctsuspend = 2;	/* stop accounting when < 2% free space left */
int	acctresume = 4;		/* resume when free space risen to > 4% */
struct	timeval chk = { 15, 0 };/* frequency to check space for accounting */

/*
 * SHOULD REPLACE THIS WITH A DRIVER THAT CAN BE READ TO SIMPLIFY.
 */
struct	vnode *acctp;
struct	vnode *savacctp;

/*
 * Perform process accounting functions.
 */
sysacct()
{
	register struct vnode *vp;
	register struct a {
		char	*fname;
	} *uap = (struct a *)u.u_ap;
	register struct nameidata *ndp = &u.u_nd;
	extern int acctwatch();
	struct vnode *oacctp;

	if (u.u_error = suser(u.u_cred, &u.u_acflag))
		return;
	if (savacctp) {
		acctp = savacctp;
		savacctp = NULL;
	}
	if (uap->fname==NULL) {
		if (vp = acctp) {
			acctp = NULL;
			vrele(vp);
			untimeout(acctwatch, (caddr_t)&chk);
		}
		return;
	}
	ndp->ni_nameiop = LOOKUP | FOLLOW;
	ndp->ni_segflg = UIO_USERSPACE;
	ndp->ni_dirp = uap->fname;
	if (u.u_error = namei(ndp))
		return;
	vp = ndp->ni_vp;
	if (vp->v_type != VREG) {
		u.u_error = EACCES;
		vrele(vp);
		return;
	}
	if (vp->v_mount->mnt_flag & MNT_RDONLY) {
		u.u_error = EROFS;
		vrele(vp);
		return;
	}
	oacctp = acctp;
	acctp = vp;
	if (oacctp)
		vrele(oacctp);
	acctwatch(&chk);
}

/*
 * Periodically check the file system to see if accounting
 * should be turned on or off.
 */
acctwatch(resettime)
	struct timeval *resettime;
{
	struct statfs sb;

	if (savacctp) {
		(void)VFS_STATFS(savacctp->v_mount, &sb);
		if (sb.f_bavail > acctresume * sb.f_blocks / 100) {
			acctp = savacctp;
			savacctp = NULL;
			log(LOG_NOTICE, "Accounting resumed\n");
			return;
		}
	}
	if (acctp == NULL)
		return;
	(void)VFS_STATFS(acctp->v_mount, &sb);
	if (sb.f_bavail <= acctsuspend * sb.f_blocks / 100) {
		savacctp = acctp;
		acctp = NULL;
		log(LOG_NOTICE, "Accounting suspended\n");
	}
	timeout(acctwatch, (caddr_t)resettime, hzto(resettime));
}

/*
 * On exit, write a record on the accounting file.
 */
acct()
{
	register struct rusage *ru;
	struct vnode *vp;
	struct timeval t, ut, st;
	int i, s;
	struct acct acctbuf;
	register struct acct *ap = &acctbuf;
	register struct proc *p = u.u_procp;

	if ((vp = acctp) == NULL)
		return;
	bcopy(p->p_comm, ap->ac_comm, sizeof(ap->ac_comm));
	ru = &u.u_ru;
	s = splclock();
	ut = p->p_utime;
	st = p->p_stime;
	t = time;
	splx(s);
	ap->ac_utime = compress(ut.tv_sec, ut.tv_usec);
	ap->ac_stime = compress(st.tv_sec, st.tv_usec);
	timevalsub(&t, &u.u_start);
	ap->ac_etime = compress(t.tv_sec, t.tv_usec);
	ap->ac_btime = u.u_start.tv_sec;
	ap->ac_uid = u.u_procp->p_ruid;
	ap->ac_gid = u.u_procp->p_rgid;
	t = st;
	timevaladd(&t, &ut);
	if (i = t.tv_sec * hz + t.tv_usec / tick)
		ap->ac_mem = (ru->ru_ixrss+ru->ru_idrss+ru->ru_isrss) / i;
	else
		ap->ac_mem = 0;
	ap->ac_mem >>= CLSIZELOG2;
	ap->ac_io = compress(ru->ru_inblock + ru->ru_oublock, (long)0);
	if (p->p_flag&SCTTY && p->p_session->s_ttyp)
		ap->ac_tty = p->p_session->s_ttyp->t_dev;
	else
		ap->ac_tty = NODEV;
	ap->ac_flag = u.u_acflag;
	u.u_error = vn_rdwr(UIO_WRITE, vp, (caddr_t)ap, sizeof (acctbuf),
		(off_t)0, UIO_SYSSPACE, IO_UNIT|IO_APPEND, u.u_cred, (int *)0);
}

/*
 * Produce a pseudo-floating point representation
 * with 3 bits base-8 exponent, 13 bits fraction.
 */
compress(t, ut)
	register long t;
	long ut;
{
	register exp = 0, round = 0;

	t = t * AHZ;  /* compiler will convert only this format to a shift */
	if (ut)
		t += ut / (1000000 / AHZ);
	while (t >= 8192) {
		exp++;
		round = t&04;
		t >>= 3;
	}
	if (round) {
		t++;
		if (t >= 8192) {
			t >>= 3;
			exp++;
		}
	}
	return ((exp<<13) + t);
}

/*-
 * Copyright (c) 1982, 1986, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)kern_acct.c	7.28 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/resourcevar.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <sys/tty.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/kernel.h>
#include <sys/file.h>
#include <sys/acct.h>
#include <sys/syslog.h>

/*
 * Values associated with enabling and disabling accounting
 */
int	acctsuspend = 2;	/* stop accounting when < 2% free space left */
int	acctresume = 4;		/* resume when free space risen to > 4% */
int	acctchkfreq = 15;	/* frequency (in seconds) to check space */

/*
 * SHOULD REPLACE THIS WITH A DRIVER THAT CAN BE READ TO SIMPLIFY.
 */
struct	vnode *acctp;
struct	vnode *savacctp;

/*
 * Enable or disable process accounting.
 *
 * If a non-null filename is given, that file is used to store accounting
 * records on process exit. If a null filename is given process accounting
 * is suspended. If accounting is enabled, the system checks the amount
 * of freespace on the filesystem at timeval intervals. If the amount of
 * freespace is below acctsuspend percent, accounting is suspended. If
 * accounting has been suspended, and freespace rises above acctresume,
 * accounting is resumed.
 */
/* ARGSUSED */
struct sysacct_args {
	char	*fname;
};
sysacct(p, uap, retval)
	struct proc *p;
	struct sysacct_args *uap;
	int *retval;
{
	register struct vnode *vp;
	extern void acctwatch __P((void *));
	struct vnode *oacctp;
	int error;
	struct nameidata nd;

	if (error = suser(p->p_ucred, &p->p_acflag))
		return (error);
	if (savacctp) {
		acctp = savacctp;
		savacctp = NULL;
	}
	if (uap->fname == NULL) {
		if (vp = acctp) {
			acctp = NULL;
			error = vn_close(vp, FWRITE, p->p_ucred, p);
			untimeout(acctwatch, NULL);
		}
		return (error);
	}
	NDINIT(&nd, LOOKUP, FOLLOW, UIO_USERSPACE, uap->fname, p);
	if (error = vn_open(&nd, FWRITE, 0644))
		return (error);
	vp = nd.ni_vp;
	VOP_UNLOCK(vp);
	if (vp->v_type != VREG) {
		(void) vn_close(vp, FWRITE, p->p_ucred, p);
		return (EACCES);
	}
	oacctp = acctp;
	acctp = vp;
	if (oacctp)
		error = vn_close(oacctp, FWRITE, p->p_ucred, p);
	acctwatch(NULL);
	return (error);
}

/*
 * Periodically check the file system to see if accounting
 * should be turned on or off.
 */
/* ARGSUSED */
void
acctwatch(a)
	void *a;
{
	struct statfs sb;

	if (savacctp) {
		(void)VFS_STATFS(savacctp->v_mount, &sb, (struct proc *)0);
		if (sb.f_bavail > acctresume * sb.f_blocks / 100) {
			acctp = savacctp;
			savacctp = NULL;
			log(LOG_NOTICE, "Accounting resumed\n");
		}
	} else {
		if (acctp == NULL)
			return;
		(void)VFS_STATFS(acctp->v_mount, &sb, (struct proc *)0);
		if (sb.f_bavail <= acctsuspend * sb.f_blocks / 100) {
			savacctp = acctp;
			acctp = NULL;
			log(LOG_NOTICE, "Accounting suspended\n");
		}
	}
	timeout(acctwatch, NULL, acctchkfreq * hz);
}

/*
 * This routine calculates an accounting record for a process and,
 * if accounting is enabled, writes it to the accounting file.
 */
acct(p)
	register struct proc *p;
{
	register struct rusage *ru;
	struct vnode *vp;
	struct timeval t, ut, st;
	int i, s;
	struct acct acctbuf;
	register struct acct *ap = &acctbuf;

	if ((vp = acctp) == NULL)
		return (0);
	bcopy(p->p_comm, ap->ac_comm, sizeof(ap->ac_comm));
	ru = &p->p_stats->p_ru;
	calcru(p, &ut, &st, NULL);
	s = splclock();
	t = time;
	splx(s);
	ap->ac_utime = compress(ut.tv_sec, ut.tv_usec);
	ap->ac_stime = compress(st.tv_sec, st.tv_usec);
	timevalsub(&t, &p->p_stats->p_start);
	ap->ac_etime = compress(t.tv_sec, t.tv_usec);
	ap->ac_btime = p->p_stats->p_start.tv_sec;
	ap->ac_uid = p->p_cred->p_ruid;
	ap->ac_gid = p->p_cred->p_rgid;
	t = st;
	timevaladd(&t, &ut);
	if (i = t.tv_sec * hz + t.tv_usec / tick)
		ap->ac_mem = (ru->ru_ixrss + ru->ru_idrss + ru->ru_isrss) / i;
	else
		ap->ac_mem = 0;
	ap->ac_io = compress(ru->ru_inblock + ru->ru_oublock, (long)0);
	if (p->p_flag&SCTTY && p->p_session->s_ttyp)
		ap->ac_tty = p->p_session->s_ttyp->t_dev;
	else
		ap->ac_tty = NODEV;
	ap->ac_flag = p->p_acflag;
	LEASE_CHECK(vp, p, p->p_ucred, LEASE_WRITE);
	return (vn_rdwr(UIO_WRITE, vp, (caddr_t)ap, sizeof (acctbuf), (off_t)0,
		UIO_SYSSPACE, IO_UNIT|IO_APPEND, p->p_ucred, (int *)0,
		(struct proc *)0));
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

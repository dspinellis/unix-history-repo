/*	sys_xxx.c	4.47	83/02/10	*/

#include "../machine/reg.h"
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/timeb.h"
#include "../h/times.h"
#include "../h/reboot.h"
#include "../h/fs.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mount.h"
#include "../h/file.h"
#ifndef NOCOMPAT
#include "../h/ioctl.h"
#endif
#include "../h/tty.h"
#include "../h/vm.h"
#include "../h/cmap.h"
#include "../h/trace.h"
#include "../h/text.h"
#include "../h/vadvise.h"
#include "../h/quota.h"
#include "../h/descrip.h"
#include "../h/kernel.h"
#include "../h/acct.h"
#include "../h/uio.h"
#include "../h/nami.h"

#ifdef vax
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"
#endif

/*
 * mode mask for creation of files
 */
umask()
{
	register struct a {
		int	mask;
	} *uap;

	uap = (struct a *)u.u_ap;
	u.u_r.r_val1 = u.u_cmask;
#ifdef MUSH
	if (u.u_quota->q_syflags & QF_UMASK && u.u_uid != 0)
		return;
#endif
	u.u_cmask = uap->mask & 07777;
}

reboot()
{
	register struct a {
		int	opt;
	};

	if (suser())
		boot(RB_BOOT, ((struct a *)u.u_ap)->opt);
}

smount()
{
	register struct a {
		char	*fspec;
		char	*freg;
		int	ronly;
	} *uap;
	dev_t dev;
	register struct inode *ip;
	register struct fs *fs;
	register char *cp;

	uap = (struct a *)u.u_ap;
	dev = getmdev();
	if (u.u_error)
		return;
	u.u_dirp = (caddr_t)uap->freg;
	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return;
	if (ip->i_count!=1 || (ip->i_mode&IFMT) != IFDIR) {
		iput(ip);
		u.u_error = EBUSY;
		return;
	}
	fs = mountfs(dev, uap->ronly, ip);
	if (fs == 0)
		return;
	u.u_dirp = uap->freg;
	for (cp = fs->fs_fsmnt; cp < &fs->fs_fsmnt[sizeof(fs->fs_fsmnt) - 2]; )
		if ((*cp++ = uchar()) == 0)
			u.u_dirp--;		/* get 0 again */
	*cp = 0;
}

/* this routine has lousy error codes */
/* this routine has races if running twice */
struct fs *
mountfs(dev, ronly, ip)
	dev_t dev;
	int ronly;
	struct inode *ip;
{
	register struct mount *mp = 0;
	struct buf *tp = 0;
	register struct buf *bp = 0;
	register struct fs *fs;
	int blks;
	caddr_t space;
	int i, size;

	u.u_error =
	    (*bdevsw[major(dev)].d_open)(dev, ronly ? FREAD : FREAD|FWRITE);
	if (u.u_error) {
		u.u_error = EIO;
		goto out;
	}
	tp = bread(dev, SBLOCK, SBSIZE);
	if (tp->b_flags & B_ERROR)
		goto out;
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != 0 && dev == mp->m_dev) {
			mp = 0;
			goto out;
		}
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp == 0)
			goto found;
	mp = 0;
	goto out;
found:
	mp->m_bufp = tp;	/* just to reserve this slot */
	mp->m_dev = NODEV;
	fs = tp->b_un.b_fs;
	bp = geteblk(fs->fs_sbsize);
	mp->m_bufp = bp;
	bcopy((caddr_t)tp->b_un.b_addr, (caddr_t)bp->b_un.b_addr,
		fs->fs_sbsize);
	brelse(tp);
	tp = 0;
	fs = bp->b_un.b_fs;
	if (fs->fs_magic != FS_MAGIC || fs->fs_bsize > MAXBSIZE)
		goto out;
	fs->fs_ronly = (ronly != 0);
	if (ronly == 0)
		fs->fs_fmod = 1;
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = wmemall(vmemall, (int)fs->fs_cssize);
	if (space == 0)
		goto out;
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
		tp = bread(dev, fsbtodb(fs, fs->fs_csaddr + i), size);
		if (tp->b_flags&B_ERROR) {
			wmemfree(space, (int)fs->fs_cssize);
			goto out;
		}
		bcopy((caddr_t)tp->b_un.b_addr, space, (u_int)size);
		fs->fs_csp[i / fs->fs_frag] = (struct csum *)space;
		space += size;
		brelse(tp);
		tp = 0;
	}
	mp->m_inodp = ip;
	mp->m_dev = dev;
	if (ip) {
		ip->i_flag |= IMOUNT;
		iunlock(ip);
	}
	return (fs);
out:
	u.u_error = EBUSY;
out1:
	if (ip)
		iput(ip);
	if (mp)
		mp->m_bufp = 0;
	if (bp)
		brelse(bp);
	if (tp)
		brelse(tp);
	return (0);
}

unmount()
{
	oumount();		/* TEMPORARY */

}

oumount()
{
	register struct a {
		char	*fspec;
	};
	dev_t dev;
	register struct mount *mp;
	int stillopen;
	register struct inode *ip;
	register struct fs *fs;
	int flag;

	dev = getmdev();
	if (u.u_error)
		return;
	xumount(dev);	/* remove unused sticky files from text table */
	update();
	for (mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if (mp->m_bufp != NULL && dev == mp->m_dev)
			goto found;
	u.u_error = EINVAL;
	return;

found:
#ifdef QUOTA
	if ((stillopen = iflush(dev, mp->m_qinod)) < 0) {
#else
	if ((stillopen = iflush(dev)) < 0) {
#endif
		u.u_error = EBUSY;
		return;
	}
#ifdef QUOTA
	closedq(mp);
	/*
	 * here we have to iflush again to get rid of the quota inode.
	 * A drag, but it would be ugly to cheat, & this doesn't happen often
	 */
	(void)iflush(dev, (struct inode *)NULL);
#endif
	ip = mp->m_inodp;
	ip->i_flag &= ~IMOUNT;
	irele(ip);
	fs = mp->m_bufp->b_un.b_fs;
	wmemfree((caddr_t)fs->fs_csp[0], (int)fs->fs_cssize);
	flag = !fs->fs_ronly;
	brelse(mp->m_bufp);
	mp->m_bufp = 0;
	mp->m_dev = 0;
	mpurge(mp - &mount[0]);
	if (!stillopen) {
		(*bdevsw[major(dev)].d_close)(dev, flag);
		binval(dev);
	}
}

sbupdate(mp)
	struct mount *mp;
{
	register struct fs *fs = mp->m_bufp->b_un.b_fs;
	register struct buf *bp;
	int blks;
	caddr_t space;
	int i, size;

	bp = getblk(mp->m_dev, SBLOCK, fs->fs_sbsize);
	bcopy((caddr_t)fs, bp->b_un.b_addr, fs->fs_sbsize);
	bwrite(bp);
	blks = howmany(fs->fs_cssize, fs->fs_fsize);
	space = (caddr_t)fs->fs_csp[0];
	for (i = 0; i < blks; i += fs->fs_frag) {
		size = fs->fs_bsize;
		if (i + fs->fs_frag > blks)
			size = (blks - i) * fs->fs_fsize;
		bp = getblk(mp->m_dev, fsbtodb(fs, fs->fs_csaddr + i), size);
		bcopy(space, bp->b_un.b_addr, (u_int)size);
		space += size;
		bwrite(bp);
	}
}

/*
 * Common code for mount and umount.
 * Check that the user's argument is a reasonable
 * thing on which to mount, and return the device number if so.
 */
dev_t
getmdev()
{
	dev_t dev;
	register struct inode *ip;

	if (!suser())
		return (NODEV);
	ip = namei(uchar, LOOKUP, 1);
	if (ip == NULL)
		return (NODEV);
	if ((ip->i_mode&IFMT) != IFBLK)
		u.u_error = ENOTBLK;
	dev = (dev_t)ip->i_rdev;
	if (major(dev) >= nblkdev)
		u.u_error = ENXIO;
	iput(ip);
	return (dev);
}

#ifdef LUCAS
/*
 * nap: routine to sleep for a time in 60'ths of a second
 */
#define MAX_NAPPING	(10)
#define PNAP		(PZERO+1)
int number_napping;

napwakeup(arg)
	caddr_t arg;
{
	wakeup(arg);
	number_napping--;
}

nap()
{
	register struct a {
		int	deltat;
	} *uap;

	uap = (struct a *)u.u_ap;
	spl6();
	if (number_napping < MAX_NAPPING) {
		number_napping++;
		timeout(napwakeup, u.u_procp, uap->deltat);
		sleep(u.u_procp, PNAP);
	} else
		u.u_error = EINVAL;
	spl0();
}
#endif

vfork()
{

	fork1(1);
}

#ifdef vax
resuba()
{

	if (suser())
	if (u.u_arg[0] < numuba)
		ubareset(u.u_arg[0]);
}
#endif

profil()
{
	register struct a {
		short	*bufbase;
		unsigned bufsize;
		unsigned pcoffset;
		unsigned pcscale;
	} *uap;

	uap = (struct a *)u.u_ap;
	u.u_prof.pr_base = uap->bufbase;
	u.u_prof.pr_size = uap->bufsize;
	u.u_prof.pr_off = uap->pcoffset;
	u.u_prof.pr_scale = uap->pcscale;
}

/*
 * Revoke access the current tty by all processes.
 * Used only by the super-user in init
 * to give ``clean'' terminals at login.
 */
vhangup()
{

	if (!suser())
		return;
	if (u.u_ttyp == NULL)
		return;
	forceclose(u.u_ttyd);
	if ((u.u_ttyp->t_state) & TS_ISOPEN)
		gsignal(u.u_ttyp->t_pgrp, SIGHUP);
}

forceclose(dev)
	dev_t dev;
{
	register struct file *fp;
	register struct inode *ip;

	for (fp = file; fp < fileNFILE; fp++) {
		if (fp->f_type == DTYPE_SOCKET)		/* XXX */
			continue;
		if (fp->f_count==0)
			continue;
		if ((ip = fp->f_inode) == 0)
			continue;
		if ((ip->i_mode & IFMT) != IFCHR)
			continue;
		if (ip->i_rdev != dev)
			continue;
		fp->f_flag &= ~(FREAD|FWRITE);
	}
}

#ifdef TRACE
int	nvualarm;

vtrace()
{
	register struct a {
		int	request;
		int	value;
	} *uap;
	int vdoualarm();

	uap = (struct a *)u.u_ap;
	switch (uap->request) {

	case VTR_DISABLE:		/* disable a trace point */
	case VTR_ENABLE:		/* enable a trace point */
		if (uap->value < 0 || uap->value >= TR_NFLAGS)
			u.u_error = EINVAL;
		else {
			u.u_r.r_val1 = traceflags[uap->value];
			traceflags[uap->value] = uap->request;
		}
		break;

	case VTR_VALUE:		/* return a trace point setting */
		if (uap->value < 0 || uap->value >= TR_NFLAGS)
			u.u_error = EINVAL;
		else
			u.u_r.r_val1 = traceflags[uap->value];
		break;

	case VTR_UALARM:	/* set a real-time ualarm, less than 1 min */
		if (uap->value <= 0 || uap->value > 60 * hz ||
		    nvualarm > 5)
			u.u_error = EINVAL;
		else {
			nvualarm++;
			timeout(vdoualarm, (caddr_t)u.u_procp->p_pid,
			    uap->value);
		}
		break;

	case VTR_STAMP:
		trace(TR_STAMP, uap->value, u.u_procp->p_pid);
		break;
	}
}

vdoualarm(arg)
	int arg;
{
	register struct proc *p;

	p = pfind(arg);
	if (p)
		psignal(p, 16);
	nvualarm--;
}
#endif

#ifndef NOCOMPAT
#ifdef vax
#include "../vax/dkio.h"
#endif
#include "../h/mtio.h"
#include "../h/socket.h"
#include "../h/mbuf.h"
#include "../net/if.h"
#include "../net/route.h"		/* for SIOCADDRT & SIOCDELRT */
/*
 * Note: these tables are sorted by
 * ioctl "code" (in ascending order).
 */
#ifdef vax
int dctls[] = { DKIOCHDR, 0 };
#endif
int fctls[] = { FIOCLEX, FIONCLEX, FIOASYNC, FIONBIO, FIONREAD, 0 };
int mctls[] = { MTIOCTOP, MTIOCGET, 0 };
int sctls[] = {
	SIOCDONE,    SIOCSKEEP,  SIOCGKEEP,  SIOCSLINGER, SIOCGLINGER,
	SIOCSENDOOB, SIOCRCVOOB, SIOCATMARK, SIOCSPGRP,   SIOCGPGRP,
	SIOCADDRT,   SIOCDELRT,  0
};
int tctls[] = {
	TIOCGETD, TIOCSETD, TIOCHPCL, TIOCMODG, TIOCMODS,
	TIOCGETP, TIOCSETP, TIOCSETN, TIOCEXCL, TIOCNXCL,
	TIOCFLUSH,TIOCSETC, TIOCGETC, TIOCREMOTE,TIOCMGET,
	TIOCMBIC, TIOCMBIS, TIOCMSET, TIOCSTART,TIOCSTOP,
	TIOCPKT,  TIOCNOTTY,TIOCGLTC, TIOCSLTC, TIOCSPGRP,
	TIOCGPGRP,TIOCCDTR, TIOCSDTR, TIOCCBRK, TIOCSBRK,
	TIOCLGET, TIOCLSET, TIOCLBIC, TIOCLBIS, 0
};

/*
 * Map an old style ioctl command to new.
 */
mapioctl(cmd)
	int cmd;
{
	register int *map, c;

	switch ((cmd >> 8) & 0xff) {

#ifdef vax
	case 'd':
		map = dctls;
		break;
#endif

	case 'f':
		map = fctls;
		break;

	case 'm':
		map = mctls;
		break;

	case 's':
		map = sctls;
		break;

	case 't':
		map = tctls;
		break;

	default:
		return (0);
	}
	while ((c = *map) && (c&0xff) < (cmd&0xff))
		map++;
	if (c && (c&0xff) == (cmd&0xff))
		return (c);
	return (0);
}
#endif

struct	inode *acctp;
struct	inode *savacctp;

long	acctlow	= 2;		/* stop accounting when < 2% data space left */
long	accthigh = 4;		/* resume when space risen to > 4% */

/*
 * Perform process accounting functions.
 */
sysacct()
{
	register struct inode *ip;
	register struct a {
		char	*fname;
	} *uap;

	uap = (struct a *)u.u_ap;
	if (suser()) {
		if (savacctp) {
			acctp = savacctp;
			savacctp = NULL;
		}
		if (uap->fname==NULL) {
			if (ip = acctp) {
				irele(ip);
				acctp = NULL;
			}
			return;
		}
		ip = namei(uchar, LOOKUP, 1);
		if(ip == NULL)
			return;
		if((ip->i_mode & IFMT) != IFREG) {
			u.u_error = EACCES;
			iput(ip);
			return;
		}
		if (acctp && (acctp->i_number != ip->i_number ||
		    acctp->i_dev != ip->i_dev))
			irele(acctp);
		acctp = ip;
		iunlock(ip);
	}
}

struct	acct acctbuf;
/*
 * On exit, write a record on the accounting file.
 */
acct()
{
	register i;
	register struct inode *ip;
	off_t siz;
	register struct acct *ap = &acctbuf;

	if (savacctp && savacctp->i_fs->fs_cstotal.cs_nbfree >
	    accthigh * savacctp->i_fs->fs_dsize / 100) {
		acctp = savacctp;
		savacctp = NULL;
		printf("Accounting resumed\n");
	}
	if ((ip=acctp)==NULL)
		return;
	if (acctp->i_fs->fs_cstotal.cs_nbfree <
	    acctlow * acctp->i_fs->fs_dsize / 100) {
		savacctp = acctp;
		acctp = NULL;
		printf("Accounting suspended\n");
		return;
	}
	ilock(ip);
	for (i=0; i<sizeof(ap->ac_comm); i++)
		ap->ac_comm[i] = u.u_comm[i];
	ap->ac_utime = compress((long)u.u_ru.ru_utime.tv_sec);
	ap->ac_stime = compress((long)u.u_ru.ru_stime.tv_sec);
	ap->ac_etime = compress((long)(time.tv_sec - u.u_start));
	ap->ac_btime = u.u_start;
	ap->ac_uid = u.u_ruid;
	ap->ac_gid = u.u_rgid;
	ap->ac_mem = 0;
	if (i = u.u_ru.ru_utime.tv_sec + u.u_ru.ru_stime.tv_sec)
		ap->ac_mem =
		    (u.u_ru.ru_ixrss + u.u_ru.ru_idrss + u.u_ru.ru_isrss) / i;
	ap->ac_io = compress((long)(u.u_ru.ru_inblock + u.u_ru.ru_oublock));
	if (u.u_ttyp)
		ap->ac_tty = u.u_ttyd;
	else
		ap->ac_tty = NODEV;
	ap->ac_flag = u.u_acflag;
	siz = ip->i_size;
	u.u_error =
	    rdwri(UIO_WRITE, ip, (caddr_t)ap, sizeof (acctbuf), siz,
		1, (int *)0);
	if (u.u_error)
		ip->i_size = siz;
	iunlock(ip);
}

/*
 * Produce a pseudo-floating point representation
 * with 3 bits base-8 exponent, 13 bits fraction.
 */
compress(t)
register long t;
{
	register exp = 0, round = 0;

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
	return((exp<<13) + t);
}


#
/*
 */

/*
 * Everything in this file is a routine implementing a system call.
 */

#include "../param.h"
#include "../user.h"
#include "../reg.h"
#include "../inode.h"
#include "../systm.h"
#include "../proc.h"

getswit()
{

	u.u_ar0[R0] = SW->integ;
}

gtime()
{

	u.u_ar0[R0] = time[0];
	u.u_ar0[R1] = time[1];
}

stime()
{

	if(suser()) {
		time[0] = u.u_ar0[R0];
		time[1] = u.u_ar0[R1];
		wakeup(tout);
	}
}

setuid()
{
	register uid;

	uid = u.u_ar0[R0].lobyte;
	if(u.u_ruid == uid.lobyte || suser()) {
		u.u_uid = uid;
		u.u_procp->p_uid = uid;
		u.u_ruid = uid;
	}
}

getuid()
{

	u.u_ar0[R0].lobyte = u.u_ruid;
	u.u_ar0[R0].hibyte = u.u_uid;
}

setgid()
{
	register gid;

	gid = u.u_ar0[R0].lobyte;
	if(u.u_rgid == gid.lobyte || suser()) {
		u.u_gid = gid;
		u.u_rgid = gid;
	}
}

getgid()
{

	u.u_ar0[R0].lobyte = u.u_rgid;
	u.u_ar0[R0].hibyte = u.u_gid;
}

getpid()
{
	u.u_ar0[R0] = u.u_procp->p_pid;
}

sync()
{

	update();
}

nice()
{
	register n;

	n = u.u_ar0[R0];
	if(n > 20)
		n = 20;
	if(n < 0 && !suser())
		n = 0;
	u.u_procp->p_nice = n;
}

/*
 * Unlink system call.
 * panic: unlink -- "cannot happen"
 */
unlink()
{
	register *ip, *pp;
	extern uchar;

	pp = namei(&uchar, 2);
	if(pp == NULL)
		return;
	prele(pp);
	ip = iget(pp->i_dev, u.u_dent.u_ino);
	if(ip == NULL)
		panic("unlink -- iget");
	if((ip->i_mode&IFMT)==IFDIR && !suser())
		goto out;
	u.u_offset[1] =- DIRSIZ+2;
	u.u_base = &u.u_dent;
	u.u_count = DIRSIZ+2;
	u.u_dent.u_ino = 0;
	writei(pp);
	ip->i_nlink--;
	ip->i_flag =| IUPD;

out:
	iput(pp);
	iput(ip);
}

chdir()
{
	register *ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	if((ip->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
	bad:
		iput(ip);
		return;
	}
	if(access(ip, IEXEC))
		goto bad;
	iput(u.u_cdir);
	u.u_cdir = ip;
	prele(ip);
}

chmod()
{
	register *ip;

	if ((ip = owner()) == NULL)
		return;
	ip->i_mode =& ~07777;
	if (u.u_uid)
		u.u_arg[1] =& ~ISVTX;
	ip->i_mode =| u.u_arg[1]&07777;
	ip->i_flag =| IUPD;
	iput(ip);
}

chown()
{
	register *ip;

	if (!suser() || (ip = owner()) == NULL)
		return;
	ip->i_uid = u.u_arg[1].lobyte;
	ip->i_gid = u.u_arg[1].hibyte;
	ip->i_flag =| IUPD;
	iput(ip);
}

/*
 * Change modified date of file:
 * time to r0-r1; sys smdate; file
 * This call has been withdrawn because it messes up
 * incremental dumps (pseudo-old files aren't dumped).
 * It works though and you can uncomment it if you like.

smdate()
{
	register struct inode *ip;
	register int *tp;
	int tbuf[2];

	if ((ip = owner()) == NULL)
		return;
	ip->i_flag =| IUPD;
	tp = &tbuf[2];
	*--tp = u.u_ar0[R1];
	*--tp = u.u_ar0[R0];
	iupdat(ip, tp);
	ip->i_flag =& ~IUPD;
	iput(ip);
}
*/

ssig()
{
	register a;

	a = u.u_arg[0];
	if(a<=0 || a>=NSIG || a ==SIGKIL) {
		u.u_error = EINVAL;
		return;
	}
	u.u_ar0[R0] = u.u_signal[a];
	u.u_signal[a] = u.u_arg[1];
	if(u.u_procp->p_sig == a)
		u.u_procp->p_sig = 0;
}

kill()
{
	register struct proc *p, *q;
	register a;
	int f;

	f = 0;
	a = u.u_ar0[R0];
	q = u.u_procp;
	for(p = &proc[0]; p < &proc[NPROC]; p++) {
		if(p == q)
			continue;
		if(a != 0 && p->p_pid != a)
			continue;
		if(a == 0 && (p->p_ttyp != q->p_ttyp || p <= &proc[1]))
			continue;
		if(u.u_uid != 0 && u.u_uid != p->p_uid)
			continue;
		f++;
		psignal(p, u.u_arg[0]);
	}
	if(f == 0)
		u.u_error = ESRCH;
}

times()
{
	register *p;

	for(p = &u.u_utime; p  < &u.u_utime+6;) {
		suword(u.u_arg[0], *p++);
		u.u_arg[0] =+ 2;
	}
}

profil()
{

	u.u_prof[0] = u.u_arg[0] & ~1;	/* base of sample buf */
	u.u_prof[1] = u.u_arg[1];	/* size of same */
	u.u_prof[2] = u.u_arg[2];	/* pc offset */
	u.u_prof[3] = (u.u_arg[3]>>1) & 077777; /* pc scale */
}

#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/reg.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/filsys.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/file.h"

#define	CSW	0177570
struct
{
	int	csw;
};

getswit()
{

	u.u_ar0[R0] = CSW->csw;
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

	if(u.u_ruid==u.u_arg[0] || suser()) {
		u.u_uid = u.u_ar0[R0];
		u.u_ruid = u.u_uid;
	}
}

getuid()
{

	u.u_ar0[R0] = u.u_ruid;
}

setgid()
{

	if(u.u_rgid==u.u_arg[0] || suser()) {
		u.u_gid = u.u_ar0[R0];
		u.u_rgid = u.u_gid;
	}
}

getgid()
{

	u.u_ar0[R0] = u.u_rgid;
}

sync()
{

	update();
}

unlink()
{
	int i, d;
	int *ip;
	extern uchar;

	ip = namei(&uchar, 2);
	if(ip == NULL)
		return;
	d = ip->i_dev;
	i = u.u_dent.u_ino;
	u.u_offset[1] =- DIRSIZ+2;
	u.u_base = &u.u_dent;
	u.u_count = DIRSIZ+2;
	u.u_dent.u_ino = 0;
	writei(ip);
	iput(ip);
	ip = iget(d, i);
	if(ip == NULL)
		panic("unlink - iget");
	ip->i_nlink--;
	ip->i_flag =| IUPD;
	iput(ip);
}

chdir()
{
	int *ip;
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
	ip->i_flag =& ~ILOCK;
}

chmod()
{
	int *ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	if(owner(ip)) {
		ip->i_mode =& ~07777;
		ip->i_mode =| u.u_arg[1]&07777;
		ip->i_flag =| IUPD;
	}
	iput(ip);
}

chown()
{
	int *ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	if(owner(ip)) {
		ip->i_uid = u.u_arg[1];
		ip->i_mode =& ~ISUID;
		ip->i_flag =| IUPD;
	}
	iput(ip);
}

fstat()
{
	int *fp;

	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	stat1(fp->f_inode, u.u_arg[0]);
}

stat()
{
	int ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	stat1(ip, u.u_arg[1]);
	iput(ip);
}

stat1(ip, ub)
int *ip;
{
	int i, *bp, *cp;

	iupdat(ip);
	bp = bread(ip->i_dev, ldiv(0, ip->i_number+31, 16));
	cp = bp->b_addr + 32*lrem(0, ip->i_number+31, 16) + 24;
	ip = &(ip->i_dev);
	for(i=0; i<14; i++) {
		suword(ub, *ip++);
		ub =+ 2;
	}
	for(i=0; i<4; i++) {
		suword(ub, *cp++);
		ub =+ 2;
	}
	brelse(bp);
}

dup()
{
	int i, *fp;

	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	if ((i = ufalloc()) < 0)
		return;
	u.u_ofile[i] = fp;
	fp->f_count++;
}

smount()
{
	int *ip, d, i;
	struct mount *mp, *smp;
	extern uchar;

	d = getmdev();
	if(u.u_error)
		return;
	u.u_dirp = u.u_arg[1];
	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	if(ip->i_count!=1 || (ip->i_mode&(IFBLK&IFCHR))!=0)
		goto out;
	smp = NULL;
	mp = &mount[0];
	for(i=0; i<NMOUNT; i++) {
		if(mp->m_bufp != NULL) {
			if(d == mp->m_dev)
				goto out;
		} else
		if(smp == NULL)
			smp = mp;
		mp++;
	}
	if(smp == NULL)
		goto out;
	mp = bread(d, 1);
	if(u.u_error) {
		brelse(mp);
		goto out1;
	}
	smp->m_inodp = ip;
	smp->m_dev = d;
	smp->m_bufp = getblk(NODEV);
	for(i=0; i<512; i++)
		smp->m_bufp->b_addr[i] = mp->b_addr[i];
	smp = smp->m_bufp->b_addr;
	smp->s_ilock = 0;
	smp->s_flock = 0;
	smp->s_ninode = 0;
	brelse(mp);
	ip->i_flag =| IMOUNT;
	ip->i_flag =& ~ILOCK;
	return;

out:
	u.u_error = EBUSY;
out1:
	iput(ip);
}

sumount()
{
	int i, d;
	struct inode *ip;
	struct mount *mp;

	update();
	d = getmdev();
	if(u.u_error)
		return;
	mp = &mount[0];
	for(i=0; i<NMOUNT; i++) {
		if(mp->m_bufp!=NULL && d==mp->m_dev)
			goto found;
		mp++;
	}
	u.u_error = EINVAL;
	return;

found:
	ip = &inode[0];
	for(i=0; i<NINODE; i++) {
		if(ip->i_number!=0 && d==ip->i_dev) {
			u.u_error = EBUSY;
			return;
		}
		ip++;
	}
	ip = mp->m_inodp;
	ip->i_flag =& ~IMOUNT;
	iput(ip);
	ip = mp->m_bufp;
	mp->m_bufp = NULL;
	brelse(ip);
}

getmdev()
{
	int d, *ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	if((ip->i_mode&IFMT) != IFBLK)
		u.u_error = ENOTBLK;
	d = ip->i_addr[0];
	iput(ip);
	return(d);
}

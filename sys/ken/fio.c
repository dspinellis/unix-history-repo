#include "/sys/nsys/param.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/file.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/reg.h"

getf(f)
{
	register *fp;

	if(f<0 || f>=NOFILE)
		goto bad;
	fp = u.u_ofile[f];
	if(fp == NULL) {
	bad:
		u.u_error = EBADF;
		fp = NULL;
	}
	return(fp);
}

closef(fp)
int *fp;
{
	register *rfp;

	rfp = fp;
	if(rfp->f_count <= 1) {
		closei(rfp->f_inode, rfp->f_flag&FWRITE);
		fp->f_count = 0;
	} else
		rfp->f_count--;
}

closei(ip, rw)
int *ip;
{
	register *rip;

	rip = ip;
	if(rip->i_count <= 1)
	switch(rip->i_mode&IFMT) {

	case IFCHR:
		(*cdevsw[rip->i_addr[0].d_major].d_close)
			(rip->i_addr[0], rw);
		break;

	case IFBLK:
		(*bdevsw[rip->i_addr[0].d_major].d_close)
			(rip->i_addr[0], rw);
	}
	iput(ip);
}

openi(ip, rw)
int *ip;
{
	register *rip;

	rip = ip;
	switch(rip->i_mode&IFMT) {

	case IFCHR:
		(*cdevsw[rip->i_addr[0].d_major].d_open)
			(rip->i_addr[0], rw);
		break;

	case IFBLK:
		(*bdevsw[ip->i_addr[0].d_major].d_open)
			(ip->i_addr[0], rw);
	}
}

access(ip, mode)
int *ip;
{
	register *rip;

	if(u.u_uid == 0)
		return(0);
	rip = ip;
	if(u.u_uid != rip->i_uid) {
		mode =>> 3;
		if(u.u_gid != rip->i_gid)
			mode =>> 3;
	}
	if((rip->i_mode&mode) != 0)
		return(0);
	u.u_error = EACCES;
	return(1);
}

owner(ip)
int *ip;
{

	if(u.u_uid == ip->i_uid)
		return(1);
	return(suser());
}

suser()
{

	if(u.u_uid == 0)
		return(1);
	u.u_error = EPERM;
	return(0);
}

ufalloc()
{
	register int i;

	for (i=0; i<NOFILE; i++)
		if (u.u_ofile[i] == NULL) {
			u.u_ar0[R0] = i;
			return(i);
		}
	u.u_error = EMFILE;
	return(-1);
}

falloc()
{
	register struct file *fp;
	register int i;

	if ((i = ufalloc()) < 0)
		return(NULL);
	for (fp = &file[0]; fp < &file[NFILE]; fp++)
		if (fp->f_count==0) {
			u.u_ofile[i] = fp;
			fp->f_count++;
			return(fp);
		}
	printf("no file\n");
	u.u_error = ENFILE;
	return(NULL);
}

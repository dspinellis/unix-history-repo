#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/reg.h"
#include "/sys/nsys/file.h"
#include "/sys/nsys/inode.h"

read()
{
	extern readi;

	rdwr(&readi, FREAD);
}

write()
{
	extern writei;

	rdwr(&writei, FWRITE);
}

rdwr(fun, mode)
int (*fun)();
{
	int *fp;

	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	if((fp->f_flag&mode) == 0) {
		u.u_error = EBADF;
		return;
	}
	u.u_base = u.u_arg[0];
	u.u_count = u.u_arg[1];
	u.u_offset[1] = fp->f_offset[1];
	u.u_offset[0] = fp->f_offset[0];
	u.u_segflg = 0;
	(*fun)(fp->f_inode);
	u.u_ar0[R0] = u.u_arg[1]-u.u_count;
	dpadd(fp->f_offset, u.u_ar0[R0]);
}

open()
{
	int *ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	u.u_arg[1]++;
	open1(ip, u.u_arg[1], 0);
}

creat()
{
	int *ip;
	extern uchar;

	ip = namei(&uchar, 1);
	if(ip == NULL) {
		if(u.u_error)
			return;
		ip = maknode(u.u_arg[1]&07777);
		open1(ip, FWRITE, 2);
	} else
		open1(ip, FWRITE, 1);
}

open1(ip, mode, trf)
int *ip;
{
	struct file *fp;

	if(trf != 2) {
		if(mode&FREAD)
			access(ip, IREAD);
		if(mode&FWRITE) {
			access(ip, IWRITE);
			if((ip->i_mode&IFMT) == IFDIR)
				u.u_error = EISDIR;
		}
	}
	ip->i_flag =& ~ILOCK;
	if(u.u_error == 0)
		openi(ip, mode&FWRITE);
	if(u.u_error) {
		iput(ip);
		return;
	}
	if(trf)
		itrunc(ip);
	if ((fp = falloc()) == NULL) {
		iput(ip);
		return;
	}
	fp->f_flag = mode&(FREAD|FWRITE);
	fp->f_inode = ip;
	fp->f_offset[1] = 0;
	fp->f_offset[0] = 0;
}

close()
{
	register *fp;

	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	u.u_ofile[u.u_ar0[R0]] = NULL;
	closef(fp);
}

seek()
{
	int n[2];
	register *fp;

	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	if(u.u_arg[1] > 2) {
		n[1] = u.u_arg[0]<<9;
		n[0] = (u.u_arg[0]>>7) & 0777;
	} else {
		n[1] = u.u_arg[0];
		n[0] = 0;
	}
	switch(u.u_arg[1]) {

	case 1:
	case 4:
		n[0] =+ fp->f_offset[0];
		dpadd(n, fp->f_offset[1]);
		break;

	default:
		n[0] =+ fp->f_inode->i_size0;
		dpadd(n, fp->f_inode->i_size1);

	case 0:
	case 3:
		;
	}
	fp->f_offset[1] = n[1];
	fp->f_offset[0] = n[0];
}

link()
{
	int *ip, *xp;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	ip->i_flag =& ~ILOCK;
	u.u_dirp = u.u_arg[1];
	xp = namei(&uchar, 1);
	if(xp != NULL) {
		u.u_error = EEXIST;
		iput(xp);
	}
	if(u.u_error)
		goto out;
	if(u.u_pdir->i_dev != ip->i_dev) {
		iput(u.u_pdir);
		u.u_error = EXDEV;
		goto out;
	}
	wdir(ip);
	ip->i_nlink++;
	ip->i_flag =| IUPD;

out:
	iput(ip);
}

mknod()
{
	int *ip;
	extern uchar;

	if(suser()) {
		ip = namei(&uchar, 1);
		if(ip != NULL) {
			u.u_error = EEXIST;
			goto out;
		}
	}
	if(u.u_error)
		return;
	ip = maknode(u.u_arg[1]);
	ip->i_addr[0] = u.u_arg[2];

out:
	iput(ip);
}

sslep()
{
	char *d[2];

	spl7();
	d[0] = time[0];
	d[1] = time[1];
	dpadd(d, u.u_ar0[R0]);

	while(dpcmp(d[0], d[1], time[0], time[1]) > 0) {
		if(dpcmp(tout[0], tout[1], time[0], time[1]) <= 0 ||
		   dpcmp(tout[0], tout[1], d[0], d[1]) > 0) {
			tout[0] = d[0];
			tout[1] = d[1];
		}
		sleep(tout, PSLEP);
	}
	spl0();
}

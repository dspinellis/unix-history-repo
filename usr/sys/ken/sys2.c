#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../reg.h"
#include "../file.h"
#include "../inode.h"

read()
{
	rdwr(FREAD);
}

write()
{
	rdwr(FWRITE);
}

rdwr(mode)
{
	register *fp, m;

	m = mode;
	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	if((fp->f_flag&m) == 0) {
		u.u_error = EBADF;
		return;
	}
	u.u_base = u.u_arg[0];
	u.u_count = u.u_arg[1];
	u.u_segflg = 0;
	if(fp->f_flag&FPIPE) {
		if(m==FREAD)
			readp(fp); else
			writep(fp);
	} else {
		u.u_offset[1] = fp->f_offset[1];
		u.u_offset[0] = fp->f_offset[0];
		if(m==FREAD)
			readi(fp->f_inode); else
			writei(fp->f_inode);
		dpadd(fp->f_offset, u.u_arg[1]-u.u_count);
	}
	u.u_ar0[R0] = u.u_arg[1]-u.u_count;
}

open()
{
	register *ip;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	u.u_arg[1]++;
	open1(ip, u.u_arg[1], 0);
}

creat()
{
	register *ip;
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
	register struct file *fp;
	register *rip, m;

	rip = ip;
	m = mode;
	if(trf != 2) {
		if(m&FREAD)
			access(rip, IREAD);
		if(m&FWRITE) {
			access(rip, IWRITE);
			if((rip->i_mode&IFMT) == IFDIR)
				u.u_error = EISDIR;
		}
	}
	if(u.u_error)
		goto out;
	if(trf)
		itrunc(rip);
	prele(rip);
	openi(rip, m&FWRITE);
	if(u.u_error)
		goto out;
	if ((fp = falloc()) == NULL)
		goto out;
	fp->f_flag = m&(FREAD|FWRITE);
	fp->f_inode = rip;
	return;

out:
	iput(rip);
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
	register *fp, t;

	fp = getf(u.u_ar0[R0]);
	if(fp == NULL)
		return;
	if(fp->f_flag&FPIPE) {
		u.u_error = ESPIPE;
		return;
	}
	t = u.u_arg[1];
	if(t > 2) {
		n[1] = u.u_arg[0]<<9;
		n[0] = u.u_arg[0]>>7;
		if(t == 3)
			n[0] =& 0777;
	} else {
		n[1] = u.u_arg[0];
		n[0] = 0;
		if(t!=0 && n[1]<0)
			n[0] = -1;
	}
	switch(t) {

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
	register *ip, *xp;
	extern uchar;

	ip = namei(&uchar, 0);
	if(ip == NULL)
		return;
	if(ip->i_nlink >= 127) {
		u.u_error = EMLINK;
		goto out;
	}
	if((ip->i_mode&IFMT)==IFDIR && !suser())
		goto out;
/*
 * NOTE:
 * the ip should not be unlocked here, BUT
 * if its not, the following namei may hang
 */
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
	register *ip;
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

#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../inode.h"
#include "../file.h"
#include "../reg.h"

#define	PIPSIZ	4096
pipe()
{
	register *ip, *rf, *wf;

	ip = ialloc(rootdev);
	if(ip == NULL)
		return;
	wf = falloc();
	if(wf == NULL) {
		iput(ip);
		return;
	}
	u.u_ar0[R1] = u.u_ar0[R0];
	rf = falloc();
	if(rf == NULL) {
		wf->f_count = 0;
		u.u_ofile[u.u_ar0[R1]] = NULL;
		iput(ip);
		return;
	}
	wf->f_flag = FWRITE|FPIPE;
	wf->f_inode = ip;
	rf->f_flag = FREAD|FPIPE;
	rf->f_inode = ip;
	ip->i_count = 2;
	ip->i_flag = IACC|IUPD;
	ip->i_mode = IALLOC;
}

readp(fp)
int *fp;
{
	register *rp, *ip;

	rp = fp;
	ip = rp->f_inode;

loop:
	plock(ip);
	if(rp->f_offset[1] == ip->i_size1) {
		if(rp->f_offset[1] != 0) {
			rp->f_offset[1] = 0;
			ip->i_size1 = 0;
			if(ip->i_mode&IWRITE) {
				ip->i_mode =& ~IWRITE;
				wakeup(ip+1);
			}
		}
		prele(ip);
		if(ip->i_count < 2)
			return;
		ip->i_mode =| IREAD;
		sleep(ip+2, PPIPE);
		goto loop;
	}
	u.u_offset[0] = 0;
	u.u_offset[1] = rp->f_offset[1];
	readi(ip);
	rp->f_offset[1] = u.u_offset[1];
	prele(ip);
}

writep(fp)
{
	register *rp, *ip, c;

	rp = fp;
	ip = rp->f_inode;
	c = u.u_count;

loop:
	plock(ip);
	if(ip->i_count<2 || c==0) {
		prele(ip);
		u.u_count = c;
		return;
	}
	if(ip->i_size1 == PIPSIZ) {
		ip->i_mode =| IWRITE;
		prele(ip);
		sleep(ip+1, PPIPE);
		goto loop;
	}
	u.u_offset[0] = 0;
	u.u_offset[1] = ip->i_size1;
	u.u_count = min(c, PIPSIZ-u.u_offset[1]);
	c =- u.u_count;
	writei(ip);
	prele(ip);
	if(ip->i_mode&IREAD) {
		ip->i_mode =& ~IREAD;
		wakeup(ip+2);
	}
	goto loop;
}

plock(ip)
int *ip;
{
	register *rp;

	rp = ip;
	while(rp->i_flag&ILOCK) {
		rp->i_flag =| IWANT;
		sleep(rp, PPIPE);
	}
	rp->i_flag =| ILOCK;
}

prele(ip)
int *ip;
{
	register *rp;

	rp = ip;
	rp->i_flag =& ~ILOCK;
	if(rp->i_flag&IWANT) {
		rp->i_flag =& ~IWANT;
		wakeup(rp);
	}
}

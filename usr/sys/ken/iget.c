#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../inode.h"
#include "../filsys.h"
#include "../conf.h"
#include "../buf.h"

struct inode *maxip;

iget(dev, ino)
int dev;
int ino;
{
	register struct inode *p;
	register *ip2;
	int *ip1;
	register struct mount *ip;

loop:
	ip = NULL;
	for(p = &inode[0]; p < &inode[NINODE]; p++) {
		if(dev==p->i_dev && ino==p->i_number) {
			if((p->i_flag&ILOCK) != 0) {
				p->i_flag =| IWANT;
				sleep(p, PINOD);
				goto loop;
			}
			if((p->i_flag&IMOUNT) != 0) {
				for(ip = &mount[0]; ip < &mount[NMOUNT]; ip++)
				if(ip->m_inodp == p) {
					dev = ip->m_dev;
					ino = ROOTINO;
					goto loop;
				}
				panic("no imt");
			}
			p->i_count++;
			p->i_flag =| ILOCK;
			return(p);
		}
		if(ip==NULL && p->i_count==0)
			ip = p;
	}
	if((p=ip) == NULL)
		panic("no inodes");
	if (p>maxip)
		maxip = p;
	p->i_dev = dev;
	p->i_number = ino;
	p->i_flag = ILOCK;
	p->i_count++;
	p->i_lastr = -1;
	ip = bread(dev, ldiv(ino+31,16));
	ip1 = ip->b_addr + 32*lrem(ino+31, 16);
	ip2 = &p->i_mode;
	while(ip2 < &p->i_addr[8])
		*ip2++ = *ip1++;
	brelse(ip);
	return(p);
}

iput(p)
struct inode *p;
{
	register *rp;

	rp = p;
	if(rp->i_count == 1) {
		rp->i_flag =| ILOCK;
		if(rp->i_nlink <= 0) {
			itrunc(rp);
			rp->i_mode = 0;
			ifree(rp->i_dev, rp->i_number);
		}
		iupdat(rp, time);
		prele(rp);
		rp->i_flag = 0;
		rp->i_number = 0;
	}
	rp->i_count--;
	prele(rp);
}

iupdat(p, tm)
int *p;
int *tm;
{
	register *ip1, *ip2, *rp;
	int *bp, i;

	rp = p;
	if((rp->i_flag&(IUPD|IACC)) != 0) {
		if(getfs(rp->i_dev)->s_ronly)
			return;
		i = rp->i_number+31;
		bp = bread(rp->i_dev, ldiv(i,16));
		ip1 = bp->b_addr + 32*lrem(i, 16);
		ip2 = &rp->i_mode;
		while(ip2 < &rp->i_addr[8])
			*ip1++ = *ip2++;
		if(rp->i_flag&IACC) {
			*ip1++ = time[0];
			*ip1++ = time[1];
		} else
			ip1 =+ 2;
		if(rp->i_flag&IUPD) {
			*ip1++ = *tm++;
			*ip1++ = *tm;
		}
		bwrite(bp);
	}
}

itrunc(ip)
int *ip;
{
	register *rp, *bp, *cp;

	rp = ip;
	if((rp->i_mode&(IFCHR&IFBLK)) != 0)
		return;
	for(ip = &rp->i_addr[0]; ip < &rp->i_addr[8]; ip++)
	if(*ip) {
		if((rp->i_mode&ILARG) != 0) {
			bp = bread(rp->i_dev, *ip);
			for(cp = bp->b_addr; cp < bp->b_addr+512; cp++)
				if(*cp)
					free(rp->i_dev, *cp);
			brelse(bp);
		}
		free(rp->i_dev, *ip);
		*ip = 0;
	}
	rp->i_mode =& ~ILARG;
	rp->i_size0 = 0;
	rp->i_size1 = 0;
	rp->i_flag =| IUPD;
}

maknode(mode)
{
	register *ip;

	ip = ialloc(u.u_pdir->i_dev);
	ip->i_flag =| IACC|IUPD;
	ip->i_mode = mode|IALLOC;
	ip->i_nlink = 1;
	ip->i_uid = u.u_uid;
	ip->i_gid = u.u_gid;
	wdir(ip);
	return(ip);
}

wdir(ip)
int *ip;
{
	register char *cp1, *cp2;

	u.u_dent.u_ino = ip->i_number;
	cp1 = &u.u_dent.u_name[0];
	for(cp2 = &u.u_dbuf[0]; cp2 < &u.u_dbuf[DIRSIZ];)
		*cp1++ = *cp2++;
	u.u_count = DIRSIZ+2;
	u.u_segflg = 1;
	u.u_base = &u.u_dent;
	writei(u.u_pdir);
	iput(u.u_pdir);
}

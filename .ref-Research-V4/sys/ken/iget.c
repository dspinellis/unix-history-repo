#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/buf.h"

iget(dev, ino)
int dev;
int ino;
{
	struct inode *p;
	int *ip, *ip1, *ip2;
	int i;

loop:
	ip = NULL;
	p = &inode[0];
	for(i=0; i<NINODE; i++) {
		if(dev==p->i_dev && ino==p->i_number) {
			if((p->i_flag&ILOCK) != 0) {
				p->i_flag =| IWANT;
				sleep(p, PINOD);
				p->i_flag =& ~IWANT;
				goto loop;
			}
			if((p->i_flag&IMOUNT) != 0)
			for(i=0; i<NMOUNT; i++)
			if(mount[i].m_inodp == p) {
				dev = mount[i].m_dev;
				ino = ROOTINO;
				goto loop;
			}
			p->i_count++;
			p->i_flag =| ILOCK;
			return(p);
		}
		if(ip==NULL && p->i_count==0)
			ip = p;
		p++;
	}
	if((p=ip) == NULL)
		panic("no inodes");
	p->i_dev = dev;
	p->i_number = ino;
	p->i_flag = ILOCK;
	p->i_count++;
	ip = bread(dev,ldiv(0,ino+31,16));
	ip1 = ip->b_addr + 32*lrem(0,ino+31,16);
	ip2 = &p->i_mode;

	/* NOTE
	 * magic number 12 below
	 * is difference between
	 * i_mode and i_addr[8]
	 */

	for(i=0; i<12; i++)
		*ip2++ = *ip1++;
	brelse(ip);
	wakeup(p);
	return(p);
}

iput(p)
struct inode *p;
{

	if(p->i_count == 1) {
		p->i_flag =| ILOCK;
		if(p->i_nlink <= 0) {
			itrunc(p);
			p->i_mode = 0;
			ifree(p->i_dev, p->i_number);
		}
		iupdat(p);
		p->i_flag = 0;
		p->i_number = 0;
		p->i_count = 0;
		wakeup(p);
		return;
	}
	p->i_count--;
	p->i_flag =& ~ILOCK;
	if(p->i_flag&IWANT)
		wakeup(p);
}

iupdat(p)
int *p;
{
	register *ip1, *ip2;
	int *bp, i;

	if((p->i_flag&(IUPD|IACC)) != 0) {
		i = p->i_number+31;
		bp = bread(p->i_dev,ldiv(0,i,16));
		ip1 = bp->b_addr + 32*lrem(0,i,16);
		ip2 = &p->i_mode;

		/*
		 * see above NOTE
		 */

		for(i=0; i<12; i++)
			*ip1++ = *ip2++;
		if(p->i_flag&IACC) {
			*ip1++ = time[0];
			*ip1++ = time[1];
		} else
			ip1 =+ 2;
		if(p->i_flag&IUPD) {
			*ip1++ = time[0];
			*ip1++ = time[1];
		}
		bwrite(bp);
		p->i_flag =& ~(IACC|IUPD);
	}
}

itrunc(ip)
int *ip;
{
	int i, j, a, b, d;
	int *bp, *cp;

	if((ip->i_mode&(IFCHR&IFBLK)) != 0)
		return;
	d = ip->i_dev;
	for(i=0; i<8; i++)
		if(a = ip->i_addr[i]) {
			if((ip->i_mode&ILARG) != 0) {
				bp = bread(d, a);
				cp = bp->b_addr;
				for(j=0; j<256; j++)
					if(b = *cp++)
						free(d, b);
				brelse(bp);
			}
			free(d, a);
			ip->i_addr[i] = 0;
		}
	ip->i_mode =& ~ILARG;
	ip->i_size0 = 0;
	ip->i_size1 = 0;
	ip->i_flag =| IUPD;
}

maknode(mode)
{
	int *ip, i;

	ip = ialloc(u.u_pdir->i_dev);
	ip->i_flag = IACC|IUPD;
	ip->i_mode = mode|IALLOC;
	ip->i_nlink = 1;
	ip->i_uid = u.u_uid;
	ip->i_gid = u.u_gid;
	ip->i_size0 = 0;
	ip->i_size1 = 0;
	for(i=0; i<8; i++)
		ip->i_addr[i] = 0;
	wdir(ip);
	return(ip);
}

wdir(ip)
int *ip;
{
	int i;

	u.u_dent.u_ino = ip->i_number;
	for(i=0; i<DIRSIZ; i++)
		u.u_dent.u_name[i] = u.u_dbuf[i];
	u.u_count = DIRSIZ+2;
	u.u_segflg = 1;
	u.u_base = &u.u_dent;
	writei(u.u_pdir);
	iput(u.u_pdir);
}

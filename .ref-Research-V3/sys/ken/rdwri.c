#include "/sys/nsys/param.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/conf.h"

readi(ip)
struct inode *ip;
{
	int *bp;
	int dn, bn, on;
	int n, i;
	char *cp;

	if(u.u_count == 0)
		return;
	ip->i_flag =| IACC;
	if((ip->i_mode&IFMT) == IFCHR) {
		(*cdevsw[ip->i_addr[0].d_major].d_read)(ip->i_addr[0]);
		return;
	}

loop:
	bn = ldiv(u.u_offset[0], u.u_offset[1], 512);
	on = lrem(u.u_offset[0], u.u_offset[1], 512);
	n = min(512-on, u.u_count);
	if((ip->i_mode&IFMT) != IFBLK) {
		dn = dpcmp(ip->i_size0, ip->i_size1,
			u.u_offset[0], u.u_offset[1]);
		if(dn <= 0)
			return;
		n = min(n, dn);
		if ((bn = bmap(ip, bn)) == 0)
			return;
		dn = ip->i_dev;
	} else
		dn = ip->i_addr[0];
	bp = bread(dn, bn);
	cp = bp->b_addr + on;
	if(u.u_segflg==0 && (n&1)==0 && (on&1)==0 && (u.u_base&1)==0) {
		if(copyout(cp, u.u_base, n))
			u.u_error = EFAULT;
		u.u_base =+ n;
		goto incr;
	} else
	if(u.u_segflg != 0) {
		for(i=0; i<n; i++) {
			*u.u_base = *cp;
			u.u_base++;
			cp++;
		}

	incr:
		dpadd(u.u_offset, n);
		u.u_count =- n;
	} else
	for(i=0; i<n; i++)
		if(passc(*cp++) < 0)
			break;
	brelse(bp);
	if(u.u_error==0 && u.u_count!=0)
		goto loop;
}

writei(ip)
struct inode *ip;
{
	int *bp;
	int dn, bn, on;
	int n, i;
	char *cp;

	if(u.u_count == 0)
		return;
	ip->i_flag =| IACC;
	if((ip->i_mode&IFMT) == IFCHR) {
		(*cdevsw[ip->i_addr[0].d_major].d_write)(ip->i_addr[0]);
		return;
	}

loop:
	bn = ldiv(u.u_offset[0], u.u_offset[1], 512);
	on = lrem(u.u_offset[0], u.u_offset[1], 512);
	n = min(512-on, u.u_count);
	if((ip->i_mode&IFMT) != IFBLK) {
		if ((bn = bmap(ip, bn)) == 0)
			return;
		dn = ip->i_dev;
	} else
		dn = ip->i_addr[0];
	if(n == 512) 
		bp = getblk(dn, bn); else
		bp = bread(dn, bn);
	cp = bp->b_addr + on;
	if(u.u_segflg==0 && (n&1)==0 && (on&1)==0 && (u.u_base&1)==0) {
		if(copyin(u.u_base, cp, n))
			u.u_error = EFAULT;
		u.u_base =+ n;
		goto incr;
	} else
	if(u.u_segflg != 0) {
		for(i=0; i<n; i++) {
			*cp = *u.u_base;
			cp++;
			u.u_base++;
		}

	incr:
		dpadd(u.u_offset, n);
		u.u_count =- n;
	} else
	for(i=0; i<n; i++)
		if(cpass(cp++) < 0)
			break;
	if(lrem(u.u_offset[0], u.u_offset[1], 512) == 0)
		bawrite(bp); else
		bdwrite(bp);
	if(dpcmp(ip->i_size0, ip->i_size1,
	  u.u_offset[0], u.u_offset[1]) < 0 &&
	  (ip->i_mode&(IFBLK&IFCHR)) == 0) {
		ip->i_size0 = u.u_offset[0];
		ip->i_size1 = u.u_offset[1];
	}
	ip->i_flag =| IUPD;
	if(u.u_error==0 && u.u_count!=0)
		goto loop;
}

max(a, b)
char *a, *b;
{

	if(a > b)
		return(a);
	return(b);
}

min(a, b)
char *a, *b;
{

	if(a < b)
		return(a);
	return(b);
}

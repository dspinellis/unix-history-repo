#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../conf.h"
#include "../inode.h"
#include "../user.h"
#include "../buf.h"
#include "../systm.h"

bmap(ip, bn)
struct inode *ip;
int bn;
{
	register *bp, *bap, nb;
	int *nbp, d, i;

	d = ip->i_dev;

	if (bn & ~03777) {
		u.u_error = EFBIG;
		return(0);
	}
	if((ip->i_mode&ILARG) == 0) {

		/*
		 * small file algorithm
		 */

		if((bn & ~7) != 0) {

			/*
			 * bad part:
			 * convert small to large
			 */

			if ((bp = alloc(d)) == NULL)
				return(0);
			bap = bp->b_addr;
			for(i=0; i<8; i++) {
				*bap++ = ip->i_addr[i];
				ip->i_addr[i] = 0;
			}
			ip->i_addr[0] = bp->b_blkno;
			bdwrite(bp);
			ip->i_mode =| ILARG;
			goto large;
		}
		nb = ip->i_addr[bn];
		if(nb == 0 && (bp = alloc(d)) != NULL) {
			bdwrite(bp);
			nb = bp->b_blkno;
			ip->i_addr[bn] = nb;
			ip->i_flag =| IUPD;
		}
		if (bn<7)
			rablock = ip->i_addr[bn+1];
		else
			rablock = 0;
		return(nb);
	}

	/*
	 * large file algorithm
	 */

    large:
	i = bn>>8;
	if((nb=ip->i_addr[i]) == 0) {
		ip->i_flag =| IUPD;
		if ((bp = alloc(d)) == NULL)
			return(0);
		nb = bp->b_blkno;
		ip->i_addr[i] = nb;
	} else
		bp = bread(d, nb);
	bap = bp->b_addr;
	i = bn & 0377;
	if((nb=bap[i]) == 0 && (nbp = alloc(d)) != NULL) {
		nb = nbp->b_blkno;
		bap[i] = nb;
		bdwrite(nbp);
		bdwrite(bp);
	} else
		brelse(bp);
	rablock = bap[i+1];
	return(nb);
}

passc(c)
char c;
{

	if(u.u_segflg)
		*u.u_base = c; else
		if(subyte(u.u_base, c) < 0) {
			u.u_error = EFAULT;
			return(-1);
		}
	u.u_count--;
	if(++u.u_offset[1] == 0)
		u.u_offset[0]++;
	u.u_base++;
	return(u.u_count == 0? -1: 0);
}

cpass()
{
	register c;

	if(u.u_count == 0)
		return(-1);
	if(u.u_segflg)
		c = *u.u_base; else
		if((c=fubyte(u.u_base)) < 0) {
			u.u_error = EFAULT;
			return(-1);
		}
	u.u_count--;
	if(++u.u_offset[1] == 0)
		u.u_offset[0]++;
	u.u_base++;
	return(c&0377);
}

nodev()
{

	u.u_error = ENODEV;
}

nulldev()
{
}

bcopy(from, to, count)
int *from, *to;
{
	register *a, *b, c;

	a = from;
	b = to;
	c = count;
	do
		*b++ = *a++;
	while(--c);
}

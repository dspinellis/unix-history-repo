#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../systm.h"
#include "../filsys.h"
#include "../buf.h"
#include "../inode.h"
#include "../user.h"

iinit()
{
	register *cp, *bp;

	bp = bread(rootdev, 1);
	cp = getblk(NODEV);
	if(u.u_error)
		panic("iinit");
	bcopy(bp->b_addr, cp->b_addr, 256);
	brelse(bp);
	mount[0].m_bufp = cp;
	mount[0].m_dev = rootdev;
	cp = cp->b_addr;
	cp->s_flock = 0;
	cp->s_ilock = 0;
	cp->s_ronly = 0;
	time[0] = cp->s_time[0];
	time[1] = cp->s_time[1];
}

alloc(dev)
{
	int bno;
	register *bp, *ip, *fp;

	fp = getfs(dev);
	while(fp->s_flock)
		sleep(&fp->s_flock, PINOD);
	bno = fp->s_free[--fp->s_nfree];
	if(bno == 0) {
		fp->s_nfree++;
		printf("No space on dev %d\n", dev);
		u.u_error = ENOSPC;
		return(NULL);
	}
	if(fp->s_nfree <= 0) {
		fp->s_flock++;
		bp = bread(dev, bno);
		ip = bp->b_addr;
		fp->s_nfree = *ip++;
		bcopy(ip, fp->s_free, 100);
		brelse(bp);
		fp->s_flock = 0;
		wakeup(&fp->s_flock);
	}
	bp = getblk(dev, bno);
	clrbuf(bp);
	fp->s_fmod = 1;
	return(bp);
}

free(dev, bno)
{
	register *fp, *bp, *ip;

	fp = getfs(dev);
	fp->s_fmod = 1;
	while(fp->s_flock)
		sleep(&fp->s_flock, PINOD);
	if(fp->s_nfree >= 100) {
		fp->s_flock++;
		bp = getblk(dev, bno);
		ip = bp->b_addr;
		*ip++ = fp->s_nfree;
		bcopy(fp->s_free, ip, 100);
		fp->s_nfree = 0;
		bwrite(bp);
		fp->s_flock = 0;
		wakeup(&fp->s_flock);
	}
	fp->s_free[fp->s_nfree++] = bno;
	fp->s_fmod = 1;
}

ialloc(dev)
{
	register *fp, *bp, *ip;
	int i, j, k, ino;

	fp = getfs(dev);
	while(fp->s_ilock)
		sleep(&fp->s_ilock, PINOD);
loop:
	if(fp->s_ninode > 0) {
		ino = fp->s_inode[--fp->s_ninode];
		ip = iget(dev, ino);
		if(ip->i_mode == 0) {
			for(bp = &ip->i_mode; bp < &ip->i_addr[8];)
				*bp++ = 0;
			fp->s_fmod = 1;
			return(ip);
		}
		printf("busy i\n");
		iput(ip);
		goto loop;
	}
	fp->s_ilock++;
	ino = 0;
	for(i=0; i<fp->s_isize; i++) {
		bp = bread(dev, i+2);
		ip = bp->b_addr;
		for(j=0; j<256; j=+16) {
			ino++;
			if(ip[j] != 0)
				continue;
			for(k=0; k<NINODE; k++)
			if(dev==inode[k].i_dev && ino==inode[k].i_number)
				goto cont;
			fp->s_inode[fp->s_ninode++] = ino;
			if(fp->s_ninode >= 100)
				break;
		cont:;
		}
		brelse(bp);
		if(fp->s_ninode >= 100)
			break;
	}
	if(fp->s_ninode <= 0)
		panic("out of inodes");
	fp->s_ilock = 0;
	wakeup(&fp->s_ilock);
	goto loop;
}

ifree(dev, ino)
{
	register *fp;

	fp = getfs(dev);
	if(fp->s_ilock)
		return;
	if(fp->s_ninode >= 100)
		return;
	fp->s_inode[fp->s_ninode++] = ino;
	fp->s_fmod = 1;
}

getfs(dev)
{
	register struct mount *p;

	for(p = &mount[0]; p < &mount[NMOUNT]; p++)
		if(p->m_bufp != NULL && p->m_dev == dev) {
			p = p->m_bufp->b_addr;
			return(p);
		}
	panic("no fs");
}

update()
{
	register struct inode *ip;
	register struct mount *mp;
	register *bp;

	if(updlock)
		return;
	updlock++;
	for(mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
		if(mp->m_bufp != NULL) {
			ip = mp->m_bufp->b_addr;
			if(ip->s_fmod==0 || ip->s_ilock!=0 ||
			   ip->s_flock!=0 || ip->s_ronly!=0)
				continue;
			bp = getblk(mp->m_dev, 1);
			ip->s_fmod = 0;
			ip->s_time[0] = time[0];
			ip->s_time[1] = time[1];
			bcopy(ip, bp->b_addr, 256);
			bwrite(bp);
		}
	for(ip = &inode[0]; ip < &inode[NINODE]; ip++)
		if((ip->i_flag&ILOCK) == 0) {
			ip->i_flag =| ILOCK;
			iupdat(ip);
			prele(ip);
		}
	updlock = 0;
	bflush(NODEV);
}

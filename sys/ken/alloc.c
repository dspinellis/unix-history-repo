#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/filsys.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/user.h"

iinit()
{
	int *cp, *bp;
	int i;

	bp = bread(ROOTDEV, 1);
	cp = getblk(NODEV);
	if(u.u_error)
		panic("iinit");
	for(i=0; i<512; i++)
		cp->b_addr[i] = bp->b_addr[i];
	brelse(bp);
	mount[0].m_bufp = cp;
	mount[0].m_dev = ROOTDEV;
	cp = cp->b_addr;
	cp->s_flock = 0;
	cp->s_ilock = 0;
	cp->s_ninode = 0;
	time[0] = cp->s_time[0];
	time[1] = cp->s_time[1];
}

alloc(dev)
{
	int bno, i, *fp;
	int *bp, *ip;

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
		fp->s_nfree = ip[0];
		for(i=0; i<100; i++)
			fp->s_free[i] = ip[i+1];
		brelse(bp);
		fp->s_flock = 0;
		wakeup(&fp->s_flock);
	}
	bp = getblk(dev, bno);
	clrbuf(bp);
	return(bp);
}

free(dev, bno)
{
	int i, *fp;
	int *bp, *ip;

	fp = getfs(dev);
	while(fp->s_flock)
		sleep(&fp->s_flock, PINOD);
	if(fp->s_nfree >= 100) {
		fp->s_flock++;
		bp = getblk(dev, bno);
		ip = bp->b_addr;
		ip[0] = fp->s_nfree;
		for(i=0; i<100; i++)
			ip[i+1] = fp->s_free[i];
		fp->s_nfree = 0;
		bwrite(bp);
		fp->s_flock = 0;
		wakeup(&fp->s_flock);
	}
	fp->s_free[fp->s_nfree++] = bno;
}

ialloc(dev)
{
	int *fp, *bp, *ip;
	int i, j, k, ino;

	fp = getfs(dev);
	while(fp->s_ilock)
		sleep(&fp->s_ilock, PINOD);
loop:
	if(fp->s_ninode > 0) {
		ino = fp->s_inode[--fp->s_ninode];
		ip = iget(dev, ino);
		if(ip->i_mode == 0)
			return(ip);
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
				continue;
			fp->s_inode[fp->s_ninode++] = ino;
			if(fp->s_ninode >= 100)
				break;
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
	int *fp;

	fp = getfs(dev);
	if(fp->s_ilock)
		return;
	if(fp->s_ninode >= 100)
		return;
	fp->s_inode[fp->s_ninode++] = ino;
}

getfs(dev)
{
	int i, *p;

	for(i=0; i<NMOUNT; i++)
	if(mount[i].m_bufp != NULL && mount[i].m_dev == dev) {
		p = mount[i].m_bufp->b_addr;
		p->s_fmod = 1;
		return(p);
	}
	panic("no fs");
	return(NULL);
}

update()
{
	int i, j, *p, *q, *bp;
	static lock;

	if(lock)
		return;
	lock++;
	for(i=0; i<NMOUNT; i++)
	if(mount[i].m_bufp != NULL) {
		p = mount[i].m_bufp->b_addr;
		if(p->s_fmod==0 || p->s_ilock!=0 || p->s_flock!=0)
			continue;
		bp = getblk(mount[i].m_dev, 1);
		p->s_fmod = 0;
		p->s_time[0] = time[0];
		p->s_time[1] = time[1];
		q = bp->b_addr;
		for(j=0; j<256; j++)
			*q++ = *p++;
		bwrite(bp);
	}
	for(i=0; i<NINODE; i++) {
		p = &inode[i];
		if((p->i_flag&ILOCK) == 0) {
			p->i_flag =| ILOCK;
			iupdat(p);
			p->i_flag =& ~ILOCK;
			if(p->i_flag&IWANT)
				wakeup(p);
		}
	}
	lock = 0;
	bflush(NODEV);
}

/*	ufs_inode.c	3.2	%H%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mount.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/ino.h"
#include "../h/filsys.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/inline.h"

#define	INOHSZ	63
#define	INOHASH(dev,ino)	(((dev)+(ino))%INOHSZ)
short	inohash[INOHSZ];
short	ifreel;

/*
 * Initialize hash links for inodes
 * and build inode free list.
 */
ihinit()
{
	register int i;

	ifreel = 0;
	for (i = 0; i < NINODE - 1; i++)
		inode[i].i_hlink = i+1;
	inode[NINODE - 1].i_hlink = -1;
	for (i = 0; i < INOHSZ; i++)
		inohash[i] = -1;
}

/*
 * Find an inode if it is incore.
 * This is the equivalent, for inodes,
 * of ``incore'' in bio.c or ``pfind'' in subr.c.
 */
struct inode *
ifind(dev, ino)
dev_t dev;
ino_t ino;
{
	register struct inode *ip;

	for (ip = &inode[inohash[INOHASH(dev,ino)]]; ip != &inode[-1];
	    ip = &inode[ip->i_hlink])
		if (ino==ip->i_number && dev==ip->i_dev)
			return (ip);
	return ((struct inode *)0);
}

/*
 * Look up an inode by device,inumber.
 * If it is in core (in the inode structure),
 * honor the locking protocol.
 * If it is not in core, read it in from the
 * specified device.
 * If the inode is mounted on, perform
 * the indicated indirection.
 * In all cases, a pointer to a locked
 * inode structure is returned.
 *
 * printf warning: no inodes -- if the inode
 *	structure is full
 * panic: no imt -- if the mounted file
 *	system is not in the mount table.
 *	"cannot happen"
 */
struct inode *
iget(dev, ino)
dev_t dev;
ino_t ino;
{
	register struct inode *ip;
	register struct mount *mp;
	register struct buf *bp;
	register struct dinode *dp;
	register int slot;

loop:
	slot = INOHASH(dev, ino);
	ip = &inode[inohash[slot]];
	while (ip != &inode[-1]) {
		if(ino == ip->i_number && dev == ip->i_dev) {
			if((ip->i_flag&ILOCK) != 0) {
				ip->i_flag |= IWANT;
				sleep((caddr_t)ip, PINOD);
				goto loop;
			}
			if((ip->i_flag&IMOUNT) != 0) {
				for(mp = &mount[0]; mp < &mount[NMOUNT]; mp++)
				if(mp->m_inodp == ip) {
					dev = mp->m_dev;
					ino = ROOTINO;
					goto loop;
				}
				panic("no imt");
			}
			ip->i_count++;
			ip->i_flag |= ILOCK;
			return(ip);
		}
		ip = &inode[ip->i_hlink];
	}
	if(ifreel < 0) {
		printf("Inode table overflow\n");
		u.u_error = ENFILE;
		return(NULL);
	}
	ip = &inode[ifreel];
	ifreel = ip->i_hlink;
	ip->i_hlink = inohash[slot];
	inohash[slot] = ip - inode;
	ip->i_dev = dev;
	ip->i_number = ino;
	ip->i_flag = ILOCK;
	ip->i_count++;
	ip->i_un.i_lastr = 0;
	bp = bread(dev, itod(ino));
	/*
	 * Check I/O errors
	 */
	if((bp->b_flags&B_ERROR) != 0) {
		brelse(bp);
		iput(ip);
		return(NULL);
	}
	dp = bp->b_un.b_dino;
	dp += itoo(ino);
	iexpand(ip, dp);
	brelse(bp);
	return(ip);
}

iexpand(ip, dp)
register struct inode *ip;
register struct dinode *dp;
{
	register char *p1, *p2;
	register int i;

	ip->i_mode = dp->di_mode;
	ip->i_nlink = dp->di_nlink;
	ip->i_uid = dp->di_uid;
	ip->i_gid = dp->di_gid;
	ip->i_size = dp->di_size;
	p1 = (char *)ip->i_un.i_addr;
	p2 = (char *)dp->di_addr;
	for(i=0; i<NADDR; i++) {
		*p1++ = *p2++;
		*p1++ = *p2++;
		*p1++ = *p2++;
		*p1++ = 0;
	}
}

/*
 * Decrement reference count of
 * an inode structure.
 * On the last reference,
 * write the inode out and if necessary,
 * truncate and deallocate the file.
 */
iput(ip)
register struct inode *ip;
{
	register int i, x;
	register struct inode *jp;

	if(ip->i_count == 1) {
		ip->i_flag |= ILOCK;
		if(ip->i_nlink <= 0) {
			itrunc(ip);
			ip->i_mode = 0;
			ip->i_flag |= IUPD|ICHG;
			ifree(ip->i_dev, ip->i_number);
		}
		IUPDAT(ip, &time, &time);
		prele(ip);
		i = INOHASH(ip->i_dev, ip->i_number);
		x = ip - inode;
		if (inohash[i] == x) {
			inohash[i] = ip->i_hlink;
		} else {
			for (jp = &inode[inohash[i]]; jp != &inode[-1];
			    jp = &inode[jp->i_hlink])
				if (jp->i_hlink == x) {
					jp->i_hlink = ip->i_hlink;
					goto done;
				}
			panic("iput");
		}
done:
		ip->i_hlink = ifreel;
		ifreel = x;
		ip->i_flag = 0;
		ip->i_number = 0;
	} else
		prele(ip);
	ip->i_count--;
}

/*
 * Check accessed and update flags on
 * an inode structure.
 * If any is on, update the inode
 * with the current time.
 */
iupdat(ip, ta, tm)
register struct inode *ip;
time_t *ta, *tm;
{
	register struct buf *bp;
	struct dinode *dp;
	register char *p1, *p2;
	register int i;

	if((ip->i_flag&(IUPD|IACC|ICHG)) != 0) {
		if(getfs(ip->i_dev)->s_ronly)
			return;
		bp = bread(ip->i_dev, itod(ip->i_number));
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			return;
		}
		dp = bp->b_un.b_dino;
		dp += itoo(ip->i_number);
		dp->di_mode = ip->i_mode;
		dp->di_nlink = ip->i_nlink;
		dp->di_uid = ip->i_uid;
		dp->di_gid = ip->i_gid;
		dp->di_size = ip->i_size;
		p1 = (char *)dp->di_addr;
		p2 = (char *)ip->i_un.i_addr;
		for(i=0; i<NADDR; i++) {
			*p1++ = *p2++;
			*p1++ = *p2++;
			*p1++ = *p2++;
			if(*p2++ != 0 && (ip->i_mode&IFMT)!=IFMPC
			   && (ip->i_mode&IFMT)!=IFMPB)
				printf("iaddress > 2^24\n");
		}
		if(ip->i_flag&IACC)
			dp->di_atime = *ta;
		if(ip->i_flag&IUPD)
			dp->di_mtime = *tm;
		if(ip->i_flag&ICHG)
			dp->di_ctime = time;
		ip->i_flag &= ~(IUPD|IACC|ICHG);
		bdwrite(bp);
	}
}

/*
 * Free all the disk blocks associated
 * with the specified inode structure.
 * The blocks of the file are removed
 * in reverse order. This FILO
 * algorithm will tend to maintain
 * a contiguous free list much longer
 * than FIFO.
 */
itrunc(ip)
register struct inode *ip;
{
	register i;
	dev_t dev;
	daddr_t bn;

	if (ip->i_vfdcnt)
		panic("itrunc");
	i = ip->i_mode & IFMT;
	if (i!=IFREG && i!=IFDIR)
		return;
	dev = ip->i_dev;
	for(i=NADDR-1; i>=0; i--) {
		bn = ip->i_un.i_addr[i];
		if(bn == (daddr_t)0)
			continue;
		ip->i_un.i_addr[i] = (daddr_t)0;
		switch(i) {

		default:
			free(dev, bn);
			break;

		case NADDR-3:
			tloop(dev, bn, 0, 0);
			break;

		case NADDR-2:
			tloop(dev, bn, 1, 0);
			break;

		case NADDR-1:
			tloop(dev, bn, 1, 1);
		}
	}
	ip->i_size = 0;
	ip->i_flag |= ICHG|IUPD;
}

tloop(dev, bn, f1, f2)
dev_t dev;
daddr_t bn;
{
	register i;
	register struct buf *bp;
	register daddr_t *bap;
	daddr_t nb;

	bp = NULL;
	for(i=NINDIR-1; i>=0; i--) {
		if(bp == NULL) {
			bp = bread(dev, bn);
			if (bp->b_flags & B_ERROR) {
				brelse(bp);
				return;
			}
			bap = bp->b_un.b_daddr;
		}
		nb = bap[i];
		if(nb == (daddr_t)0)
			continue;
		if(f1) {
			brelse(bp);
			bp = NULL;
			tloop(dev, nb, f2, 0);
		} else
			free(dev, nb);
	}
	if(bp != NULL)
		brelse(bp);
	free(dev, bn);
}

/*
 * Make a new file.
 */
struct inode *
maknode(mode)
{
	register struct inode *ip;

	ip = ialloc(u.u_pdir->i_dev);
	if(ip == NULL) {
		iput(u.u_pdir);
		return(NULL);
	}
	ip->i_flag |= IACC|IUPD|ICHG;
	if((mode&IFMT) == 0)
		mode |= IFREG;
	ip->i_mode = mode & ~u.u_cmask;
	ip->i_nlink = 1;
	ip->i_uid = u.u_uid;
	ip->i_gid = u.u_gid;
	wdir(ip);
	return(ip);
}

/*
 * Write a directory entry with
 * parameters left as side effects
 * to a call to namei.
 */
wdir(ip)
struct inode *ip;
{

	u.u_dent.d_ino = ip->i_number;
	bcopy((caddr_t)u.u_dbuf, (caddr_t)u.u_dent.d_name, DIRSIZ);
	u.u_count = sizeof(struct direct);
	u.u_segflg = 1;
	u.u_base = (caddr_t)&u.u_dent;
	writei(u.u_pdir);
	iput(u.u_pdir);
}

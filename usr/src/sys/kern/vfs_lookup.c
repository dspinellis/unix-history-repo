/*	vfs_lookup.c	3.2	%H%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/mount.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"

/*
 * Convert a pathname into a pointer to
 * an inode. Note that the inode is locked.
 *
 * func = function called to get next char of name
 *	&uchar if name is in user space
 *	&schar if name is in system space
 * flag = 0 if name is sought
 *	1 if name is to be created
 *	2 if name is to be deleted
 */
struct inode *
namei(func, flag)
int (*func)();
{
	register struct inode *dp;
	register c;
	register char *cp;
	struct buf *bp;
	register struct direct *ep;
	int i;
	dev_t d;
	off_t eo;

	/*
	 * If name starts with '/' start from
	 * root; otherwise start from current dir.
	 */

	dp = u.u_cdir;
	if((c=(*func)()) == '/')
		if ((dp = u.u_rdir) == NULL)
			dp = rootdir;
	(void) iget(dp->i_dev, dp->i_number);
	while(c == '/')
		c = (*func)();
	if(c == '\0' && flag != 0)
		u.u_error = ENOENT;

cloop:
	/*
	 * Here dp contains pointer
	 * to last component matched.
	 */

	if(u.u_error)
		goto out;
	if(c == '\0')
		return(dp);

	/*
	 * If there is another component,
	 * Gather up name into
	 * users' dir buffer.
	 */

	cp = &u.u_dbuf[0];
	while (c != '/' && c != '\0' && u.u_error == 0 ) {
		if (mpxip!=NULL && c=='!')
			break;
		if (cp < &u.u_dbuf[DIRSIZ])
			*cp++ = c;
		c = (*func)();
	}
	while(cp < &u.u_dbuf[DIRSIZ])
		*cp++ = '\0';
	while(c == '/')
		c = (*func)();
	if (c == '!' && mpxip != NULL) {
		iput(dp);
		plock(mpxip);
		mpxip->i_count++;
		return(mpxip);
	}

seloop:
	/*
	 * dp must be a directory and
	 * must have X permission.
	 */

	if((dp->i_mode&IFMT) != IFDIR)
		u.u_error = ENOTDIR;
	(void) access(dp, IEXEC);
	if(u.u_error)
		goto out;

	/*
	 * set up to search a directory
	 */
	u.u_offset = 0;
	u.u_segflg = 1;
	eo = 0;
	bp = NULL;

eloop:

	/*
	 * If at the end of the directory,
	 * the search failed. Report what
	 * is appropriate as per flag.
	 */

	if(u.u_offset >= dp->i_size) {
		if(bp != NULL)
			brelse(bp);
		if(flag==1 && c=='\0') {
			if(access(dp, IWRITE))
				goto out;
			u.u_pdir = dp;
			if(eo)
				u.u_offset = eo-sizeof(struct direct);
			else
				dp->i_flag |= IUPD|ICHG;
			return(NULL);
		}
		u.u_error = ENOENT;
		goto out;
	}

	/*
	 * If offset is on a block boundary,
	 * read the next directory block.
	 * Release previous if it exists.
	 */

	if((u.u_offset&BMASK) == 0) {
		if(bp != NULL)
			brelse(bp);
		bp = bread(dp->i_dev,
			bmap(dp, (daddr_t)(u.u_offset>>BSHIFT), B_READ));
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			goto out;
		}
		ep = (struct direct *)bp->b_un.b_addr;
	} else
		ep++;

	/*
	 * Note first empty directory slot
	 * in eo for possible creat.
	 * String compare the directory entry
	 * and the current component.
	 * If they do not match, go back to eloop.
	 */

	u.u_offset += sizeof(struct direct);
	if(ep->d_ino == 0) {
		if(eo == 0)
			eo = u.u_offset;
		goto eloop;
	}
	for(i=0; i<DIRSIZ; i++) {
		if(u.u_dbuf[i] != ep->d_name[i])
			goto eloop;
		if(u.u_dbuf[i] == 0)
			break;
	}

	/*
	 * Here a component matched in a directory.
	 * If there is more pathname, go back to
	 * cloop, otherwise return.
	 */
	bcopy((caddr_t)ep, (caddr_t)&u.u_dent, sizeof(struct direct));
	if(bp != NULL)
		brelse(bp);
	if(flag==2 && c=='\0') {
		if(access(dp, IWRITE))
			goto out;
		return(dp);
	}
	d = dp->i_dev;
	if(u.u_dent.d_ino == ROOTINO)
	if(dp->i_number == ROOTINO)
	if(u.u_dent.d_name[1] == '.')
		for(i=1; i<NMOUNT; i++)
			if(mount[i].m_bufp != NULL)
			if(mount[i].m_dev == d) {
				iput(dp);
				dp = mount[i].m_inodp;
				dp->i_count++;
				plock(dp);
				goto seloop;
			}
	iput(dp);
	dp = iget(d, u.u_dent.d_ino);
	if(dp == NULL)
		return(NULL);
	goto cloop;

out:
	iput(dp);
	return(NULL);
}

/*
 * Return the next character from the
 * kernel string pointed at by dirp.
 */
schar()
{

	return(*u.u_dirp++ & 0377);
}

/*
 * Return the next character from the
 * user string pointed at by dirp.
 */
uchar()
{
	register c;

	c = fubyte(u.u_dirp++);
	if(c == -1)
		u.u_error = EFAULT;
	return(c);
}

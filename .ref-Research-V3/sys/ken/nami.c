#include "/sys/nsys/param.h"
#include "/sys/nsys/inode.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/buf.h"

namei(func, flag)
int (*func)();
int flag;

/*
 * func = function called to get name
 *	&uchar if name is in user space
 *	&schar if name is in system space
 * flag = 0 if name is saught
 *	1 if name is to be created
 *	2 if name is to be deleted
 * return is incremented locked inode.
 *	NULL if name not found
 */

{
	struct inode *dp;
	int *bp;
	int i, j, c, eo;

	/*
	 * start from indicated
	 * directory
	 */

	if((c=(*func)()) == '/')
		dp = rootdir; else
		dp = u.u_cdir;
	iget(dp->i_dev, dp->i_number);
	while(c == '/')
		c = (*func)();

cloop:
	/*
	 * here dp contains pointer
	 * to last component matched.
	 */

	if(u.u_error)
		goto out;
	if(c == '\0')
		return(dp);

	/*
	 * if there is another component,
	 * must be a directory and
	 * must have x permission
	 */

	if((dp->i_mode&IFMT) != IFDIR) {
		u.u_error = ENOTDIR;
		goto out;
	}

	if(access(dp, IEXEC))
		goto out;

	/*
	 * gather up name into
	 * users' dir buffer
	 */

	for(i=0; c!='/' && c!='\0' && u.u_error==0; i++) {
		if(i < DIRSIZ)
			u.u_dbuf[i] = c;
		c = (*func)();
	}
	while(i < DIRSIZ)
		u.u_dbuf[i++] = '\0';
	while(c == '/')
		c = (*func)();
	if(u.u_error)
		goto out;

	/*
	 * search the directory
	 */

	u.u_offset[1] = 0;
	u.u_offset[0] = 0;
	u.u_segflg = 1;
	eo = 0;
	u.u_count = ldiv(0, dp->i_size1, 16);
	u.u_offset[1] = 0;
	bp = NULL;

eloop:
	i = lrem(0, u.u_offset[1], 512);
	if(u.u_count == 0) {
		if(bp != NULL)
			brelse(bp);
		if(flag==1 && c=='\0') {
			if(access(dp, IWRITE))
				goto out;
			u.u_pdir = dp;
			if(eo)
				u.u_offset[1] = eo-DIRSIZ-2; else
				dp->i_flag =| IUPD;
			return(NULL);
		}
		u.u_error = ENOENT;
		goto out;
	}
	if(i == 0) {
		if(bp != NULL)
			brelse(bp);
		bp = bread(dp->i_dev,
			bmap(dp, ldiv(0, u.u_offset[1], 512)));
	}
	for(j = -2; j<DIRSIZ; j++)
		u.u_dent.u_name[j] =
			bp->b_addr[i++];
	u.u_offset[1] =+ DIRSIZ+2;
	u.u_count--;
	if(u.u_dent.u_ino == 0) {
		if(eo == 0)
			eo = u.u_offset[1];
		goto eloop;
	}
	for(i=0; i<DIRSIZ; i++)
		if(u.u_dbuf[i] != u.u_dent.u_name[i])
			goto eloop;
	if(bp != NULL)
		brelse(bp);
	if(flag==2 && c=='\0') {
		if(access(dp, IWRITE))
			goto out;
		return(dp);
	}
	i = dp->i_dev;
	iput(dp);
	dp = iget(i, u.u_dent.u_ino);
	if(dp == NULL)
		return(NULL);
	goto cloop;

out:
	iput(dp);
	return(NULL);
}

schar()
{

	return(*u.u_dirp++ & 0377);
}

uchar()
{
	int c;

	c = fubyte(u.u_dirp++);
	if(c == -1)
		u.u_error = EFAULT;
	return(c);
}

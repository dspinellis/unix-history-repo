#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../inode.h"
#include "../user.h"
#include "../systm.h"
#include "../buf.h"

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
	register struct inode *dp;
	register c;
	register char *cp;
	int eo, *bp;

	/*
	 * start from indicated
	 * directory
	 */

	dp = u.u_cdir;
	if((c=(*func)()) == '/')
		dp = rootdir;
	iget(dp->i_dev, dp->i_number);
	while(c == '/')
		c = (*func)();
	if(c == '\0' && flag != 0) {
		u.u_error = ENOENT;
		goto out;
	}

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
	 * dp must be a directory and
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

	cp = &u.u_dbuf[0];
	while(c!='/' && c!='\0' && u.u_error==0) {
		if(cp < &u.u_dbuf[DIRSIZ])
			*cp++ = c;
		c = (*func)();
	}
	while(cp < &u.u_dbuf[DIRSIZ])
		*cp++ = '\0';
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
	u.u_count = ldiv(dp->i_size1, DIRSIZ+2);
	bp = NULL;

eloop:
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
	if((u.u_offset[1]&0777) == 0) {
		if(bp != NULL)
			brelse(bp);
		bp = bread(dp->i_dev,
			bmap(dp, ldiv(u.u_offset[1], 512)));
	}
	bcopy(bp->b_addr+(u.u_offset[1]&0777), &u.u_dent, (DIRSIZ+2)/2);
	u.u_offset[1] =+ DIRSIZ+2;
	u.u_count--;
	if(u.u_dent.u_ino == 0) {
		if(eo == 0)
			eo = u.u_offset[1];
		goto eloop;
	}
	for(cp = &u.u_dbuf[0]; cp < &u.u_dbuf[DIRSIZ]; cp++)
		if(*cp != cp[u.u_dent.u_name - u.u_dbuf])
			goto eloop;
	if(bp != NULL)
		brelse(bp);
	if(flag==2 && c=='\0') {
		if(access(dp, IWRITE))
			goto out;
		return(dp);
	}
	bp = dp->i_dev;
	iput(dp);
	dp = iget(bp, u.u_dent.u_ino);
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
	register c;

	c = fubyte(u.u_dirp++);
	if(c == -1)
		u.u_error = EFAULT;
	return(c);
}

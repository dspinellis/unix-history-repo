#include <retrofit.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * Discover the teletype letter of the passed file
 * descriptor.  C library.
 */


#define	BLKSIZE 32			/* Directory entries per disk block */

ttyn(tty)
{
	struct dirent {
		int	d_ino;
		char	d_name[14];
	};
	struct dirent buf[BLKSIZE];
	struct stat sbuf;
	register int fi, inum;
	register struct dirent *dp;

	if (fstat(tty, &sbuf) < 0)
		return('x');
	fi = open("/dev", 0);
	if (fi < 0)
		return('x');
	inum = sbuf.st_ino;
	while (read(fi, &buf, sizeof buf) > 0)
		for (dp = &buf[0]; dp < &buf[BLKSIZE]; dp++)
			if (dp->d_ino == inum) {
				close(fi);
				return(dp->d_name[3]);
			}
	close(fi);
	return('x');
}

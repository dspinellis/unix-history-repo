/*	@(#)dirdmp.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.2 Lachman ONC Test Suite source
 */

#include <sys/param.h>
#ifndef major
#include <sys/types.h>
#endif
#ifdef SVR3
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <stdio.h>
#include <ctype.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	argv++;
	argc--;
	while (argc--) {
		print(*argv++);
	}
}

print(dir)
	char *dir;
{
	DIR *dirp;
#ifdef SVR3
	struct dirent *dp;
#else
	struct direct *dp;
#endif
	int rec = 0;

	dirp = opendir(dir);
	if (dirp == NULL) {
		perror(dir);
		return;
	}
	while ((dp = readdir(dirp)) != NULL) ;
	closedir(dirp);
}

#include <sys/stat.h>

/*
 * open a directory.
 */
DIR *
opendir(name)
	char *name;
{
	register DIR *dirp;
	register int fd;
	struct stat sb;
	extern char *malloc();

	if ((fd = open(name, 0)) == -1) {
		printf("open failed\n");
		return (NULL);
	}
	if (fstat(fd, &sb) == -1) {
		printf("stat failed\n");
		return (NULL);
	}
	/*
	if ((sb.st_mode & S_IFMT) != S_IFDIR) {
		printf("not a directory\n");
		return (NULL);
	}
	*/
	printf("%s mode %o dir %o\n", name, sb.st_mode, S_IFDIR);
	if (((dirp = (DIR *)malloc(sizeof(DIR))) == NULL) ||
#ifdef SVR3
	    ((dirp->dd_buf = malloc(DIRBUF)) == NULL)) {
#else
	    ((dirp->dd_buf = malloc((int)sb.st_blksize)) == NULL)) {
#endif
		if (dirp) {
			if (dirp->dd_buf) {
				free(dirp->dd_buf);
			}
			free(dirp);
		}
		close(fd);
		return (NULL);
	}
#ifndef SVR3
	dirp->dd_bsize = sb.st_blksize;
	dirp->dd_bbase = 0;
	dirp->dd_entno = 0;
#endif
	dirp->dd_fd = fd;
	dirp->dd_loc = 0;
	return (dirp);
}

/*
 * get next entry in a directory.
 */
#ifdef SVR3
struct dirent *
#else
struct direct *
#endif
readdir(dirp)
	register DIR *dirp;
{
#ifdef SVR3
	register struct dirent *dp;
#else
	register struct direct *dp;
#endif

	for (;;) {
		if (dirp->dd_loc == 0) {
#ifdef SVR3
			dirp->dd_size = getdents(dirp->dd_fd,
			    dirp->dd_buf, DIRBUF);
#else
			dirp->dd_size = getdirentries(dirp->dd_fd,
			    dirp->dd_buf, dirp->dd_bsize, &dirp->dd_bbase);
#endif
			if (dirp->dd_size <= 0) {
				printf("EOF\n");
				return (NULL);
			}
#ifndef SVR3
			dirp->dd_entno = 0;
#endif
		}
		if (dirp->dd_loc >= dirp->dd_size) {
			printf("EOB offset %d\n", tell(dirp->dd_fd));
			dirp->dd_loc = 0;
			continue;
		}
#ifdef SVR3
		dp = (struct dirent *)(dirp->dd_buf + dirp->dd_loc);
#else
		dp = (struct direct *)(dirp->dd_buf + dirp->dd_loc);
#endif
		if (dp->d_reclen <= 0) {
			printf("0 reclen\n");
			return (NULL);
		}
		dirp->dd_loc += dp->d_reclen;
#ifndef SVR3
		dirp->dd_entno++;
#endif
#ifdef SVR3
		printf("%5d %5ld %5d %s\n",
		    dirp->dd_loc, dp->d_ino, dp->d_reclen,
		    dp->d_name);
#else
		printf("%5d %5d %5d %5d %s\n",
		    dirp->dd_loc, dp->d_fileno, dp->d_reclen,
		    dp->d_namlen, dp->d_name);
#endif
#ifdef SVR3
		if (dp->d_ino == 0) {
#else
		if (dp->d_fileno == 0) {
#endif
			continue;
		}
		return (dp);
	}
}

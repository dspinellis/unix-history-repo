#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)ttyname.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * ttyname(f): return "/dev/ttyXX" which the the name of the
 * tty belonging to file f.
 *  NULL if it is not a tty
 */

#define	NULL	0
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/stat.h>

static	char	dev[]	= "/dev/";
char	*strcpy();
char	*strcat();

char *
ttyname(f)
{
	struct stat fsb;
	struct stat tsb;
	register struct direct *db;
	register DIR *df;
	static char rbuf[32];

	if (isatty(f)==0)
		return(NULL);
	if (fstat(f, &fsb) < 0)
		return(NULL);
	if ((fsb.st_mode&S_IFMT) != S_IFCHR)
		return(NULL);
	if ((df = opendir(dev)) == NULL)
		return(NULL);
	while ((db = readdir(df)) != NULL) {
		if (db->d_ino != fsb.st_ino)
			continue;
		strcpy(rbuf, dev);
		strcat(rbuf, db->d_name);
		if (stat(rbuf, &tsb) < 0)
			continue;
		if (tsb.st_dev == fsb.st_dev && tsb.st_ino == fsb.st_ino) {
			closedir(df);
			return(rbuf);
		}
	}
	closedir(df);
	return(NULL);
}

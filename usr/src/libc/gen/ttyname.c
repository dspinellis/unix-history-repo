/* @(#)ttyname.c	4.1 (Berkeley) 12/21/80 */
/*
 * ttyname(f): return "/dev/ttyXX" which the the name of the
 * tty belonging to file f.
 *  NULL if it is not a tty
 */

#define	NULL	0
#include <sys/types.h>
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
	struct direct db;
	static char rbuf[32];
	register df;

	if (isatty(f)==0)
		return(NULL);
	if (fstat(f, &fsb) < 0)
		return(NULL);
	if ((fsb.st_mode&S_IFMT) != S_IFCHR)
		return(NULL);
	if ((df = open(dev, 0)) < 0)
		return(NULL);
	while (read(df, (char *)&db, sizeof(db)) == sizeof(db)) {
		if (db.d_ino == 0)
			continue;
		if (db.d_ino != fsb.st_ino)
			continue;
		strcpy(rbuf, dev);
		strcat(rbuf, db.d_name);
		if (stat(rbuf, &tsb) < 0)
			continue;
		if (tsb.st_dev==fsb.st_dev && tsb.st_ino==fsb.st_ino) {
			close(df);
			return(rbuf);
		}
	}
	close(df);
	return(NULL);
}

# include	"../hdr/defines.h"
# include	<dir.h>

SCCSID(@(#)dofile.c	1.4);

int	nfiles;
char	had_dir;
char	had_standinp;


do_file(p,func)
register char *p;
int (*func)();
{
	extern char *Ffile;
	char str[FILESIZE];
	char ibuf[FILESIZE];
	DIR *dir;
	struct direct *dp;
	register char *s;
	int fd;

	if (p[0] == '-') {
		had_standinp = 1;
		while (gets(ibuf) != NULL) {
			if (sccsfile(ibuf)) {
				Ffile = ibuf;
				(*func)(ibuf);
				nfiles++;
			}
		}
	}
	else if (exists(p) && (Statbuf.st_mode & S_IFMT) == S_IFDIR) {
		had_dir = 1;
		Ffile = p;
		if((dir = opendir(p)) == 0)
			return;
		(void) readdir(dir);	/* skip . */
		(void) readdir(dir);	/* and .. */
		while (dp = readdir(dir)) {
			sprintf(str,"%s/%s", p, dp->d_name);
			if(sccsfile(str)) {
				Ffile = str;
				(*func)(str);
				nfiles++;
			}
		}
		closedir(dir);
	}
	else {
		Ffile = p;
		(*func)(p);
		nfiles++;
	}
}

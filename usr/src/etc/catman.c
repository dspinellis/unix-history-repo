#ifndef lint
static char *sccsid = "@(#)catman.c	4.5 (Berkeley) 7/1/83";
#endif

/*
 * catman: update cat'able versions of manual pages
 *  (whatis database also)
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/dir.h>
#include <ctype.h>

#define	SYSTEM(str)	(pflag ? printf("%s\n", str) : system(str))

char	buf[BUFSIZ];
char	pflag;
char	nflag;
char	wflag;
char	man[MAXNAMLEN+6] = "manx/";
char	cat[MAXNAMLEN+6] = "catx/";
char	*rindex();

main(ac, av)
	int ac;
	char *av[];
{
	register char *msp, *csp, *sp;
	register char *sections;
	register int exstat = 0;
	register char changed = 0;

	while (ac > 1) {
		av++;
		if (strcmp(*av, "-p") == 0)
			pflag++;
		else if (strcmp(*av, "-n") == 0)
			nflag++;
		else if (strcmp(*av, "-w") == 0)
			wflag++;
		else if (*av[0] == '-')
			goto usage;
		else
			break;
		ac--;
	}
	if (ac == 2)
		sections = *av;
	else if (ac < 2)
		sections = "12345678";
	else {
usage:
		printf("usage: catman [ -p ] [ -n ] [ -w ] [ sections ]\n");
		exit(-1);
	}
	if (wflag)
		goto whatis;
	chdir("/usr/man");
	msp = &man[5];
	csp = &cat[5];
	umask(0);
	for (sp = sections; *sp; sp++) {
		register DIR *mdir;
		register struct direct *dir;
		struct stat sbuf;

		man[3] = cat[3] = *sp;
		*msp = *csp = '\0';
		if ((mdir = opendir(man)) == NULL) {
			fprintf(stderr, "opendir:");
			perror(man);
			exstat = 1;
			continue;
		}
		if (stat(cat, &sbuf) < 0) {
			char buf[MAXNAMLEN + 6], *cp, *rindex();

			strcpy(buf, cat);
			cp = rindex(buf, '/');
			if (cp && cp[1] == '\0')
				*cp = '\0';
			if (pflag)
				printf("mkdir %s\n", buf);
			else if (mkdir(buf, 0777) < 0) {
				sprintf(buf, "catman: mkdir: %s", cat);
				perror(buf);
				continue;
			}
			stat(cat, &sbuf);
		}
		if ((sbuf.st_mode & 0777) != 0777)
			chmod(cat, 0777);
		while ((dir = readdir(mdir)) != NULL) {
			time_t time;
			char *tsp;
			FILE *inf;

			if (dir->d_ino == 0 || dir->d_name[0] == '.')
				continue;
			/*
			 * Make sure this is a man file, i.e., that it
			 * ends in .[0-9] or .[0-9][a-z]
			 */
			tsp = rindex(dir->d_name, '.');
			if (tsp == NULL)
				continue;
			if (!isdigit(*++tsp))
				continue;
			if (*++tsp && !isalpha(*tsp))
				continue;
			if (*tsp && *++tsp)
				continue;
			strcpy(msp, dir->d_name);
			if ((inf = fopen(man, "r")) == NULL) {
				perror(man);
				exstat = 1;
				continue;
			}
			if (getc(inf) == '.' && getc(inf) == 's'
			    && getc(inf) == 'o') {
				fclose(inf);
				continue;
			}
			fclose(inf);
			strcpy(csp, dir->d_name);
			if (stat(cat, &sbuf) >= 0) {
				time = sbuf.st_mtime;
				stat(man, &sbuf);
				if (time >= sbuf.st_mtime)
					continue;
				unlink(cat);
			}
			sprintf(buf, "nroff -man %s > %s", man, cat);
			SYSTEM(buf);
			changed = 1;
		}
		closedir(mdir);
	}
	if (changed && !nflag) {
whatis:
		if (pflag)
			printf("/bin/sh /usr/lib/makewhatis\n");
		else {
			execl("/bin/sh", "/bin/sh", "/usr/lib/makewhatis", 0);
			perror("/bin/sh /usr/lib/makewhatis");
			exstat = 1;
		}
	}
	exit(exstat);
}

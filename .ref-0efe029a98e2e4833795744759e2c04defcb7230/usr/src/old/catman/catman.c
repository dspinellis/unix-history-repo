/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)catman.c	5.8 (Berkeley) %G%";
#endif not lint

/*
 * catman: update cat'able versions of manual pages
 *  (whatis database also)
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/dir.h>
#include <ctype.h>

char	buf[BUFSIZ];
char	pflag;
char	nflag;
char	wflag;
char	man[MAXNAMLEN+6] = "manx/";
int	exstat = 0;
char	cat[MAXNAMLEN+6] = "catx/";
char	lncat[MAXNAMLEN+9] = "../catx/";
char	*manpath = "/usr/man";
char	*sections = "12345678ln";
char	*makewhatis = "/usr/lib/makewhatis";
char	*index(), *rindex();
char	*strcpy();
char	*getenv();

main(ac, av)
	int ac;
	char *av[];
{
	char *mp, *nextp;

	if ((mp = getenv("MANPATH")) != NULL)
		manpath = mp;

	ac--, av++;
	while (ac > 0 && av[0][0] == '-') {
		switch (av[0][1]) {

		case 'p':
			pflag++;
			break;

		case 'n':
			nflag++;
			break;

		case 'w':
			wflag++;
			break;

		case 'M':
		case 'P':
			if (ac < 1) {
				fprintf(stderr, "%s: missing path\n",
				    av[0]);
				exit(1);
			}
			ac--, av++;
			manpath = *av;
			break;

		default:
			goto usage;
		}
		ac--, av++;
	}
	if (ac > 1) {
usage:
		printf("usage: catman [ -p ] [ -n ] [ -w ] [ -M path ] [ sections ]\n");
		exit(-1);
	}
	if (ac == 1)
		sections = *av;
	for (mp = manpath; mp && ((nextp = index(mp, ':')), 1); mp = nextp) {
		if (nextp)
			*nextp++ = '\0';
		doit(mp);
	}
	exit(exstat);
}

doit(mandir)
	char *mandir;
{
	register char *msp, *csp, *sp;
	int changed = 0;
	int status;

	if (wflag)
		goto whatis;
	if (chdir(mandir) < 0) {
		sprintf(buf, "catman: %s", mandir);
		perror(buf);
		/* exstat = 1; */
		return;
	}
	if (pflag)
		printf("cd %s\n", mandir);
	msp = &man[5];
	csp = &cat[5];
	(void) umask(0);
	for (sp = sections; *sp; sp++) {
		register DIR *mdir;
		register struct direct *dir;
		struct stat sbuf;

		man[3] = cat[3] = *sp;
		*msp = *csp = '\0';
		if ((mdir = opendir(man)) == NULL) {
			sprintf(buf, "catman: opendir: %s", man);
			perror(buf);
			/* exstat = 1; */
			continue;
		}
		if (stat(cat, &sbuf) < 0) {
			register char *cp;

			(void) strcpy(buf, cat);
			cp = rindex(buf, '/');
			if (cp && cp[1] == '\0')
				*cp = '\0';
			if (pflag)
				printf("mkdir %s\n", buf);
			else if (mkdir(buf, 0777) < 0) {
				sprintf(buf, "catman: mkdir: %s", cat);
				perror(buf);
				exstat = 1;
				continue;
			}
			(void) stat(cat, &sbuf);
		}
		if (access(cat, R_OK|W_OK|X_OK) == -1) {
			sprintf(buf, "catman: %s", cat);
			perror(buf);
			exstat = 1;
			continue;
		}
		if ((sbuf.st_mode & S_IFMT) != S_IFDIR) {
			fprintf(stderr, "catman: %s: Not a directory\n", cat);
			exstat = 1;
			continue;
		}
		while ((dir = readdir(mdir)) != NULL) {
			time_t time;
			register char *tsp;
			FILE *inf;
			int  makelink;

			if (dir->d_ino == 0 || dir->d_name[0] == '.')
				continue;
			/*
			 * Make sure this is a man file, i.e., that it
			 * ends in .[0-9] or .[0-9][a-z]
			 */
			tsp = rindex(dir->d_name, '.');
			if (tsp == NULL)
				continue;
			if (!isdigit(*++tsp) && *tsp != *sp)
				continue;
			if (*++tsp && !isalpha(*tsp))
				continue;
			if (*tsp && *++tsp)
				continue;
			(void) strcpy(msp, dir->d_name);
			if ((inf = fopen(man, "r")) == NULL) {
				sprintf(buf, "catman: %s", man);
				perror(buf);
				exstat = 1;
				continue;
			}
			makelink = 0;
			if (getc(inf) == '.' && getc(inf) == 's'
			    && getc(inf) == 'o') {
				if (getc(inf) != ' ' ||
				    fgets(lncat+3, sizeof(lncat)-3, inf)==NULL) {
					fclose(inf);
					continue;
				}
				if (lncat[strlen(lncat)-1] == '\n')
					lncat[strlen(lncat)-1] = '\0';
				if (strncmp(lncat+3, "man", 3) != 0) {
					fclose(inf);
					continue;
				}
				bcopy("../cat", lncat, sizeof("../cat")-1);
				makelink = 1;
			}
			fclose(inf);
			(void) strcpy(csp, dir->d_name);
			if (stat(cat, &sbuf) >= 0) {
				time = sbuf.st_mtime;
				(void) stat(man, &sbuf);
				if (time >= sbuf.st_mtime)
					continue;
				(void) unlink(cat);
			}
			if (makelink) {
				/*
				 * Don't unlink a directory by accident.
				 */
				if (stat(lncat+3, &sbuf) >= 0 &&
				    (((sbuf.st_mode&S_IFMT)==S_IFREG) ||
				     ((sbuf.st_mode&S_IFMT)==S_IFLNK)))
					(void) unlink(cat);
				if (pflag)
					printf("ln -s %s %s\n", lncat, cat);
				else
					if (symlink(lncat, cat) == -1) {
						sprintf(buf, "catman: symlink: %s", cat);
						perror(buf);
						exstat = 1;
						continue;
					}
			}
			else {
				sprintf(buf, "nroff -man %s > %s", man, cat);
				if (pflag)
					printf("%s\n", buf);
				else if ((status = system(buf)) != 0) {
					fprintf(stderr, "catman: nroff: %s: exit status %d: Owooooo!\n", cat, status);
					exstat = 1;
					continue;
				}
			}
			changed = 1;
		}
		closedir(mdir);
	}
	if (changed && !nflag) {
whatis:
		sprintf(buf, "%s %s", makewhatis, mandir);
		if (pflag)
			printf("%s\n", buf);
		else if ((status = system(buf)) != 0) {
			fprintf(stderr, "catman: %s: exit status %d\n",
			    buf, status);
			exstat = 1;
		}
	}
	return;
}

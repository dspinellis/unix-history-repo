/*
 * Written by Michael Fischbein, currently with Sun Microsystems, Inc.
 * (sun!sunbow!msf)
 * 16 June 1989 18:41
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>
#include <dirent.h>
#include <sys/sysmacros.h>
#include <grp.h>
#include <pwd.h>
#include <time.h>
#include <ctype.h>

extern int getopt();
extern char *optarg;
extern int optind, opterr;
extern int stat(), lstat();

int     namecmp();		/* name comparison for qsort */
int     revnamecmp();
int     acccmp();		/* access time comparison for qsort */
int     revacccmp();
int     modcmp();		/* modify time comparixon for qsort */
int     revmodcmp();
int     statcmp();		/* status time comparixon for qsort */
int     revstatcmp();
int     printfancy();
int     prablelen();
char   *emalloc();

typedef struct lsstruct {
	char   *name;		/* pointer to filename */
	struct stat lstat;
}       lsstruct;


int	qflg, Aflg, Cflg, Fflg, Lflg, Rflg, Sflg;

/*
 * entry point for ls.
 * Parse options and call appropriate subroutines
 */
main(argc, argv)
int     argc;
char   *argv[];

{
	static char options[] = "aAcCdfFgilLqrRstu1";
	int     inch;		/* input character */

 /* set up defaults for terminal/nonterminal stdout */
	firsttimethruflag = 1;
	if (isatty(1)) {
		singlecolflag = 0;
		sortacrossflag = 0;
		nonprintflag++;
		lengthfcn = prablelen;
	} else {
		singlecolflag++;
	}

	if (getuid() == 0) {
		listallflag++;
	}
	while ((inch = getopt(argc, argv, options)) != -1) {
		switch (inch) {
		case 'a':
			listallflag++;
			listalwaysflag++;
			break;
		case 'A':
			listallflag++;
			break;
		case 'c':
			statustimeflag++;
			modtimeflag = 0;
			accesstimeflag = 0;
			break;
		case 'C':
			singlecolflag = 0;
			sortacrossflag = 0;
			break;
		case 'S':
			Sflg++; /* fall into... */
		case 'd':
			listdirflag++;
			break;
		case 'f':
			longformflag = 0;
			timesortflag = 0;
			sizeflag = 0;
			reversesortflag = 0;
			listallflag++;
			listalwaysflag++;
			specialdirflag++;
			break;
		case 'F':
			fancyflag++;
			break;
		case 'g':
			groupflag++;
			break;
		case 'i':
			inodeflag++;
			break;
		case 'l':
			longformflag++;
			numberflag = 0;
			singlecolflag = 0;
			statfcn = lstat;
			if (accesstimeflag == 0 && statustimeflag == 0) {
				modtimeflag++;
			}
			break;
		case 'L':
			statfcn = stat;
			break;
		case 'q':
			nonprintflag++;
			lengthfcn = prablelen;
			break;
		case 'r':
			reversesortflag++;
			break;
		case 'R':
			recursiveflag++;
			statfcn = lstat;
			break;
		case 's':
			sizeflag++;
			statfcn = lstat;
			break;
		case 't':
			timesortflag++;
			if (accesstimeflag == 0 && statustimeflag == 0) {
				modtimeflag++;
			}
			break;
		case 'u':
			accesstimeflag++;
			modtimeflag = 0;
			statustimeflag = 0;
			break;
		case '1':
			singlecolflag++;
			break;
		default:
		case '?':
			(void) fprintf(stderr, "Usage: %s [-%s] [file ...]\n", argv[0], options);
			exit(1);
		}
	}			/* end of option loop */
 /* do the work */
	if (argc > optind) {
		lsdir(argc - optind, &argv[optind]);
	} else {
		char   *defname = ".";

		lsdir(1, &defname);
	}
}

/*
 * list the files in a directory
 */


extern int errno;

int     lsdir(argc, argv)
int     argc;			/* count of file names passed */
char   *argv[];			/* array of file names */

{
	int     curname;
	int     goodcount = 0;
	lsstruct *stats;
	int     i;
	static char curpath[MAXPATHLEN + 1];
	char   *midpt;

 /* allocate memory to proceed. Lint complains, but emalloc is aligned */
	stats = (lsstruct *) emalloc((unsigned) argc * (sizeof(struct lsstruct)));

	for (curname = 0; curname < argc; ++curname) {
		if (!firsttimethruflag) {

		/*
		 * check for .xxx files. Note can't get listalways without
		 * listall being set
		 */
			if (argv[curname][0] == '.' && !listallflag) {
				continue;
			}
		/* and now for listalways: . and .. */
			if ((argv[curname][0] == '.' && argv[curname][1] == '\0') ||
					(argv[curname][0] == '.' && argv[curname][1] == '.' &&
					 argv[curname][2] == '\0')) {
				if (!listalwaysflag)
					continue;
			}
		}		/* end of not firsttimethru test */
		if (firsttimethruflag || longformflag || recursiveflag || timesortflag || sizeflag || inodeflag || fancyflag) {
			if (statfcn(argv[curname], &stats[goodcount].lstat) == -1) {
				if (errno == ENOENT) {
					(void) fprintf(stderr, "%s not found\n", argv[curname]);
					continue;
				} else {
					perror(argv[curname]);
					exit(1);
				}
			}	/* end of stat error check */
		}
		stats[goodcount].name = argv[curname];
		goodcount++;
	}			/* end of per name loop */

 /* sort the names */
	if (goodcount > 1 && !specialdirflag) {
		if (reversesortflag) {
			if (!timesortflag) {
				qsort((char *) stats, goodcount, sizeof(struct lsstruct), revnamecmp);
			} else {
				if (accesstimeflag) {
					qsort((char *) stats, goodcount, sizeof(struct lsstruct), revacccmp);
				} else {
					if (modtimeflag) {
						qsort((char *) stats, goodcount, sizeof(struct lsstruct), revmodcmp);
					} else {
						if (statustimeflag) {
							qsort((char *) stats, goodcount, sizeof(struct lsstruct), revstatcmp);
						} else {
						}
					}
				}
			}
		} else {
			if (!timesortflag) {
				qsort((char *) stats, goodcount, sizeof(struct lsstruct), namecmp);
			} else {
				if (accesstimeflag) {
					qsort((char *) stats, goodcount, sizeof(struct lsstruct), acccmp);
				} else {
					if (modtimeflag) {
						qsort((char *) stats, goodcount, sizeof(struct lsstruct), modcmp);
					} else {
						if (statustimeflag) {
							qsort((char *) stats, goodcount, sizeof(struct lsstruct), statcmp);
						} else {
						}
					}
				}
			}
		}
	}			/* end of sort conditionals */
	prindir(stats, goodcount);

	if ((firsttimethruflag && !listdirflag) || recursiveflag) {
		for (i = 0; i < goodcount; ++i) {
		/* recurse on directories */
			if ((stats[i].lstat.st_mode & S_IFMT) == S_IFDIR) {
			/* don't recurse on . or .. */
				if ((stats[i].name[0] == '.' && stats[i].name[1] == '\0') ||
						(stats[i].name[0] == '.' && stats[i].name[1] == '.' && stats[i].name[2] == '\0')) {
					if (!firsttimethruflag) {
						continue;
					}
				}
				if (chdir(stats[i].name) == -1) {
					perror(stats[i].name);
					break;
				}
				if (goodcount > 1 || !firsttimethruflag || recursiveflag) {
				/* add current name to path */
					if ((midpt = strchr(curpath, (int) '\0')) != curpath) {
						if (midpt[-1] != '/') {
							*midpt++ = '/';
						}
					}
					(void) strcpy(midpt, stats[i].name);

					if (goodcount > 1 || !firsttimethruflag) {
						(void) printf("\n%s:\n", curpath);
					}
				}
				recursedir(&stats[i]);
				if (goodcount > 1 || !firsttimethruflag) {
					*midpt = '\0';
				}
				if (chdir("..") == -1) {
					perror(stats[i].name);
				}
			}
		}		/* end of for loop looking for directories */
		firsttimethruflag = 0;
	}
	free((char *) stats);
}

/*
 * set up the call to lsdir (lsdir(count, argv-type array))
 * mutually recursive with lsdir
 */


recursedir(orig)
lsstruct *orig;
{
	DIR    *dirp;
	struct dirent *entry;
	char   *argvblock;
	char  **argv;
	int     blocksub = 0;
	int     vsub = 0;
	int     oldfirsttimeflag;

	if ((dirp = opendir(".")) == NULL) {
	/* perror(orig->name); more info, less compatible */
		(void) fprintf(stderr, "%s unreadable\n", orig->name);
		return;
	}
 /* wildly overestimate dynamic amount of memory needed */
 /* lint complains about casting off_t to unsigned int. */
	argvblock = emalloc((unsigned) orig->lstat.st_size);
 /* lint also complains about this alignment.  It's OK */
	argv = (char **) emalloc((unsigned) orig->lstat.st_size);

	while ((entry = readdir(dirp)) != NULL) {
		argv[vsub++] = strncpy(&argvblock[blocksub], entry->d_name, (int) entry->d_namlen);
		blocksub += entry->d_namlen;
		argvblock[blocksub++] = '\0';
	}
	(void) closedir(dirp);

	oldfirsttimeflag = firsttimethruflag;
	firsttimethruflag = 0;

	lsdir(vsub, argv);

	firsttimethruflag = oldfirsttimeflag;

	free(argvblock);
	free((char *) argv);
}

/*
 * print a statted (if necessary), sorted directory by listing option
 */

int     printaname();

prindir(stats, endcount)
lsstruct stats[];		/* the statted, sorted directory */
int     endcount;

{
	int     i;		/* subscript to stats */
	int     maxlen;		/* length of longest name string */
	int     colwidth;	/* width of a printing column */
	int     numcols;	/* number of columns */
	int     collength;	/* lines in longest column */
	int     base;		/* subscript for leftmost column */
	int     offset;		/* delta from base to next column */
	int     chcnt;		/* character count printed */
	long    blocks;		/* sum of blocks in longform listing */

	if (endcount <= 0) {	/* sanity check; also if only bad names given */
		return;
	}
/* don't print out a single directory name as argument first time around */
	if (!((firsttimethruflag && !listdirflag) && (endcount == 1) && ((stats[0].lstat.st_mode & S_IFMT) == S_IFDIR))) {
		if (singlecolflag) {
			for (i = 0; i < endcount; ++i) {
				(void) printaname(&stats[i]);
				(void) putchar('\n');
			}
		} else if (longformflag) {
			for (i = 0, blocks = 0; i < endcount; ++i) {
				blocks += stats[i].lstat.st_blocks;
			}
			(void) printf("total %ld\n", blocks / 2);
			for (i = 0; i < endcount; ++i) {
				if (inodeflag) {
					(void) printf("%6d ", stats[i].lstat.st_ino);
				}
				if (sizeflag) {
					(void) printf("%4d ", stats[i].lstat.st_blocks / 2);
				}
				printperms(stats[i].lstat.st_mode);
				(void) printf("%3d ", stats[i].lstat.st_nlink);
				printowner(stats[i].lstat.st_uid);
				if (groupflag) {
					printgrp(stats[i].lstat.st_gid);
				}
				if (((stats[i].lstat.st_mode & S_IFMT) == S_IFCHR) ||
						((stats[i].lstat.st_mode & S_IFMT) == S_IFBLK)) {
					(void) printf("%3d,%4d ", major(stats[i].lstat.st_rdev), minor(stats[i].lstat.st_rdev));
				} else {
					(void) printf("%8d ", stats[i].lstat.st_size);
				}
				if (accesstimeflag) {
					printtime(stats[i].lstat.st_atime);
				} else if (statustimeflag) {
					printtime(stats[i].lstat.st_ctime);
				} else {
					printtime(stats[i].lstat.st_mtime);
				}
				(void) printf("%s", stats[i].name);
				if (fancyflag) {
					(void) printfancy(stats[i].lstat.st_mode);
				}
				if ((stats[i].lstat.st_mode & S_IFMT) == S_IFLNK) {
					char    buf[MAXPATHLEN + 1];
					int     j;

					if ((j = readlink(stats[i].name, buf, MAXPATHLEN)) == -1) {
						perror(stats[i].name);
						(void) putchar('\n');
						continue;
					}
					buf[j + 1] = '\0';
					(void) printf(" -> %s", buf);
				}
				(void) putchar('\n');
			}
		} else {

		/*
		 * assume tabs every 8 columns WARNING: bad code (hard coded
		 * constants) follows:
		 */

		/* figure out max width */
			maxlen = 0;
			for (i = 0; i < endcount; ++i) {
				if (maxlen < lengthfcn(stats[i].name)) {
					maxlen = lengthfcn(stats[i].name);
				}
			}	/* end of determining max width of name */

		/* add fudge factors to max name length */
			if (inodeflag) {
				maxlen += 6;
			}
			if (sizeflag) {
				maxlen += 5;
			}
			if (fancyflag) {
				maxlen += 1;
			}
			colwidth = (maxlen + 9) & ~0x7;	/* one tab after maxlen */
			numcols = (80 + colwidth - maxlen) / colwidth;
			collength = (int) ((float) endcount / (float) numcols + 0.999);

			for (base = 0; base < collength; base++) {
				for (offset = 0, i = 0; i < numcols; ++i, offset += collength) {

					if ((base + offset) >= endcount) {
						break;
					}
					chcnt = printaname(&stats[base + offset]);

					if ((base + offset + collength) < endcount) {
						while (chcnt + 8 < colwidth) {
							(void) putchar('\t');
							chcnt += 8;
						}
						if (chcnt < colwidth) {
							(void) putchar('\t');
						}
						chcnt = (chcnt + 8) & ~0x7;
					}
				}
				if (base + offset < endcount) {
					(void) printaname(&stats[base + offset]);
				}
				(void) printf("\n");
			}	/* end of base and offset loop */
		}
	}
}

/*
 * print [inode] [size] name
 * return # of characters printed
 * no trailing characters
 */
int     printaname(entry)
lsstruct *entry;
{
	int     chcnt = 0;

	if (inodeflag) {
		chcnt += printf("%5d ", entry->lstat.st_ino);
	}
	if (sizeflag) {
		chcnt += printf("%4d ", entry->lstat.st_blocks / 2);
	}
	chcnt += printf("%s", entry->name);

	if (fancyflag) {
		return (chcnt + printfancy(entry->lstat.st_mode));
	}
	return chcnt;
}

/*
 * print group and user name
 */



printgrp(groupid)
short   groupid;
{
	struct group *groupentry;

	if ((groupentry = getgrgid((int) groupid)) == NULL) {
	/* can't find group, print out number instead */
		(void) printf("%-9d ", groupid);
		return;
	}
	(void) printf("%-9s", groupentry->gr_name);
	(void) getgrent();	/* to rewind group file */
}


printowner(uid)
short   uid;
{
	struct passwd *pwentry;

	if ((pwentry = getpwuid((int) uid)) == NULL) {
	/* can't find owner, print out number instead */
		(void) printf("%-9d ", uid);
		return;
	}
	(void) printf("%-9s", pwentry->pw_name);
	(void) getpwent();
}

#define SIXMONTHS 6l*30l*24l*3600l
time_t  time();

printtime(ftime)
time_t  ftime;
{
	int     i;
	char   *longstring;

	longstring = ctime((long *) &ftime);

	for (i = 4; i < 11; ++i) {
		(void) putchar(longstring[i]);
	}

	if (ftime + SIXMONTHS > time((time_t *) NULL)) {
		for (i = 11; i < 16; ++i) {
			(void) putchar(longstring[i]);
		}
	} else {
		(void) putchar(' ');
		for (i = 20; i < 24; ++i) {
			(void) putchar(longstring[i]);
		}
	}
	(void) putchar(' ');
}

/*
 * act like strlen, but also translate non-printing chars to '?'
 */


int     prablelen(cp)
char   *cp;
{
	register int len = 0;

	while (*cp != '\0') {
		if (!isprint(*cp)) {
			*cp = '?';
		}
		++len;
		++cp;
	}
	return len;
}

/*
 * do the permissions printing, passed the mode
 */

printperms(mode)
u_short mode;
{
 /* print type */
	switch (mode & S_IFMT) {
	case S_IFDIR:		/* directory */
		(void) putchar('d');
		break;
	case S_IFCHR:		/* character special */
		(void) putchar('c');
		break;
	case S_IFBLK:		/* block special */
		(void) putchar('b');
		break;
	case S_IFREG:		/* regular */
		(void) putchar('-');
		break;
	case S_IFLNK:		/* symbolic link */
		(void) putchar('l');
		break;
	case S_IFSOCK:		/* socket */
		(void) putchar('s');
		break;
#ifdef S_IFIFO
	case S_IFIFO:		/* fifo */
		(void) putchar('p');
		break;
#endif
	default:		/* unknown */
		(void) putchar('?');
		break;
	}
 /* usr */
	if (mode & S_IRUSR) {
		(void) putchar('r');
	} else {
		(void) putchar('-');
	}
	if (mode & S_IWUSR) {
		(void) putchar('w');
	} else {
		(void) putchar('-');
	}
	switch (mode & (S_IXUSR | S_ISUID)) {
	case 0:
		(void) putchar('-');
		break;
	case S_IXUSR:
		(void) putchar('x');
		break;
	case S_ISUID:
		(void) putchar('S');
		break;
	case S_IXUSR | S_ISUID:
		(void) putchar('s');
		break;
	}
 /* group */
	if (mode & S_IRGRP) {
		(void) putchar('r');
	} else {
		(void) putchar('-');
	}
	if (mode & S_IWGRP) {
		(void) putchar('w');
	} else {
		(void) putchar('-');
	}
	switch (mode & (S_IXGRP | S_ISGID)) {
	case 0:
		(void) putchar('-');
		break;
	case S_IXGRP:
		(void) putchar('x');
		break;
	case S_ISGID:
		(void) putchar('S');
		break;
	case S_IXGRP | S_ISGID:
		(void) putchar('s');
		break;
	}
 /* other */
	if (mode & S_IROTH) {
		(void) putchar('r');
	} else {
		(void) putchar('-');
	}
	if (mode & S_IWOTH) {
		(void) putchar('w');
	} else {
		(void) putchar('-');
	}
	switch (mode & (S_IXOTH | S_ISVTX)) {
	case 0:
		(void) putchar('-');
		break;
	case S_IXOTH:
		(void) putchar('x');
		break;
	case S_ISVTX:
		(void) putchar('T');
		break;
	case S_IXOTH | S_ISVTX:
		(void) putchar('t');
		break;
	}
}

int     printfancy(mode)
u_short mode;
{
	if ((mode & S_IFMT) == S_IFDIR) {
		(void) putchar('/');
		return 1;
	}
	if ((mode & S_IFMT) == S_IFLNK && !longformflag) {
		(void) putchar('@');
		return 1;
	}
	if ((mode & S_IFMT) == S_IFSOCK) {
		(void) putchar('=');
		return 1;
	}
	if (mode & (S_IXUSR | S_IXGRP | S_IXOTH)) {
		(void) putchar('*');
		return 1;
	}
	return 0;
}

/*
 * error checks for dynamic memory;
 * comparison routines for various calls to qsort(3)
 */

int     namecmp(a, b)
lsstruct *a, *b;
{
	return strcmp(a->name, b->name);
}

int     revnamecmp(a, b)
lsstruct *a, *b;
{
	return strcmp(b->name, a->name);
}

int     modcmp(a, b)
lsstruct *a, *b;
{
	return (a->lstat.st_mtime < b->lstat.st_mtime);
}

int     revmodcmp(a, b)
lsstruct *a, *b;
{
	return (b->lstat.st_mtime < a->lstat.st_mtime);
}

int     acccmp(a, b)
lsstruct *a, *b;
{
	return (a->lstat.st_atime < b->lstat.st_atime);
}

int     revacccmp(a, b)
lsstruct *a, *b;
{
	return (b->lstat.st_atime < a->lstat.st_atime);
}

int     statcmp(a, b)
lsstruct *a, *b;
{
	return (a->lstat.st_ctime < b->lstat.st_ctime);
}

int     revstatcmp(a, b)
lsstruct *a, *b;
{
	return (b->lstat.st_ctime < a->lstat.st_ctime);
}

char   *malloc();

char   *emalloc(size)
unsigned size;
{
	register char *retval;

	if ((retval = malloc(size)) == NULL) {
		perror("Can't find memory");
		exit(1);
	}
	return retval;
}

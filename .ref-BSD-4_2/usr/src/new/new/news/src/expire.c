/*
 * expire - expire daemon runs around and nails all articles that
 *		 have expired.
 *
 * Note: This version of expire contains some code to implement new
 * history features, e.g. to work without a history file or to rebuild
 * the history file.  This code was written for B news 2.9 and would
 * need some conversion to deal with the 2.10 heirarchical subgroups
 * in subdirectories.  This would imply a recursive traversal of the
 * tree.  Such code could be written but I didn't have the energy to
 * convert this 2.9 style code.
 */

static char	*SccsId = "@(#)expire.c	2.16	6/24/83";

#include "params.h"
#include "ndir.h"

#define NART	100

extern char	groupdir[BUFSIZ], rcbuf[BUFLEN];
extern char	ACTIVE[];
extern char	SPOOL[];
extern char	ARTFILE[];
extern char	filename[];
char	NARTFILE[BUFSIZ], OARTFILE[BUFSIZ];
char	OLDNEWS[BUFLEN];
int	verbose = 0;
int	ignorexp = 0;
int	doarchive = 0;
int	nohistory = 0;
int	rebuild = 0;

/*
 * The code dealing with this is ifdeffed out.
 * However, it should be redone to malloc the mh_ident
 * fields, and realloc the multhist array, so that the
 * fixed constant NART can be done away with.  Apparently
 * when rebuilding the history file, 100 is much too small.
 */
struct multhist {
	char	mh_ident[BUFLEN];
	char	*mh_file;
} multhist[NART];

typedef struct {
	char *dptr;
	int dsize;
} datum;

long	expincr;
long	atol();
time_t	cgtdate(), time();
FILE *popen();

main(argc, argv)
int	argc;
char	**argv;
{
	register int	i;
	register FILE *fp = NULL, *actfp;
	register char	*ptr;
	struct hbuf h;
	struct stat statbuf;
	register time_t now, newtime;
	char	ngpat[LBUFLEN];
	char	afline[BUFLEN];
	char	*p1, *p2, *p3;
	FILE	*ohfd, *nhfd;
	DIR	*ngdirp;
	static struct direct *ngdir;
	char fn[BUFLEN];
	datum	key;

	pathinit();
	umask(N_UMASK);
	expincr = DFLTEXP;
	ngpat[0] = '\0';
	while (argc > 1) {
		switch (argv[1][1]) {
		case 'v':
			if (isdigit(argv[1][2]))
				verbose = argv[1][2] - '0';
			else
				verbose = 1;
			if (verbose < 3)
				setbuf(stdout, NULL);
			break;
		case 'e':	/* Use this as default expiration time */
			if (argc > 2 && argv[2][0] != '-') {
				argv++;
				argc--;
				expincr = atol(argv[1]) * DAYS;
			}
			break;
		case 'I':	/* Ignore any existing expiration date */
			ignorexp = 2;
			break;
		case 'i':	/* Ignore any existing expiration date */
			ignorexp = 1;
			break;
		case 'n':
			if (argc > 2) {
				argv++;
				argc--;
				while (argc > 1 && argv[1][0] != '-') {
					strcat(ngpat, argv[1]);
					ngcat(ngpat);
					argv++;
					argc--;
				}
				argv--;
				argc++;
			}
			break;
		case 'a':	/* archive expired articles */
			doarchive++;
			break;
#ifdef notdef
/*
 * Nohistory and Rebuild are broken by the new directory format.
 * A recursive directory traversal needs to be made.  I don't
 * have the energy to do this - hopefully someone else will.
 */
		case 'h':	/* ignore history */
			nohistory++;
			break;
		case 'r':	/* rebuild history file */
			rebuild++;
			nohistory++;
			break;
#endif
		default:
			printf("Usage: expire [ -v [level] ] [-e days ] [-i] [-n newsgroups]\n");
			exit(1);
		}
		argc--; 
		argv++;
	}
	if (ngpat[0] == 0)
		strcpy(ngpat, "all,");
	now = time(0);
	if (chdir(SPOOL))
		xerror("Cannot chdir %s", SPOOL);

	sprintf(OARTFILE, "%s/%s", LIB, "ohistory");
	sprintf(ARTFILE, "%s/%s", LIB, "history");
	sprintf(NARTFILE, "%s/%s", LIB, "nhistory");
#ifdef DBM
	dbminit(ARTFILE);
#endif
	if (verbose)
		printf("expire: nohistory %d, rebuild %d, doarchive %d\n",
			nohistory, rebuild, doarchive);

	if (nohistory) {
		ohfd = xfopen(ACTIVE, "r");
		if (rebuild) {
			sprintf(afline, "sort +2 >%s", NARTFILE);
			if ((nhfd = popen(afline, "w")) == NULL)
				xerror("Cannot exec %s", NARTFILE);
		} else
			nhfd = xfopen("/dev/null", "w");
	} else {
		ohfd = xfopen(ARTFILE, "r");
		nhfd = xfopen(NARTFILE, "w");
	}

	while (TRUE) {
		if (nohistory) {
#ifdef notdef
			do {
				if (ngdir == NULL) {
					if ( ngdirp != NULL )
						closedir(ngdirp);
					if (fgets(afline, BUFLEN, ohfd) == NULL)
						goto out;
					strcpy(groupdir, afline);
					p1 = index(groupdir, ' ');
					if (p1 == NULL)
						p1 = index(groupdir, '\n');
					if (p1 != NULL)
						*p1 = NULL;
					ngcat(groupdir);
					if (!ngmatch(groupdir, ngpat))
						continue;
					ngdel(groupdir);
					if ((ngdirp = opendir(groupdir)) == NULL)
						continue;
				}
				ngdir = readdir(ngdirp);
			} while ( ngdir == NULL || ngdir->d_name[0] == '.' );
			sprintf(fn, "%s/%s", groupdir, ngdir->d_name);
			p2 = fn;
			if (verbose > 2)
				printf("article: %s\t", fn);
#endif
		} else {
			if (fgets(afline, BUFLEN, ohfd) == NULL)
				break;
			if (verbose > 2)
				printf("article: %s", afline);
			p1 = index(afline, '\t');
			if (p1)
				p2 = index(p1 + 1, '\t');
			else
				continue;
			if (!p2)
				continue;
			p2++;
			strcpy(groupdir, p2);
			p3 = index(groupdir, '/');
			if (p3)
				*p3 = 0;
			else {
				/*
				 * Nothing after the 2nd tab.  This happens
				 * when a control message is stored in the
				 * history file.  Use the date in the history
				 * file to decide expiration.
				 */
				h.expdate[0] = 0;
				strcpy(h.recdate, p1+1);
				goto checkdate;
			}
			ngcat(groupdir);
			if (!ngmatch(groupdir, ngpat)) {
				fputs(afline, nhfd);
				continue;
			}
			ngdel(groupdir);
			strcpy(fn, p2);
			p1 = index(fn, ' ');
			if (p1 == 0)
				p1 = index(fn, '\n');
			if (p1)
				*p1 = 0;
		}

		strcpy(filename, dirname(fn));
		if (access(filename, 4)
		|| (fp = fopen(filename, "r")) == NULL) {
			if (verbose > 3)
				printf("Can't open %s.\n", filename);
			continue;
		}
		if (hread(&h, fp, TRUE) == NULL) {
			if (verbose)
				printf("Garbled article %s.\n", filename);
			fclose(fp);
			continue;
		}
#ifdef notdef
		if (rebuild) {
			register char	*cp;
			register struct multhist *mhp;

			if ((cp = index(h.nbuf, NGDELIM)) == NULL) {
saveit:
				fprintf(nhfd, "%s\t%s\t%s \n", h.ident, h.recdate, filename);
				fclose(fp);
				continue;
			}
			for (mhp = multhist; mhp->mh_ident[0] != NULL && mhp < &multhist[NART]; mhp++) {
				if (mhp->mh_file == NULL)
					continue;
				if (strcmp(mhp->mh_ident, h.ident) != 0)
					continue;
				if (index(mhp->mh_file, ' ') != NULL)
					cp = index(++cp, NGDELIM);
				strcat(filename, " ");
				strcat(filename, mhp->mh_file);
				free(mhp->mh_file);
				if (*cp == NULL || (cp = index(++cp, NGDELIM)) == NULL) {
					mhp->mh_file = NULL;
					goto saveit;
				} else
					break;
			}
			if (mhp >= &multhist[NART])
				xerror("Too many articles with multiple newsgroups");
			strcpy(mhp->mh_ident, h.ident);
			cp = malloc(strlen(filename) + 1);
			if ( cp == NULL)
				xerror("Out of memory");
			strcpy(cp, filename);
			mhp->mh_file = cp;
			fclose(fp);
			continue;
		}
#endif

		fclose(fp);
checkdate:
		if (h.expdate[0])
			h.exptime = cgtdate(h.expdate);
		newtime = cgtdate(h.recdate) + expincr;
		if (!h.expdate[0] || ignorexp == 2 || 
		    (ignorexp == 1 && newtime < h.exptime))
			h.exptime = newtime;
		if (now >= h.exptime) {
#ifdef DEBUG
			printf("cancel %s\n", filename);
#else
			if (verbose)
				printf("cancel %s\n", filename);
			ulall(p2, &h);
# ifdef DBM
			key.dptr = h.ident;
			key.dsize = strlen(key.dptr) +1;
			delete(key);
# endif
#endif
		} else {
			fputs(afline, nhfd);
			if (verbose > 2)
				printf("Good article %s\n", rcbuf);
		}
	}

out:
#ifdef notdef
	if (rebuild) {
		register struct multhist *mhp;
		for (mhp = multhist; mhp->mh_ident[0] != NULL && mhp < &multhist[NART]; mhp++)
			/* should "never" happen */
			if (mhp->mh_file != NULL )
				printf("Article: %s %s Cannot find all links\n", mhp->mh_ident, mhp->mh_file);
		pclose(nhfd);
	}
#endif

	if (rebuild || !nohistory) {
		unlink(OARTFILE);
		link(ARTFILE, OARTFILE);
		unlink(ARTFILE);
		link(NARTFILE, ARTFILE);
		unlink(NARTFILE);
	}
	exit(0);
}

/* Unlink (using tail recursion) all the articles in 'artlist'. */
ulall(artlist, h)
char	*artlist;
struct hbuf *h;
{
	char	*p;
	int	last = 0;
	char	newname[BUFLEN];
	char	newgroup[BUFLEN];
	time_t	timep[2];
	char *fn;

	if (nohistory) {
		last = 1;
	} else {
		while (*artlist == ' ' || *artlist == '\n' || *artlist == ',')
			artlist++;
		if (*artlist == 0)
			return;
		p = index(artlist, ' ');
		if (p == 0) {
			last = 1;
			p = index(artlist, '\n');
		}
		if (p == 0) {
			last = 1;
			p = index(artlist, ',');
		}
		if (p == 0) {
			last = 1;
			fn = dirname(artlist);
			unlink(fn);
			return;
		}
		if (p)
			*p = 0;
	}
	fn = dirname(artlist);
	if (doarchive && access(OLDNEWS, 0) == 0) {
		p = fn + strlen(SPOOL) + 1;
		sprintf(newname, "%s/%s", OLDNEWS, p);
		if (verbose > 1)
			printf("link %s to %s\n", fn, newname);
		if (link(fn, newname) == -1) {
			if (mkparents(newname) == 0)
				link(fn, newname);
		}
		timep[0] = timep[1] = cgtdate(h->subdate);
		utime(newname, timep);
	}

	if (verbose)
		printf("unlink %s\n", fn);
	unlink(fn);
	if (!last)
		ulall(p + 1, h);
}


xerror(message)
char	*message;
{
	printf("expire: %s.\n", message);
	fflush(stdout);
	exit(1);
}

/*
 * If any parent directories of this dir don't exist, create them.
 */
mkparents(dirname)
char *dirname;
{
	char buf[200], sysbuf[200];
	register char *p;
	int rc;
	struct passwd *pw;

	strcpy(buf, dirname);
	p = rindex(buf, '/');
	if (p)
		*p = '\0';
	if (exists(buf))
		return;
	mkparents(buf);
	sprintf(sysbuf, "mkdir %s", buf);
	rc = system(sysbuf);
	strcpy(sysbuf, buf);
	if (verbose)
		printf("mkdir %s, rc %d\n", sysbuf, rc);
	chmod(sysbuf, 0755);
	if ((pw = getpwnam(NEWSU)) != NULL)
		chown(sysbuf, pw->pw_uid, pw->pw_gid);
}

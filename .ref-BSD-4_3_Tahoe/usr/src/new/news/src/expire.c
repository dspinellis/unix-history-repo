/*
 * This software is Copyright (c) 1986 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * expire - expire daemon runs around and nails all articles that
 *		 have expired.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)expire.c	2.55	10/15/87";
#endif /* SCCSID */

#include "params.h"
#include <errno.h>

#ifdef BSD4_2
# include <sys/file.h>
#endif /* BSD4_2 */

#ifdef LOCKF
#include <unistd.h>
#endif /* LOCKF */

char *Progname = "expire";	/* used by xerror to identify failing program */

/*	Number of array entries to allocate at a time.	*/
#define SPACE_INCREMENT	1000

struct expdata {
	char *e_name;
	long e_min, e_max;
	time_t	e_droptime, e_expiretime;
	char e_ignorexp;
	char e_doarchive;
	char e_doexpire;
};

extern int	errno;
char	NARTFILE[BUFLEN], OARTFILE[BUFLEN];
char	PAGFILE[BUFLEN], DIRFILE[BUFLEN];
char	NACTIVE[BUFLEN], OACTIVE[BUFLEN];
char	recdate[BUFLEN];
long	rectime, exptime;
extern char *OLDNEWS;
int	verbose = 0;		/* output trace information */
int	ignorexp = 0;		/* ignore Expire: lines */
int	doarchive = 0;		/* archive articles in SPOOL/oldnews */
int	nohistory = 0;		/* ignore history file */
int	dorebuild = 0;		/* rebuild history file */
int	dorbldhistory = 0;	/* rebuild history.d directory */
int	usepost = 0;		/* use posting date to expire */
int	frflag = 0;		/* expire specific user */
int	doupdateactive = 0;	/* update ACTIVE file */
char	baduser[BUFLEN];
extern 	char filename[], nbuf[];

struct timeb Now;

/*
 * This code uses realloc to get more of the multhist array.
 */
struct multhist {
	char	*mh_ident;
	char	*mh_file;
} *multhist;
unsigned int mh_size;
extern char *calloc(), *realloc();
struct tm *gmtime();

#ifndef DBM
FILE *nexthistfile();
#endif /* !DBM */

long	expincr;
long	dropincr;
long	atol();
time_t	cgtdate(), time();
FILE *popen();
struct passwd *pw;
struct group *gp;
char	arpat[LBUFLEN];
int	arpatlen = 0;
char	ngpat[LBUFLEN];
int	ngpatlen = 0;
char	afline[BUFLEN];
char	grpsleft[BUFLEN];
struct hbuf h;
int	rmlock();
time_t	today;

main(argc, argv)
int	argc;
char	**argv;
{
	pathinit();
	(void) umask(N_UMASK);
	username = NEWSUSR;

	/*
	 * Try to run as NEWSUSR/NEWSGRP
	 */
	if ((pw = getpwnam(NEWSUSR)) == NULL)
		xerror("Cannot get NEWSUSR pw entry");

	uid = pw->pw_uid;
	if ((gp = getgrnam(NEWSGRP)) == NULL)
		xerror("Cannot get NEWSGRP gr entry");
	gid = gp->gr_gid;
	(void) setgid(gid);
	(void) setuid(uid);

	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, rmlock);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, rmlock);
	expincr = DFLTEXP;
	dropincr = HISTEXP;
	ngpat[0] = ',';
	arpat[0] = ',';
	while (argc > 1) {
		switch (argv[1][1]) {
		case 'v':
			if (isdigit(argv[1][2]))
				verbose = argv[1][2] - '0';
			else if (argc > 2 && argv[2][0] != '-') {

				argv++;
				argc--;
				verbose = atoi(argv[1]);
			} else
				verbose = 1;
			if (verbose < 3)
				setbuf(stdout, (char *)NULL);
			break;
		case 'e':	/* Use this as default expiration time */
			if (argc > 2 && argv[2][0] != '-') {
				argv++;
				argc--;
				expincr = atol(argv[1]) * DAYS;
			} else if (isdigit(argv[1][2]))
				expincr = atol(&argv[1][2]) * DAYS;
			break;
		case 'E':	/* Use this as default forget time */
			if (argc > 2 && argv[2][0] != '-') {
				argv++;
				argc--;
				dropincr = atol(argv[1]) * DAYS;
			} else if (isdigit(argv[1][2]))
				dropincr = atol(&argv[1][2]) * DAYS;
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
					int argvlen;
					argvlen = strlen(argv[1]);
					if (ngpatlen + argvlen + 2 > sizeof (ngpat)) {
						xerror("Too many groups specified for -n\n");
					}
					if (ngpat[ngpatlen] == '\0') {
						ngpat[ngpatlen++] = ',';
						ngpat[ngpatlen] = '\0';
					}
					strcpy(&ngpat[ngpatlen], argv[1]);
					ngpatlen += argvlen;
					argv++;
					argc--;
				}
				argv--;
				argc++;
			}
			break;
		case 'a':	/* archive expired articles */
			if (access(OLDNEWS,0) < 0){
				perror(OLDNEWS);
				xerror("No archiving possible\n");
			}
			doarchive++;
			if (argc > 2) {
				argv++;
				argc--;
				while (argc > 1 && argv[1][0] != '-') {
					int argvlen;
					argvlen = strlen(argv[1]);
					if (arpatlen + argvlen + 2 > sizeof (arpat)) {
						xerror("Too many groups specified for -a\n");
					}
					if (arpat[arpatlen] == '\0') {
						arpat[arpatlen++] = ',';
						arpat[arpatlen] = '\0';
					}
					strcpy(&arpat[arpatlen], argv[1]);
					arpatlen += argvlen;
					argv++;
					argc--;
				}
				argv--;
				argc++;
			}
			break;
		case 'h':	/* ignore history */
			nohistory++;
			break;
		case 'r':	/* rebuild history file */
			dorebuild++;
			nohistory++;
			break;
		case 'R':	/* just rebuild the dbm files */
#ifdef DBM
			rebuilddbm();
			xxit(0);
#else /* !DBM */
			fprintf(stderr, "You have not compiled expire with DBM, so -R is meaningless\n");
			xxit(1);
#endif /* !DBM */

		case 'p':	/* use posting date to expire */
			usepost++;
			break;
		case 'f':	/* expire messages from baduser */
			frflag++;
			if (argc > 2) {
				strcpy(baduser, argv[2]);
				argv++;
				argc--;
			}
			break;
		case 'u':	/* update the active file from 2.10.1 fmt */
			doupdateactive++;
			break;
		case 'H':	/* convert to history.d format */
			dorbldhistory++;
			break;
		default:
			printf("Usage: expire [ -v [level] ] [-e days ] [-i] [-a] [-r] [-h] [-p] [-u] [-f username] [-n newsgroups] [-H]\n");
			xxit(1);
		}
		argc--;
		argv++;
	}
	if (dorbldhistory) {
#ifndef DBM
		rebuildhistorydir();
#endif /* !DBM */
		exit(0);
	}
	if (dropincr < expincr) {
		dropincr = HISTEXP;
		fprintf(stderr, "History expiration time < article expiration time. Default used.\n");
	}
	if (ngpat[0] == ',')
		(void) strcpy(ngpat, "all,");
	if (arpat[0] == ',')
		(void) strcpy(arpat, "all,");
	(void) ftime(&Now);
	today = Now.time;
	if (chdir(SPOOL))
		xerror("Cannot chdir %s", SPOOL);

	if (verbose) {
		printf("expire: nohistory %d, rebuild %d, doarchive %d\n",
			nohistory, dorebuild, doarchive);
		printf("newsgroups: %s\n",ngpat);
		if (doarchive)
			printf("archiving: %s\n",arpat);
	}

#ifdef DBM
	(void) sprintf(OARTFILE, "%s/%s", LIB, "ohistory");
#endif /* DBM */
	(void) sprintf(NARTFILE, "%s/%s", LIB, "nhistory");

	(void) sprintf(OACTIVE, "%s/%s", LIB, "oactive");
	(void) sprintf(NACTIVE, "%s/%s", LIB, "nactive");

	if (!doupdateactive) {
		expire();
#ifndef DBM
		rebuildhistorydir();
#endif
	}

	updateactive();
	rmlock();

	/*
	 * Now read in any saved news.
	 */
#ifdef PROFILING
	monitor((int(*)())0,(int(*)())0,0,0,0);
#endif /* PROFILING */
#ifdef LOGDIR
	/*afline happens to be available - (we're getting out anyway)*/
	sprintf(afline, "%s/%s", logdir(HOME), RNEWS);
	execl(afline, "rnews", "-U", (char *)NULL);
#else /* ! LOGDIR */
	execl(RNEWS, "rnews", "-U", (char *)NULL);
#endif /* ! LOGDIR */
	perror(RNEWS);
	xxit(1);
	/* NOTREACHED */
}

expire()
{
	register char	*p1, *p2, *p3;
	register time_t newtime;
	register FILE *fp = NULL;
	FILE	*ohfd, *nhfd;
	int i;
	char	fn[BUFLEN];
	DIR	*ngdirp = NULL;
	static struct direct *ngdir;

#ifdef DBM
	if (!dorebuild) {
		(void) sprintf(PAGFILE, "%s/%s", LIB, "nhistory.pag");
		(void) sprintf(DIRFILE, "%s/%s", LIB, "nhistory.dir");
		(void) close(creat(PAGFILE, 0666));
		(void) close(creat(DIRFILE, 0666));
		initdbm(NARTFILE);
	}
#endif

	if (nohistory) {
		ohfd = xfopen(ACTIVE, "r");
		if (dorebuild) {
			/* Allocate initial space for multiple newsgroup (for
			   an article) array */
			multhist = (struct multhist *)calloc (SPACE_INCREMENT,
					sizeof (struct multhist));
			mh_size = SPACE_INCREMENT;

			(void) sprintf(afline, "exec sort -t\t +1.6 -2 +1 >%s",
#ifdef DBM
			NARTFILE);
#else /* !DBM */
			ARTFILE);
#endif /* !DBM */
			if ((nhfd = popen(afline, "w")) == NULL)
				xerror("Cannot exec %s", afline);
		} else
			nhfd = xfopen("/dev/null", "w");
	} else {
#ifdef DBM
		ohfd = xfopen(ARTFILE, "r");
#else
		ohfd = nexthistfile((FILE *)NULL);
#endif /* DBM */
		nhfd = xfopen(NARTFILE, "w");
	}

	dolock();

	for(i=0;i<NUNREC;i++)
		h.unrec[i] = NULL;

	while (TRUE) {
		fp = NULL;
		if (nohistory) {
			recdate[0] = '\0';
			do {
				if (ngdir == NULL) {
					if ( ngdirp != NULL )
						closedir(ngdirp);
					if (fgets(afline, BUFLEN, ohfd) == NULL)
						goto out;
					(void) strcpy(nbuf, afline);
					p1 = index(nbuf, ' ');
					if (p1 == NULL)
						p1 = index(nbuf, '\n');
					if (p1 != NULL)
						*p1 = '\0';
					if (!ngmatch(nbuf, ngpat))
						continue;

					/* Change a group name from
					   a.b.c to a/b/c */
					for (p1=nbuf; *p1; p1++)
						if (*p1 == '.')
							*p1 = '/';

					if ((ngdirp = opendir(nbuf)) == NULL)
						continue;

				}
				ngdir = readdir(ngdirp);
			/*	Continue looking if not an article.	*/
			} while (ngdir == NULL || !islegal(fn,nbuf,ngdir->d_name));

			p2 = fn;
			if (verbose > 2)
				printf("article: %s\n", fn);
			strcpy(filename, dirname(fn));
			fp = access(filename, 04) ? NULL : art_open(filename, "r");
		} else {
			char dc;
#ifdef DBM
			if (fgets(afline, BUFLEN, ohfd) == NULL)
				break;
#else
			if (fgets(afline, BUFLEN, ohfd) == NULL)
				if (!(ohfd = nexthistfile(ohfd)))
					break;
				else
					continue;
#endif /* DBM */
			if (verbose > 2)
				printf("article: %s", afline);
			p1 = index(afline, '\t');
			if (!p1)
				continue;
			*p1 = '\0';
			(void) strcpy(h.ident, afline);
			*p1 = '\t';
			p2 = index(p1 + 1, '\t');
			if (!p2)
				continue;
			*p2 = '\0';
			(void) strcpy(recdate, p1+1);
			(void) strcat(recdate, " GMT");
			rectime = cgtdate(recdate);
			*p2++ = '\t';
			(void) strcpy(nbuf, p2);
			p3 = index(nbuf, '/');
			if (p3) {
				register char *p4;

				p4 = index(p3, '\n');
				if (p4) {
					while (p4[-1] == ' ')
						p4--;
					*p4 = '\0';
				}

				/*
				 * convert list of newsgroups from
				 *	ng1/num ng2/num ...
				 * to
				 *	ng1,ng2,...
				 */
				p4 = p3;
				do {
					*p3++ = NGDELIM;
					while (*p4 != '\0' && *p4 != ' ')
						p4++;
					if (*p4++ == '\0') {
						*--p3 = '\0';
						break;
					}
					while (*p3 = *p4++) {
						if (*p3 == '/')
							break;
						else
							p3++;
					}
				} while (*p3);
			} else {
				/*
				 * Nothing after the 2nd tab.  This happens
				 * when there is no message left in the spool
				 * directory, only the memory of it in the
				 * history file. (That is, it got cancelled
				 * or expired.) Use date in the history file
				 * to decide if we should keep the memory.
				 */
				grpsleft[0] = '\0';
				goto checkdate;
			}
			if (!ngmatch(nbuf, ngpat) ||
			     ((rectime+expincr > today) && !dorebuild &&
				 !frflag && !usepost && recdate[0] != ' '))
				goto keephist;
			if (!dorebuild && !frflag && !usepost &&
				recdate[0] != ' ') {
				grpsleft[0] = '\0';
				goto nailit; /* just expire it */
			}

			/*
			 * Look for the file--possibly several times,
			 * if it was posted to several news groups.
			 */
			dc = ' ';
			p3 = p2;
			while (dc != '\n') {
				p1 = index(p3, ' ');
				if (p1) {
					dc = ' ';
					*p1 = '\0';
				} else {
					p1 = index(p3, '\n');
					if (p1 && p1 > p3) {
						dc = '\n';
						*p1 = '\0';
					} else {
						fp = NULL;
						break;
					}
				}
				strcpy(filename, dirname(p3));
				if (access(filename, 4) == 0 &&
					((fp=art_open(filename, "r")) != NULL))
						break;
				p3 = p1 + 1;
			}
			if (p1)
				*p1 = dc;
		}

		if (fp == NULL) {
			/*
			 * this probably means that the article has been
			 * cancelled.  Lets assume that, and make an
			 * entry in the history file to that effect.
			 */
			if (verbose)
				perror(filename);
			strcpy(p2, "cancelled\n");
			grpsleft[0] = '\0';
			goto checkdate;
		}
		for(i=0; i<NUNREC; i++)
			if (h.unrec[i] != NULL) {
				free(h.unrec[i]);
				h.unrec[i] = NULL;
			} else
				break;
		if (!hread(&h, fp, TRUE)) {
			printf("Garbled article %s.\n", filename);
			(void) fclose(fp);
			/*
			 * Usually means disk ran out of space.
			 * Drop this article from our history file
			 * completely, so we have a chance of picking
			 * it up again from another feed ..
			 */
			goto nailit;
		}
		if (nohistory) {
			if (recdate[0] == '\0') {
				struct stat statb;
				if (fstat(fileno(fp), &statb) < 0)
					rectime = cgtdate(h.subdate);
				else
					rectime = statb.st_mtime;
			} else
				rectime = cgtdate(recdate);
		}
		if (dorebuild) {
			register char	*cp, *lastslash;
			register struct multhist *mhp;

			/*
			 * Format of filename until now was /SPOOL/a/b/c/4
			 * and this code changes it to a.b.c/4 (the correct
			 * kind of entry in the history file.)
			 *
			 * This cannot be a strcpy because the addresses
			 * overlap and some machines cannot handle that.
			 */
			p1 = filename;
			cp = p1 + strlen(SPOOL);
			while (*++cp) {
				if (*cp == '/') {
					lastslash = p1;
					*p1++ = '.';
				} else
					*p1++ = *cp;
			}
			*p1 = '\0';
			*lastslash = '/';

			if ((cp = index(h.nbuf, NGDELIM)) == NULL) {
				struct tm *tm;
saveit:
				tm = gmtime(&rectime);
				if (fprintf(nhfd,
#ifdef USG
				     "%s\t%s%2.2d/%2.2d/%d %2.2d:%2.2d\t%s\n",
#else /* !USG */
				     "%s\t%s%02d/%02d/%d %02d:%02d\t%s\n",
#endif /* !USG */
					h.ident, h.expdate[0] ? " " : "",
					tm->tm_mon+1, tm->tm_mday, tm->tm_year,
					tm->tm_hour, tm->tm_min, filename)
					== EOF)
						xerror("History write failed");
				(void) fclose(fp);
				continue;
			}
			for (mhp = multhist; mhp < multhist+mh_size && mhp->mh_ident != NULL; mhp++) {
				if (mhp->mh_file == NULL)
					continue;
				if (strcmp(mhp->mh_ident, h.ident))
					continue;
				(void) strcat(filename, " ");
				(void) strcat(filename, mhp->mh_file);
				free(mhp->mh_file);
				mhp->mh_file = NULL;
				/*
				 * if we have all the links, write to hist now
				 */
				if (chrcnt(filename, ' ') == chrcnt(cp,NGDELIM))
					goto saveit;
				break;
			}

			/*
			 * Here is where we realloc the multhist space rather
			 * than the old way of static allocation.  It is
			 * really trivial.  We just clear out the space
			 * in case it was reused.  The old static array was
			 * guaranteed to be cleared since it was cleared when
			 * the process started.
			 */
			if (mhp >= multhist + mh_size) {
				multhist = (struct multhist *)
					realloc ((char *)multhist,
					  sizeof (struct multhist) *
					  (SPACE_INCREMENT + mh_size));
				if (multhist == NULL)
					xerror("Too many articles with multiple newsgroups");
				for (mhp = multhist + mh_size;
				  mhp < multhist+mh_size+SPACE_INCREMENT;
					mhp++) {
					mhp->mh_ident = NULL;
					mhp->mh_file = NULL;
				}
				mhp = multhist + mh_size;
				mh_size += SPACE_INCREMENT;
			}

			if (mhp->mh_ident == NULL) {
				mhp->mh_ident = malloc(strlen(h.ident)+1);
				(void) strcpy(mhp->mh_ident, h.ident);
			}
			cp = malloc(strlen(filename) + 1);
			if (cp == NULL)
				xerror("Out of memory");
			(void) strcpy(cp, filename);
			mhp->mh_file = cp;
			(void) fclose(fp);
			continue;
		}

		(void) fclose(fp);

		if (h.expdate[0]) {
			Now.time = rectime;
			exptime = cgtdate(h.expdate);
		}
		newtime = (usepost ? cgtdate(h.subdate) : rectime) + expincr;
		if (!h.expdate[0] || ignorexp == 2 ||
		    (ignorexp == 1 && newtime < exptime))
			exptime = newtime;
		if (frflag ? strcmp(baduser,h.from)==0 : today >= exptime) {
nailit:
#ifdef DEBUG
			printf("cancel %s\n", filename);
#else /* !DEBUG */
			if (verbose)
				printf("cancel %s\n", h.ident);
			ulall(p2, &h);
			(void) sprintf(p2, "%s\n", grpsleft);
			if (verbose > 2 && grpsleft[0])
				printf("Some good in %s\n", h.ident);
#endif /* !DEBUG */
		} else {
			if (verbose > 2)
				printf("Good article %s\n", h.ident);
			grpsleft[0] = '!';
		}

checkdate:
		if (grpsleft[0] == '\0' && today >= rectime + dropincr) {
			if (verbose > 3)
				printf("Drop history of %s - %s\n",
				    h.ident, recdate);
		} else {
#ifdef DBM
			long hpos;
#endif /* DBM */
keephist:
#ifdef DBM
			hpos = ftell(nhfd);
#endif /* DBM */

			if (verbose > 3)
				printf("Retain history of %s - %s\n",
				    h.ident, recdate);
			if (fputs(afline, nhfd) == EOF)
				xerror("history write failed");
#ifdef DBM
			if (!dorebuild)
				remember(h.ident, hpos);
#endif /* DBM */
		}
	}
out:
	if (dorebuild) {
		register struct multhist *mhp;
		struct tm *tm;
		for (mhp = multhist; mhp < multhist+mh_size && mhp->mh_ident != NULL; mhp++)
			if (mhp->mh_file != NULL) {
				if (verbose)
					printf("Article: %s [%s] Cannot find all links\n", mhp->mh_ident, mhp->mh_file);
				(void) sprintf(filename,"%s/%s",SPOOL,mhp->mh_file);
				for (p1 = filename; *p1 != ' ' && *p1 != '\0'; p1++)
					if (*p1 == '.')
						*p1 = '/';
				*p1 = '\0';
				if ((fp = art_open(filename, "r")) == NULL) {
					if (verbose)
						printf("Can't open %s.\n", filename);
					continue;
				}
				if (!hread(&h, fp, TRUE)) {
					printf("Garbled article %s.\n", filename);
					(void) fclose(fp);
					continue;
				} else {
					struct stat statb;
					if (fstat(fileno(fp), &statb) < 0)
						rectime = cgtdate(h.subdate);
					else
						rectime = statb.st_mtime;
				}
				tm = gmtime(&rectime);
				if ( fprintf(nhfd,
#ifdef USG
					"%s\t%s%2.2d/%2.2d/%d %2.2d:%2.2d\t%s\n",
#else /* !USG */
					"%s\t%s%02d/%02d/%d %02d:%02d\t%s\n",
#endif /* !USG */
					h.ident, h.expdate[0] ? " " : "",
					tm->tm_mon+1, tm->tm_mday, tm->tm_year,
					tm->tm_hour, tm->tm_min, mhp->mh_file)
					== EOF )
						xerror("History write failed");
				(void) fclose(fp);
				continue;
			}
		(void) pclose(nhfd);
		free ((char *)multhist);
	} else
		if (fclose(nhfd))
			xerror("History write failed, %s", errmsg(errno));

	if (dorebuild || !nohistory) {
#ifdef DBM
		(void) rename(ARTFILE, OARTFILE);
		(void) rename(NARTFILE, ARTFILE);
		if (dorebuild)
			rebuilddbm( );
		else {
			char tempname[BUFLEN];
			(void) sprintf(tempname,"%s.pag", ARTFILE);
			(void) strcat(NARTFILE, ".pag");
			(void) rename(NARTFILE, tempname);
			(void) sprintf(tempname,"%s.dir", ARTFILE);
			(void) strcpy(rindex(NARTFILE, '.'), ".dir");
			(void) rename(NARTFILE, tempname);
		}
#endif
	}
}

#if defined(BSD4_2) || defined(LOCKF)
static int LockFd = -1;
#endif

dolock()
{
	/* set up exclusive locking so inews does not run while expire does */
#if defined(BSD4_2) || defined(LOCKF)
	LockFd = open(ACTIVE, 2);
# ifdef	LOCKF
	if (lockf(LockFd, F_LOCK, 0L) < 0)
# else	/* BSD4_2 */
	if (flock(LockFd, LOCK_EX) < 0)
# endif	/* BSD4_2 */
		xerror("Can't get lock for expire: %s", errmsg(errno));
#else	/* !BSD4_2 && !LOCKF */
	int i = 0;
	sprintf(afline,"%s.lock", ACTIVE);
	while (LINK(ACTIVE, afline) < 0 && errno == EEXIST) {
		if (i++ > 5) {
			xerror("Can't get lock for expire");
		}
		sleep(i*2);
	}
#endif	/* !BSD4_2  && !LOCKF */
}

rmlock()
{
#if defined(BSD4_2) || defined(LOCKF)
	close(LockFd);
#else
	sprintf(bfr, "%s.lock", ACTIVE);
	(void) UNLINK(bfr);
#endif	/* !BSD4_2 */
}

updateactive()
{
	register char	*p1;
	FILE	*ohfd, *nhfd;
	DIR	*ngdirp = NULL;
	static struct direct *ngdir;

	if (verbose)
		printf("updating active file %s\n", ACTIVE);
	ohfd = xfopen(ACTIVE, "r");
	nhfd = xfopen(NACTIVE, "w");
	do {
		long n;
		long maxart, minart;
		char cansub;
		int gdsize, hassubs;
		struct stat stbuf;

		if (fgets(afline, BUFLEN, ohfd) == NULL)
			continue;
		if (sscanf(afline,"%s %ld %ld %c",nbuf,&maxart, &minart,
		    &cansub) < 4)
			xerror("Active file corrupt");
		if (verbose > 3)
			printf("looking at group %s\n", nbuf);
		if (!ngmatch(nbuf, ngpat)) {
			if (fputs(afline, nhfd) == EOF)
				xerror("active file write failed");
			continue;
		}
		minart = 99999L;
		/* Change a group name from a.b.c to a/b/c */
		for (p1=nbuf; *p1; p1++)
			if (*p1 == '.')
				*p1 = '/';

		hassubs = stat(nbuf, &stbuf) != 0 || stbuf.st_nlink != 2;
		gdsize = strlen(nbuf);
		if ((ngdirp = opendir(nbuf)) != NULL) {
			while (ngdir = readdir(ngdirp)) {
				nbuf[gdsize] = '/';
				(void) strcpy(&nbuf[gdsize+1], ngdir->d_name);
				/* We have to do a stat because of micro.6809 */
				if (hassubs && (stat(nbuf, &stbuf) < 0 ||
					!(stbuf.st_mode&S_IFREG)) )
					continue;
				n = atol(ngdir->d_name);
				if (n > 0 && n < minart)
					minart = n;
				if (n > 0 && n > maxart)
					maxart = n;
			}
			closedir(ngdirp);
		}
		afline[gdsize] = '\0';
		if (minart > maxart)
			minart = maxart;
#ifdef USG
		if (verbose > 4)
			printf("\tmaxart = %5.5ld, minart = %5.5ld\n",
				maxart, minart);
		if (fprintf(nhfd,"%s %5.5ld %5.5ld %c\n", afline, maxart,
			minart, cansub) == EOF)
			xerror("Active file write failed");
#else
		if (verbose > 4)
			printf("\tmaxart = %05ld, minart = %05ld\n",
				maxart, minart);
		if (fprintf(nhfd,"%s %05ld %05ld %c\n", afline, maxart,
			minart, cansub) == EOF)
			xerror("Active file write failed");
#endif /* !USG */
	} while (!feof(ohfd));
	if (fclose(nhfd))
		xerror("Active file write failed, %s", errmsg(errno));
	(void) fclose(ohfd); /* this might unlock inews as a side effect */

	(void) rename(ACTIVE, OACTIVE);
	(void) rename(NACTIVE, ACTIVE);
}

/* Unlink (using unwound tail recursion) all the articles in 'artlist'. */
ulall(artlist, hp)
char	*artlist;
struct hbuf *hp;
{
	register char	*p, *q;
	int	last = 0;
	char	newname[BUFLEN];
	time_t	timep[2];
	char *fn;

	grpsleft[0] = '\0';
	do {
		if (verbose > 2)
			printf("ulall '%s', '%s'\n", artlist, hp->subdate);
		if (nohistory) {
			last = 1;
		} else {
			while (*artlist == ' ' || *artlist == '\n' || *artlist == ',')
				artlist++;
			if (*artlist == '\0')
				return;
			p = index(artlist, ' ');
			if (p == NULL) {
				last = 1;
				p = index(artlist, '\n');
			}
			if (p)
				*p = 0;
		}
		strcpy(newname, artlist);
		q = index(newname,'/');
		if (q) {
			*q++ = NGDELIM;
			*q = '\0';
		} else {
			q = index(newname, '\0');
			if (q == artlist)		/* null -> the end */
				return;
			/* should be impossible to get here */
		}
		fn = dirname(artlist);
		if (ngmatch(newname, ngpat)) {
			if (doarchive){
				if (ngmatch(newname, arpat)) {
					q = fn + strlen(SPOOL) + 1;
					(void) sprintf(newname, "%s/%s", OLDNEWS, q);
					if (verbose)
						printf("link %s to %s\n", fn, newname);
					if (LINK(fn, newname) == -1) {
						if (mkparents(newname) == 0)
							if (LINK(fn, newname) == -1)
								fcopy(fn, newname);
					}
					timep[0] = timep[1] = cgtdate(hp->subdate);
					(void) utime(newname, timep);
				}
			}
			if (verbose)
				printf("unlink %s\n", fn);
			if (UNLINK(fn) < 0 && errno != ENOENT)
				perror(fn);
		} else {
			if (verbose > 3)
				printf("retain %s (%s)\n", hp->ident, fn);
			strcat(grpsleft, artlist);
			strcat(grpsleft, " ");
		}
		artlist = p + 1;
	} while (!last);
}

fcopy(fn, newname)
char *fn, *newname;
{
	int f1, f2;
	int r;
	char buf[BUFSIZ];
	f1 = open(fn, 0);
	if (f1 < 0)
		return -1;
	f2 = open(newname, 1);
	if (f2 < 0) {
		if (errno == ENOENT) {
			f2 = creat(newname,0644);
			if (f2 < 0) {
				close(f1);
				return -1;
			}
		} else {
			close(f1);
			return -1;
		}
	}
	while((r=read(f1, buf, BUFSIZ)) > 0)
		write(f2, buf, r);
	(void) close(f1);
	(void) close(f2);
	return 0;
}

/*
 * Count instances of c in s
 */
chrcnt(s, c)
register char *s;
register c;
{
	register n = 0;
	register cc;

	while (cc = *s++)
		if (cc == c)
			n++;
	return n;
}

/*
 * If any parent directories of this dir don't exist, create them.
 */
mkparents(fullname)
char *fullname;
{
	char buf[200];
	register char *p;
	int rc;

	(void) strcpy(buf, fullname);
	p = rindex(buf, '/');
	if (p)
		*p = '\0';
	if (access(buf, 0) == 0)
		return 0;
	mkparents(buf);
	if ((rc = mkdir(buf, 0755)) < 0)
		perror("mkdir failed");
	if (verbose)
		printf("mkdir %s, rc %d\n", buf, rc);

	return rc;
}

/*	Make sure this file is a legal article. */
islegal(fullname, path, name)
register char *fullname;
register char *path;
register char *name;
{
	struct stat buffer;

	(void) sprintf(fullname, "%s/%s", path, name);

	/* make sure the article is numeric. */
	while (*name != '\0')
		if (!isascii(*name) || !isdigit(*name))
			return 0;
		else
			name++;

	/*  Now make sure we don't have a group like net.micro.432,
	 *  which is numeric but not a regular file -- i.e., check
	 *  for being a regular file.
	 */
	if ((stat(fullname, &buffer) == 0) &&
		((buffer.st_mode & S_IFMT) == S_IFREG)) {
		/* Now that we found a legal group in a/b/c/4
		   notation, switch it to a.b.c/4 notation.  */
		for (name = fullname; name != NULL && *name != '\0'; name++)
			if (*name == '/' && name != rindex (name, '/'))
				*name = '.';

			return 1;
	}
	return 0;
}

#ifdef DBM
/*
 * This is taken mostly intact from ../cvt/cvt.hist.c and is used at the
 * end by the options that make a new history file.
 * Routine to convert history file to dbm file.  The old 3 field
 * history file is still kept there, because we need it for expire
 * and for a human readable copy.  But we keep a dbm hashed copy
 * around by message ID so we can answer the yes/no question "have
 * we already seen this message".  The content is the ftell offset
 * into the real history file when we get the article - you can't
 * really do much with this because the file gets compacted.
 */

FILE *fd;

char namebuf[BUFSIZ];
char lb[BUFSIZ];

rebuilddbm()
{
	register char *p;
	long fpos;

	(void) sprintf(namebuf, "%s.dir", ARTFILE);
	(void) close(creat(namebuf, 0666));
	(void) sprintf(namebuf, "%s.pag", ARTFILE);
	(void) close(creat(namebuf, 0666));
	(void) sprintf(namebuf, "%s", ARTFILE);

	fd = fopen(namebuf, "r");
	if (fd == NULL) {
		perror(namebuf);
		xxit(2);
	}

	initdbm(namebuf);
	while (fpos=ftell(fd), fgets(lb, BUFSIZ, fd) != NULL) {
		p = index(lb, '\t');
		if (p)
			*p = 0;
		remember(lb, fpos);
	}
}

remember(article, fileoff)
register char *article;
long fileoff;
{
	datum	lhs, rhs;

	lcase(article);
	lhs.dptr = article;
	lhs.dsize = strlen(article) + 1;
	rhs.dptr = (char *) &fileoff;
	rhs.dsize = sizeof fileoff;

	if (verbose > 5)
		printf("remember: %s @ %ld\n", article, fileoff);
	if (store(lhs, rhs) < 0)
		xerror("dbm store failed");
}
#else
/*
 * Open the next history subdirectory file
 */

FILE *
nexthistfile(ofp)
FILE *ofp;
{
	static int histfilecounter = -1;

	if (ofp)
		fclose(ofp);
	do {
		if (++histfilecounter > 9)
			return NULL;
		sprintf(bfr, "%s.d/%d", ARTFILE, histfilecounter);
		if (verbose > 3)
			printf("reading history file %s\n", bfr);
		ofp = xfopen(bfr, "r");
	} while (ofp == NULL);
	return ofp;
}

/*
 * Rebuild the history subdirectory from LIBDIR/history
 */
rebuildhistorydir()
{
	char fn[BUFLEN], ofn[BUFLEN];
	register int i;
	FILE *subfd[10], *ohfd;

	/* rebuild history subfiles */
	(void) sprintf(fn, "%s.od", ARTFILE);
	if (access(fn,0) != 0)
		(void) mkdir(fn, 0755);
	(void) sprintf(fn, "%s.d", ARTFILE);
	if (verbose)
		printf("Rebuilding history subfile directory %s.\n", fn);
	if (access(fn,0) != 0)
		(void) mkdir(fn, 0755);
	for (i = 0; i < 10; i++) {
		(void) sprintf(fn, "%s.d/%c", ARTFILE, i + '0');
		(void) sprintf(ofn, "%s.od/%c", ARTFILE, i + '0');
		(void) rename(fn, ofn);
		close(creat(fn, 0644));
		subfd[i] = xfopen(fn, "w+");
	}
	ohfd = xfopen(ARTFILE, "r");
	while (fgets(fn, BUFLEN, ohfd) != NULL) {
		i = findhfdigit(fn) - '0';
		fputs(fn, subfd[i]);
	}
	(void) fclose(ohfd);
	for (i = 0; i < 10; i++)
		if (ferror(subfd[i]) || fclose(subfd[i]))
			xerror("History subfile write");
	(void) UNLINK(ARTFILE);
}
#endif /* !DBM */

xxit(i)
{
	rmlock();
	exit(i);
}

#ifndef lint
static	char *sccsid = "@(#)main.c	4.3 (Berkeley) 83/10/10";
#endif

#include "defs.h"

/*
 * Remote distribution program.
 */

char	*distfile = "distfile";
char	tmpfile[] = "/tmp/rdistAXXXXXX";
char	*tmpname = &tmpfile[5];
char	*tmpinc = &tmpfile[10];

int	debug;		/* debugging flag */
int	nflag;		/* NOP flag, just print commands without executing */
int	qflag;		/* Quiet. Don't print messages */
int	vflag;		/* verify only */
int	yflag;		/* update iff remote younger than master */
int	iamremote;	/* act as remote server for transfering files */

int	filec;		/* number of files to update */
char	**filev;	/* list of files/directories to update */
FILE	*fin = NULL;	/* input file pointer */
int	rem = 0;	/* file descriptor to remote source/sink process */
char	host[32];	/* host name */
int	errs;		/* number of errors while sending/receiving */
char	user[10];	/* user's name */
char	homedir[128];	/* user's home directory */
int	userid;		/* user's user ID */
int	groupid;	/* user's group ID */
int	iamupdate;

int	cleanup();
int	lostconn();

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;
	register struct	passwd *pw;

	arg = rindex(argv[0], '/');
	if (arg == NULL)
		arg = argv[0];
	else
		arg++;
	if (!strcmp(arg, "update"))
		iamupdate++;

	pw = getpwuid(userid = getuid());
	if (pw == NULL) {
		fprintf(stderr, "%s: Who are you?\n", argv[0]);
		exit(1);
	}
	strcpy(user, pw->pw_name);
	strcpy(homedir, pw->pw_dir);
	groupid = pw->pw_gid;
	gethostname(host, sizeof(host));

	while (--argc > 0) {
		if ((arg = *++argv)[0] != '-')
			break;
		if (!strcmp(arg, "-Server"))
			iamremote++;
		else while (*++arg)
			switch (*arg) {
			case 'f':
				if (--argc <= 0)
					usage();
				distfile = *++argv;
				if (distfile[0] == '-' && distfile[1] == '\0')
					fin = stdin;
				break;

			case 'd':
				if (--argc <= 0)
					usage();
				define(*++argv);
				break;

			case 'D':
				debug++;
				break;

			case 'n':
				nflag++;
				break;

			case 'q':
				qflag++;
				break;

			case 'v':
				vflag++;
				break;

			case 'y':
				yflag++;
				break;

			default:
				usage();
			}
	}

	mktemp(tmpfile);
	signal(SIGPIPE, lostconn);
	if (iamremote) {
		server();
		exit(errs);
	}

	signal(SIGHUP, cleanup);
	signal(SIGINT, cleanup);
	signal(SIGQUIT, cleanup);
	signal(SIGTERM, cleanup);

	if (iamupdate)
		doupdate(argc, argv);
	else {
		filec = argc;
		filev = argv;
		if (fin == NULL && (fin = fopen(distfile, "r")) == NULL) {
			perror(distfile);
			exit(1);
		}
		yyparse();
	}

	exit(errs);
}

usage()
{
	printf("Usage: rdist [-f distfile] [-d var=value] [-nqyD] [file ...]\n");
	exit(1);
}

/*
 * rcp like interface for distributing files.
 */
doupdate(nargs, args)
	int nargs;
	char *args[];
{
	struct block *bp, *files, *hosts, *cmds, *prev;
	int i;
	char *pos, dest[BUFSIZ];

	if (nargs < 2)
		upusage();

	prev = NULL;
	bp = files = ALLOC(block);
	for (i = 0; i < nargs - 1; bp = ALLOC(block), i++) {
		bp->b_type = NAME;
		bp->b_name = args[i];
		if (prev != NULL)
			prev->b_next = bp;
		bp->b_next = bp->b_args = NULL;
		prev = bp;
	}

	hosts = ALLOC(block);
	hosts->b_type = NAME;
	hosts->b_name = args[i];
	hosts->b_name = args[i];
	hosts->b_next = hosts->b_args = NULL;
	if ((pos = index(hosts->b_name, ':')) != NULL) {
		*pos++ = '\0';
		strcpy(dest, pos);
	} else
		dest[0] = '\0';

	hosts = expand(hosts, 0);

	if (dest[0] == '\0')
		cmds = NULL;
	else {
		cmds = ALLOC(block);
		if (vflag)
			cmds->b_type = VERIFY;
		else
			cmds->b_type = INSTALL;
		cmds->b_name = dest;
		cmds->b_next = cmds->b_args = NULL;
	}

	if (debug) {
		printf("doupdate()\nfiles = ");
		prnames(files);
		printf("hosts = ");
		prnames(hosts);
	}
	dohcmds(files, hosts, cmds);
}

upusage()
{
	printf("Usage: update [-nqyD] source [...] machine[:dest]\n");
	exit(1);
}

/*
 * Remove temporary files and do any cleanup operations before exiting.
 */
cleanup()
{
	(void) unlink(tmpfile);
	exit(1);
}

/*
 * Print a list of NAME blocks (mostly for debugging).
 */
prnames(bp)
	register struct block *bp;
{
	printf("( ");
	while (bp != NULL) {
		printf("%s ", bp->b_name);
		bp = bp->b_next;
	}
	printf(")\n");
}

/*VARARGS*/
warn(fmt, a1, a2,a3)
	char *fmt;
{
	extern int yylineno;

	fprintf(stderr, "rdist: line %d: Warning: ", yylineno);
	fprintf(stderr, fmt, a1, a2, a3);
	fputc('\n', stderr);
}

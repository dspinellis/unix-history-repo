#ifndef lint
static	char *sccsid = "@(#)main.c	4.6 (Berkeley) 83/10/20";
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
int	options;	/* global options */
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

int	cleanup();
int	lostconn();

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;
	register struct	passwd *pw;
	int cmdargs = 0;

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

			case 'c':
				cmdargs++;
				break;

			case 'n':
				nflag++;
				break;

			case 'q':
				qflag++;
				break;

			case 'r':
				options |= REMOVE;
				break;

			case 'v':
				options |= VERIFY;
				break;

			case 'w':
				options |= WHOLE;
				break;

			case 'y':
				options |= YOUNGER;
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

	if (cmdargs)
		docmdargs(argc, argv);
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
	printf("Usage: rdist [-nqvwyD] [-f distfile] [-d var=value] [file ...]\n");
	printf("or: rdist [-nqvwyD] -c source [...] machine[:dest]\n");
	exit(1);
}

/*
 * rcp like interface for distributing files.
 */
docmdargs(nargs, args)
	int nargs;
	char *args[];
{
	struct block *bp, *files, *hosts, *cmds, *prev;
	int i;
	char *pos, dest[BUFSIZ];

	if (nargs < 2)
		usage();

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
		cmds->b_type = INSTALL;
		cmds->b_options = options;
		cmds->b_name = dest;
		cmds->b_next = cmds->b_args = NULL;
	}

	if (debug) {
		printf("docmdargs()\nfiles = ");
		prnames(files);
		printf("hosts = ");
		prnames(hosts);
	}
	dohcmds(files, hosts, cmds);
}

/*
 * Remove temporary files and do any cleanup operations before exiting.
 */
cleanup()
{
	do {
		(void) unlink(tmpfile);
		(*tmpinc)--;
	} while (*tmpinc >= 'A');
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

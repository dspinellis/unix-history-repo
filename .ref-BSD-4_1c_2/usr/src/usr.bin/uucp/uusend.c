static char *sccsid = "@(#)uusend.c	4.2 (Berkeley) 4/21/81";
/*
 * uusend: primative operation to allow uucp like copy of binary files
 * but handle indirection over systems.
 *
 * usage:  uusend [-m ooo] localfile  sysname1!sysname2!...!destfile
 *         uusend [-m ooo]     -      sysname1!sysname2!...!destfile
 *
 * Author: Mark Horton, May 1980.
 */

#include <stdio.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/stat.h>

/* #define DEBUG	"/usr/spool/uucp/uusend.log" */

FILE	*in, *out;
FILE	*dout;

FILE	*popen();
char	*index();

int	mode = -1;	/* mode to chmod new file to */
char	nextsys[20];	/* next system in the chain */
char	dnbuf[200];	/* buffer for result of ~user/file */
char	cmdbuf[256];	/* buffer to build uux command in */

struct	passwd *user;	/* entry  in /etc/passwd for ~user */
struct	passwd *getpwnam();
struct	stat	stbuf;

char	*excl;		/* location of first ! in destname */
char	*sl;		/* location of first / in destname */
char	*sourcename;	/* argv[1] */
char	*destname;	/* argv[2] */

main(argc, argv)
int	argc;
char	**argv;
{
	register int c;
	register int count = 0;

#ifdef DEBUG
	long t;
	dout = fopen(DEBUG, "a");
	if (dout == NULL) {
		printf("Cannot append to %s\n", DEBUG);
		exit(1);
	}
	freopen(DEBUG, "a", stdout);
	freopen(DEBUG, "a", stderr);
	chmod(DEBUG, 0666);
	fprintf(dout, "\nuusend run: ");
	for (c=0; c<argc; c++)
		fprintf(dout, "%s ", argv[c]);
	time(&t);
	fprintf(dout, "%s", ctime(&t));
#endif
	while (argc > 1 && argv[1][0] == '-' && argv[1][1]) {
		switch(argv[1][1]) {
		case 'm':
			sscanf(argv[2], "%o", &mode);
			argc--; argv++;
			break;
		default:
			fprintf(stderr, "Bad flag: %s\n", argv[1]);
			break;
		}
		argc--; argv++;
	}

	if (argc != 3) {
		fprintf(stderr, "Usage: uusend [-m ooo] -/file sys!sys!..!rfile\n");
		exit(1);
	}

	sourcename = argv[1];
	destname = argv[2];

	if (sourcename[0] == '-')
		in = stdin;
	else {
		in = fopen(sourcename, "r");
		if (in == NULL) {
			perror(argv[1]);
			exit(2);
		}
	}

	excl = index(destname, '!');
	if (excl) {
		/*
		 * destname is on a remote system.
		 */
		strncpy(nextsys, destname, excl-destname);
		nextsys[excl-destname] = 0;
		destname = excl+1;
		if (mode < 0) {
			fstat(fileno(in), &stbuf);
			mode = stbuf.st_mode & 0777;
		}
		sprintf(cmdbuf, "uux - -r \"%s!uusend -m %o - \(%s\)\"",
			nextsys, mode, destname);
#ifdef DEBUG
		fprintf(dout, "remote: nextsys='%s', destname='%s', cmd='%s'\n", nextsys, destname, cmdbuf);
#endif
		out = popen(cmdbuf, "w");
	} else {
		/*
		 * destname is local.
		 */
		if (destname[0] == '~') {
#ifdef DEBUG
			fprintf(dout, "before ~: '%s'\n", destname);
#endif
			sl = index(destname, '/');
			if (sl == NULL) {
				fprintf(stderr, "Illegal ~user\n");
				exit(3);
			}
			*sl++ = 0;
			user = getpwnam(destname+1);
			if (user == NULL) {
				fprintf(stderr, "No such user as %s\n", destname);
				exit(4);
			}
			strcpy(dnbuf, user->pw_dir);
			strcat(dnbuf, "/");
			strcat(dnbuf, sl);
			destname = dnbuf;
		}
		out = fopen(destname, "w");
#ifdef DEBUG
		fprintf(dout, "local, file='%s'\n", destname);
#endif
		if (out == NULL) {
			perror(destname);
			exit(5);
		}
		if (mode > 0)
			chmod(destname, mode);	/* don't bother to check it */
	}

	/*
	 * Now, in any case, copy from in to out.
	 */

	while ((c=getc(in)) != EOF) {
		putc(c, out);
		count++;
	}
#ifdef DEBUG
	fprintf(dout, "count %d bytes\n", count);
	fclose(dout);
#endif
	
	fclose(in);
	fclose(out);	/* really should pclose in that case */
	exit(0);
}

/*
 * Return the ptr in sp at which the character c appears;
 * NULL if not found.  Included so I don't have to fight the
 * index/strchr battle.
 */

#define	NULL	0

char *
index(sp, c)
register char *sp, c;
{
	do {
		if (*sp == c)
			return(sp);
	} while (*sp++);
	return(NULL);
}

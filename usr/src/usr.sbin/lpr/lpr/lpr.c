/*
 *
 *      1.1 mike  5/77 - all files copied to spooler space
 *    	(see note below)
 *
 *      1.2 dlm 26 Sep 1977
 *      fix for allowing variable indents (-i flag)
 *
 *      1.3 dlm/njl 12 Dec 1977
 *      fix for open pipe when spawning progess (affects lpd)
 *
 *      1.4 dlm 24 Feb 1978
 *      test for data files; add -C option for classification
 *      and change format of $L card; reversed 1.1 on copies
 *
 *      1.5 dlm 27 Mar 1978
 *      add job name and -h option
 *
 *	2.0 was 21 Dec 1978
 *	-h option changed to -J
 *	added -H option to cause header page to be
 *	  printed, default is no header page
 *	added -p option to cause files to be printed
 *	  using pr
 *	default for classification changed
 *
 *	2.1 was 14 Mar 1979
 *	multiple printer logic changed,
 *	printer number sent to daemon.
 *
 *	3.0 sjl 6 May 1980
 *	Mods for Version 7, plus portability
 *	(in preparation for UNIX/24V)
 *
 *	3.1 sjl Oct 28 1980
 *	Mods for protected spooling area, see note below
 *
 *	3.2 sjl Mar 13 1981
 *	Add N card for file names to help out sq
 *
 *	3.3 sjl	Mar 27 1981 from decvax!shannon
 *	Mods and cleanup for 4bsd.
 *	Send mail option added.
 *
 *	4.0 sjl Mar 28 1981
 *	Support multiple printers and daemons through termcap-like
 *	  data base
 *
 *	4.1 sjl	Apr 28 1981
 *	Check for printer being down (mode 0 on device)
 *
 *	4.2 was May 1 1981
 *	Clean up handling of printcap database, add more defaults
 */

char lpr_id[] = "~|^`lpr.c:\t4.2\t1 May 1981\n";

/*	lpr.c	4.1	81/05/09	*/
/*
 *      lpr -- off line print
 *              also known as print
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <pwd.h>
#include <stdio.h>
#include <ctype.h>
#include "lp.local.h"

/*
 * Multiple printer scheme using info from printer data base:
 *
 *	DN		who to invoke to print stuff
 *	SA		rectory used as spool queue
 *
 * daemon identifies what printer it should use (in the case of the
 *  same code being shared) by inspecting its argv[1].
 */
char    *tfname;		/* tmp copy of df before linking */
char    *cfname;		/* copy files */
char    *lfname;		/* linked files */
char    *dfname;		/* daemon files, linked from tf's */

int	nact;			/* number of jobs to act on */
int	tff;			/* daemon file descriptor */
int     mailflg;		/* send mail */
int	jflag;			/* job name specified */
int	qflg;			/* q job, but don't exec daemon */
int	prflag;			/* ``pr'' files */
char	*person;		/* user name */
int	inchar;			/* location to increment char in file names */
int     ncopies = 1;		/* # of copies to make */
int	iflag;			/* indentation wanted */
int	indent;			/* amount to indent */
char	*daemname;		/* path name to daemon program */
char	*DN;			/* daemon name */
char	*SA;			/* spooling area */
char	*LP;			/* line printer device */
int     MX;			/* maximum size in blocks of a print file */
int	hdr = 1;		/* 1 =>'s default header */
int     user;			/* user id */
int	spgroup;		/* daemon's group for creating spool files */
char	*title;			/* pr'ing title */
char	*class = SYSTEM_NAME;	/* class title on header page */
char    *jobname;		/* job name on header page */
char	*name;			/* program name */
char	*printer;		/* printer name */

char	*pgetstr();
char	*mktmp();
char	*malloc();
char	*getenv();
char	*rindex();

main(argc, argv)
int argc;
char *argv[];
{
	register char *arg;
	int i, c, f, flag, out();
	char *sp;
	struct stat sbuf;

	/*
	 * Strategy to maintain protected spooling area:
	 *	1. Spooling area is writable only by daemon and spooling group
	 *	2. lpr runs setuid root and setgrp spooling group; it uses
	 *	   root to access any file it wants (verifying things before
	 *	   with an access call) and group id to know how it should
	 *	   set up ownership of files in spooling area.
	 *	3. Files in spooling area are owned by printer and spooling
	 *	   group, with mode 660.
	 *	4. lpd runs setuid daemon and setgrp spooling group to
	 *	   access files and printer.  Users can't get to anything
	 *	   w/o help of sq and dq programs.
	 */
	user = getuid();
	spgroup = getegid();
	if(signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, out);
	if(signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, out);
	if(signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		signal(SIGQUIT, out);
	if(signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, out);
	flag = 0;
	if ((printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;
	name = argv[0];

	while (argc>1 && (arg = argv[1])[0]=='-') {
		switch (arg[1]) {

		case 'c':		/* force copy of files */
			flag = '+';
			break;

		case 'C':		/* classification spec */
			hdr++;
			if (arg[2])
				class = &arg[2];
			else if (argc >= 2) {
				++argv;
				arg = argv[1];
				argc--;
				class = arg;
			}
			break;

		case 'r':		/* remove file when done */
			flag = '-';
			break;

		case 'm':		/* send mail when done */
			mailflg++;
			break;

		case 'q':		/* just q job */
			qflg++;
			break;

		case 'J':		/* job spec */
			jflag++, hdr++;
			if (arg[2]) {
				jobname = &arg[2];
				break;
			}
			if (argc>=2) {
				++argv;
				arg = argv[1];
				jobname = &arg[0];
				argc--;
			}
			break;

		case 'i':		/* indent output */
			iflag++;
			indent = arg[2] ? atoi(&arg[2]) : 8;
			break;

		case 'p':		/* use pr to print files */
			prflag++;
			break;

		case 'h':		/* pr's title line */
			if (arg[2])
				title = &arg[2];
			else if (argc >= 2) {
				++argv;
				arg = argv[1];
				argc--;
				title = arg;
			}
			break;

		case 'P':		/* specifiy printer name */
			printer = argv[2];
			break;

		case 'H':		/* toggle want of header page */
			hdr = !hdr;
			break;

		default:		/* n copies ? */
			if (isdigit(arg[1]))
				ncopies = atoi(&arg[1]);
		}
		argc--;
		argv++;
	}
	if (!chkprinter(printer)) {
		fprintf(stderr, "%s: no entry for default printer %s\n", name,
			printer);
		exit(2);
	}
	i = getpid();
	f = strlen(SA)+11;
	tfname = mktmp("tf", i, f);
	cfname = mktmp("cf", i, f);
	lfname = mktmp("lf", i, f);
	dfname = mktmp("df", i, f);
	inchar = f-7;
	tff = nfile(tfname);
	if (jflag == 0) {
		if(argc == 1)
			jobname = &dfname[f-10];
		else
			jobname = argv[1];
	}
	ident();

	if(argc == 1)
		copy(0, " ");
	else while(--argc) {
		if(test(arg = *++argv) == -1)	/* file reasonable */
			continue;

		if (flag == '+')		/* force copy flag */
			goto cf;
		if (stat(arg, &sbuf) < 0) {
			printf("lpr:");
			perror(arg);
			continue;
		}
		if((sbuf.st_mode&04) == 0)
			goto cf;
		if(*arg == '/' && flag != '-') {
			for(i=0;i<ncopies;i++) {
				if (prflag) {
					if (title)
						card('H', title);
					card('R', arg);
				} else
					card('F', arg);
				card('N', arg);
			}
			nact++;
			continue;
		}
		if(link(arg, lfname) < 0)
			goto cf;
		for(i=0;i<ncopies;i++) {
			if (prflag) {
				card('H', title ? title : arg);
				card('R', lfname);
			} else 
				card('F', lfname);
			card('N', arg);
		}
		card('U', lfname);
		lfname[inchar]++;
		nact++;
		goto df;

	cf:
		if((f = open(arg, 0)) < 0) {
			printf("%s: cannot open %s\n", name, arg);
			continue;
		}
		copy(f, arg);
		close(f);

	df:
		if(flag == '-' && unlink(arg))
			printf("%s: cannot remove %s\n", name, arg);
	}

	if(nact) {
		tfname[inchar]--;
		if(link(tfname, dfname) < 0) {
			printf("%s: cannot rename %s\n", name, dfname);
			tfname[inchar]++;
			out();
		}
		unlink(tfname);
		if (qflg)		/* just q things up */
			exit(0);
		if (stat(LP, &sbuf) >= 0 && (sbuf.st_mode&0777) == 0) {
			printf("job queued, but printer down\n");
			exit(0);
		}
		for(f = 0; f < NOFILE; close(f++))
			;
		open("/dev/tty", 0);
		open("/dev/tty", 1);
		dup2(1, 2);
		execl(DN, rindex(DN, '/') ? rindex(DN, '/')+1 : DN, printer, 0);
		dfname[inchar]++;
	}
	out();
}

copy(f, n)
int f;
char n[];
{
	int ff, i, nr, nc;
	char buf[BUFSIZ];

	for(i=0;i<ncopies;i++) {
		if (prflag) {
			card('H', title ? title : n);
			card('R', cfname);
		} else 
			card('F', cfname);
		card('N', n);
	}
	card('U', cfname);
	ff = nfile(cfname);
	nr = nc = 0;
	while((i = read(f, buf, BUFSIZ)) > 0) {
		if (write(ff, buf, i) != i) {
			printf("%s: %s: temp file write error\n", name, n);
			break;
		}
		nc += i;
		if(nc >= BUFSIZ) {
			nc -= BUFSIZ;
			if(nr++ > MX) {
				printf("%s: %s: copy file is too large\n", name, n);
				break;
			}
		}
	}
	close(ff);
	nact++;
}

card(c, p2)
register char c, *p2;
{
	char buf[BUFSIZ];
	register char *p1 = buf;
	int col = 0;

	*p1++ = c;
	while((c = *p2++) != '\0') {
		*p1++ = c;
		col++;
	}
	*p1++ = '\n';
	write(tff, buf, col+2);
}

ident()
{
	extern char *getlogin();
	extern struct passwd *getpwuid();
	struct passwd *pw;
	extern char *itoa();

	if ((person = getlogin()) == NULL) {
		if ((pw = getpwuid(user)) == NULL)
			person = "Unknown User";
		else
			person = pw->pw_name;
	}

	if (hdr) {
		card('J',jobname);
		card('C',class);
		card('L', person);
	}
	if (iflag)
		card('I', itoa(indent));
	if (mailflg)
		card('M', person);
}

nfile(n)
char *n;
{
	register f;

	if((f = creat(n, FILMOD)) < 0) {
		printf("%s: cannot create %s\n", name, n);
		out();
	}
	if (chown(n, user, spgroup) < 0) {
		unlink(n);
		printf("%s: cannot chown %s\n", name, n);
		out();
	}
	n[inchar]++;
	return(f);
}

out()
{
	register i;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	i = inchar;
	if (tfname)
		while(tfname[i] != 'A') {
			tfname[i]--;
			unlink(tfname);
		}
	if (cfname)
		while(cfname[i] != 'A') {
			cfname[i]--;
			unlink(cfname);
		}
	if (lfname)
		while(lfname[i] != 'A') {
			lfname[i]--;
			unlink(lfname);
		}
	if (dfname)
		while(dfname[i] != 'A') {
			dfname[i]--;
			unlink(dfname);
		}
	exit();
}

test(file)
char *file;
{
	struct exec buf;
	struct stat mbuf;
	int fd;

	if (access(file, 4) < 0) {
		printf("%s: cannot access %s\n", name, file);
		return(-1);
	}
	if(stat(file, &mbuf) < 0) {
		printf("%s: cannot stat %s\n", name, file);
		return (-1);
	}
	if ((mbuf.st_mode&S_IFMT) == S_IFDIR) {
		printf("%s: %s is a directory\n", name, file);
		return(-1);
	}

	if((fd = open(file, 0)) < 0) {
		printf("%s: cannot open %s\n", name, file);
		return(-1);
	}
	if (read(fd, &buf, sizeof(buf)) == sizeof(buf))
		switch(buf.a_magic) {
		case A_MAGIC1:
		case A_MAGIC2:
		case A_MAGIC3:
#ifdef A_MAGIC4
		case A_MAGIC4:
#endif
			printf("%s: %s is an executable program", name, file);
			goto error1;

		case ARMAG:
			printf("%s: %s is an archive file", name, file);
			goto error1;
		}

	close(fd);
	return(0);
error1:
	printf(" and is unprintable\n");
	close(fd);
	return(-1);
}

/*
 * itoa - integer to string conversion
 */
char *
itoa(i)
register int i;
{
	static char b[10] = "########";
	register char *p;

	p = &b[8];
	do
		*p-- = i%10 + '0';
	while (i /= 10);
	return(++p);
}

/*
 * Perform lookup for printer name or abbreviation --
 *   return pointer to daemon structure
 */
chkprinter(s)
register char *s;
{
	static char buf[BUFSIZ/2];
	char b[BUFSIZ];
	int stat;
	char *bp = buf;

	if ((stat = pgetent(b, s)) < 0) {
		fprintf(stderr, "%s: can't open printer description file\n", name);
		exit(3);
	} else if (stat == 0)
		return(NULL);
	if ((DN = pgetstr("dn", &bp)) == NULL)
		DN = DEFDAEMON;
	if ((LP = pgetstr("lp", &bp)) == NULL)
		LP = DEFDEVLP;
	if ((SA = pgetstr("sa", &bp)) == NULL)
		SA = DEFSPOOL;
	if ((MX = pgetnum("mx")) < 0)
		MX = DEFMX;
	return(1);
}

/*
 * Make a temp file
 */
char *
mktmp(id, pid, n)
char *id;
{
	register char *s;

	if ((s = malloc(n)) == NULL) {
		fprintf(stderr, "%s: out of memory\n", name);
		exit(1);
	}
	sprintf(s, "%s/%sA%05d", SA, id, pid);
	return(s);
}

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

/*	lpr.c	4.16	83/05/13	*/
/*
 *      lpr -- off line print
 *
 * Allows multiple printers and printers on remote machines by
 * using information from a printer data base.
 */

#include "lp.h"

char    *tfname;		/* tmp copy of cf before linking */
char    *cfname;		/* daemon control files, linked from tf's */
char    *dfname;		/* data files */

int	nact;			/* number of jobs to act on */
int	tfd;			/* control file descriptor */
int     mailflg;		/* send mail */
int	qflag;			/* q job, but don't exec daemon */
char	format = 'f';		/* format char for printing files */
int	rflag;			/* remove files upon completion */	
int	sflag;			/* symbolic link flag */
int	inchar;			/* location to increment char in file names */
int     ncopies = 1;		/* # of copies to make */
int	iflag;			/* indentation wanted */
int	indent;			/* amount to indent */
int	hdr = 1;		/* print header or not (default is yes) */
int     userid;			/* user id */
char	*person;		/* user name */
char	*title;			/* pr'ing title */
char	*fonts[4];		/* troff font names */
char	*width;			/* width for versatec printing */
char	*class = host;		/* class title on header page */
char    *jobname;		/* job name on header page */

char	*linked();
int	cleanup();

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char *argv[];
{
	extern struct passwd *getpwuid();
	struct passwd *pw;
	extern char *itoa();
	register char *arg, *cp;
	int i, f;
	struct stat stb;

	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, cleanup);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, cleanup);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		signal(SIGQUIT, cleanup);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, cleanup);

	name = argv[0];
	gethostname(host, sizeof (host));

	while (argc > 1 && argv[1][0] == '-') {
		argc--;
		arg = *++argv;
		switch (arg[1]) {

		case 'P':		/* specifiy printer name */
			printer = &arg[2];
			break;

		case 'C':		/* classification spec */
			hdr++;
			if (arg[2])
				class = &arg[2];
			else if (argc > 1) {
				argc--;
				class = *++argv;
			}
			break;

		case 'J':		/* job name */
			hdr++;
			if (arg[2])
				jobname = &arg[2];
			else if (argc > 1) {
				argc--;
				jobname = *++argv;
			}
			break;

		case 'T':		/* pr's title line */
			if (arg[2])
				title = &arg[2];
			else if (argc > 1) {
				argc--;
				title = *++argv;
			}
			break;

		case 'l':		/* literal output */
		case 'p':		/* print using ``pr'' */
		case 't':		/* print troff output (cat files) */
		case 'd':		/* print tex output (dvi files) */
		case 'g':		/* print graph(1G) output */
		case 'c':		/* print cifplot output */
		case 'v':		/* print vplot output */
			format = arg[1];
			break;

		case 'f':		/* print fortran output */
			format = 'r';
			break;

		case '4':		/* troff fonts */
		case '3':
		case '2':
		case '1':
			if (argc > 1) {
				argc--;
				fonts[arg[1] - '1'] = *++argv;
				format = 't';
			}
			break;

		case 'w':		/* versatec page width */
			width = arg+2;
			break;

		case 'r':		/* remove file when done */
			rflag++;
			break;

		case 'm':		/* send mail when done */
			mailflg++;
			break;

		case 'h':		/* toggle want of header page */
			hdr = !hdr;
			break;

		case 's':		/* try to link files */
			sflag++;
			break;

		case 'q':		/* just q job */
			qflag++;
			break;

		case 'i':		/* indent output */
			iflag++;
			indent = arg[2] ? atoi(&arg[2]) : 8;
			break;

		case '#':		/* n copies */
			if (isdigit(arg[2]))
				ncopies = atoi(&arg[2]);
		}
	}
	if (printer == NULL && (printer = getenv("PRINTER")) == NULL)
		printer = DEFLP;
	chkprinter(printer);
	/*
	 * Check to make sure queuing is enabled.
	 */
	(void) sprintf(line, "%s/%s", SD, LO);
	if (stat(line, &stb) == 0 && (stb.st_mode & 010))
		fatal("Printer queue is disabled");
	/*
	 * Get the identity of the person doing the lpr using the same
	 * algorithm as lprm. 
	 */
	userid = getuid();
	if ((pw = getpwuid(userid)) == NULL)
		fatal("Who are you?");
	person = pw->pw_name;
	/*
	 * Initialize the control file.
	 */
	mktemps();
	tfd = nfile(tfname);
	card('H', host);
	card('P', person);
	if (hdr) {
		if (jobname == NULL) {
			if (argc == 1)
				jobname = "stdin";
			else
				jobname = (arg = rindex(argv[1], '/')) ? arg+1 : argv[1];
		}
		card('J', jobname);
		card('C', class);
		card('L', person);
	}
	if (iflag)
		card('I', itoa(indent));
	if (mailflg)
		card('M', person);
	if (format == 't' || format == 'd')
		for (i = 0; i < 4; i++)
			if (fonts[i] != NULL)
				card('1'+i, fonts[i]);
	if (width != NULL)
		card('W', width);

	/*
	 * Read the files and spool them.
	 */
	if (argc == 1)
		copy(0, " ");
	else while (--argc) {
		if ((f = test(arg = *++argv)) < 0)
			continue;	/* file unreasonable */

		if (sflag && (cp = linked(arg)) != NULL) {
			if (format == 'p')
				card('T', title ? title : arg);
			for (i = 0; i < ncopies; i++)
				card(format, &dfname[inchar-2]);
			card('U', &dfname[inchar-2]);
			if (f)
				card('U', cp);
			card('N', arg);
			dfname[inchar]++;
			nact++;
			continue;
		}
		if (sflag)
			printf("%s: %s: not linked, copying instead\n", name, arg);
		if ((i = open(arg, 0)) < 0) {
			printf("%s: cannot open %s\n", name, arg);
			continue;
		}
		copy(i, arg);
		(void) close(i);
		if (f && unlink(arg) < 0)
			printf("%s: %s: not removed\n", name, arg);
	}

	if (nact) {
		(void) close(tfd);
		tfname[inchar]--;
		/*
		 * Touch the control file to fix position in the queue.
		 */
		if ((tfd = open(tfname, 2)) >= 0) {
			char c;

			if (read(tfd, &c, 1) == 1 && lseek(tfd, 0L, 0) == 0 &&
			    write(tfd, &c, 1) != 1) {
				printf("%s: cannot touch %s\n", name, tfname);
				tfname[inchar]++;
				cleanup();
			}
			(void) close(tfd);
		}
		if (link(tfname, cfname) < 0) {
			printf("%s: cannot rename %s\n", name, cfname);
			tfname[inchar]++;
			cleanup();
		}
		unlink(tfname);
		if (qflag)		/* just q things up */
			exit(0);
		if (!startdaemon())
			printf("jobs queued, but cannot start daemon.\n");
		exit(0);
	}
	cleanup();
	/* NOTREACHED */
}

/*
 * Create the file n and copy from file descriptor f.
 */
copy(f, n)
	int f;
	char n[];
{
	register int fd, i, nr, nc;
	char buf[BUFSIZ];

	if (format == 'p')
		card('T', title ? title : n);
	for (i = 0; i < ncopies; i++)
		card(format, &dfname[inchar-2]);
	card('U', &dfname[inchar-2]);
	card('N', n);
	fd = nfile(dfname);
	nr = nc = 0;
	while ((i = read(f, buf, BUFSIZ)) > 0) {
		if (write(fd, buf, i) != i) {
			printf("%s: %s: temp file write error\n", name, n);
			break;
		}
		nc += i;
		if (nc >= BUFSIZ) {
			nc -= BUFSIZ;
			nr++;
			if (MX > 0 && nr > MX) {
				printf("%s: %s: copy file is too large\n", name, n);
				break;
			}
		}
	}
	(void) close(fd);
	nact++;
}

/*
 * Try and link the file to dfname. Return a pointer to the full
 * path name if successful.
 */
char *
linked(file)
	register char *file;
{
	register char *cp;
	static char buf[BUFSIZ];

	if (*file != '/') {
		if (getwd(buf) == NULL)
			return(NULL);
		while (file[0] == '.') {
			switch (file[1]) {
			case '/':
				file += 2;
				continue;
			case '.':
				if (file[2] == '/') {
					if ((cp = rindex(buf, '/')) != NULL)
						*cp = '\0';
					file += 3;
					continue;
				}
			}
			break;
		}
		strcat(buf, "/");
		strcat(buf, file);
		file = buf;
	}
	return(symlink(file, dfname) ? NULL : file);
}

/*
 * Put a line into the control file.
 */
card(c, p2)
	register char c, *p2;
{
	char buf[BUFSIZ];
	register char *p1 = buf;
	register int len = 2;

	*p1++ = c;
	while ((c = *p2++) != '\0') {
		*p1++ = c;
		len++;
	}
	*p1++ = '\n';
	write(tfd, buf, len);
}

/*
 * Create a new file in the spool directory.
 */
nfile(n)
	char *n;
{
	register f;
	int oldumask = umask(0);		/* should block signals */

	f = creat(n, FILMOD);
	(void) umask(oldumask);
	if (f < 0) {
		printf("%s: cannot create %s\n", name, n);
		cleanup();
	}
	if (++n[inchar] > 'z') {
		if (++n[inchar-2] == 't') {
			printf("too many files - break up the job\n");
			cleanup();
		}
		n[inchar] = 'A';
	} else if (n[inchar] == '[')
		n[inchar] = 'a';
	return(f);
}

/*
 * Cleanup after interrupts and errors.
 */
cleanup()
{
	register i;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	i = inchar;
	if (tfname)
		do
			unlink(tfname);
		while (tfname[i]-- != 'A');
	if (cfname)
		do
			unlink(cfname);
		while (cfname[i]-- != 'A');
	if (dfname)
		do {
			do
				unlink(dfname);
			while (dfname[i]-- != 'A');
			dfname[i] = 'z';
		} while (dfname[i-2]-- != 'd');
	exit(1);
}

/*
 * Test to see if this is a printable file.
 * Return -1 if it is not, 0 if its printable, and 1 if
 * we should remove it after printing.
 */
test(file)
	char *file;
{
	struct exec execb;
	struct stat statb;
	register int fd;
	register char *cp;

	if (access(file, 4) < 0) {
		printf("%s: cannot access %s\n", name, file);
		return(-1);
	}
	if (stat(file, &statb) < 0) {
		printf("%s: cannot stat %s\n", name, file);
		return(-1);
	}
	if ((statb.st_mode & S_IFMT) == S_IFDIR) {
		printf("%s: %s is a directory\n", name, file);
		return(-1);
	}
	if ((fd = open(file, 0)) < 0) {
		printf("%s: cannot open %s\n", name, file);
		return(-1);
	}
	if (read(fd, &execb, sizeof(execb)) == sizeof(execb))
		switch(execb.a_magic) {
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
	(void) close(fd);
	if (rflag) {
		if ((cp = rindex(file, '/')) == NULL) {
			if (access(".", 2) == 0)
				return(1);
		} else {
			*cp = '\0';
			fd = access(file, 2);
			*cp = '/';
			if (fd == 0)
				return(1);
		}
		printf("%s: %s: is not removable by you\n", name, file);
	}
	return(0);

error1:
	printf(" and is unprintable\n");
	(void) close(fd);
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
 */
chkprinter(s)
	register char *s;
{
	int status;

	if ((status = pgetent(line, s)) < 0)
		fatal("cannot open printer description file");
	else if (status == 0)
		fatal("unknown printer");
	if ((SD = pgetstr("sd", &bp)) == NULL)
		SD = DEFSPOOL;
	if ((LO = pgetstr("lo", &bp)) == NULL)
		LO = DEFLOCK;
	if ((MX = pgetnum("mx")) < 0)
		MX = DEFMX;
	if ((DU = pgetnum("du")) < 0)
		DU = DEFUID;
	RM = host;		/* machine for getport to connect to */
}

/*
 * Make the temp files.
 */
mktemps()
{
	register int c, len;
	int n;
	char buf[BUFSIZ], *mktemp();
	FILE *fp;

	(void) sprintf(buf, "%s/.seq", SD);
	if ((fp = fopen(buf, "r+")) == NULL) {
		if ((fp = fopen(buf, "w")) == NULL) {
			printf("%s: cannot create %s\n", name, buf);
			exit(1);
		}
		setbuf(fp, buf);
		n = 0;
	} else {
		setbuf(fp, buf);
		if (flock(fileno(fp), FEXLOCK)) {
			printf("%s: cannot lock %s\n", name, buf);
			exit(1);
		}
		n = 0;
		while ((c = getc(fp)) >= '0' && c <= '9')
			n = n * 10 + (c - '0');
	}
	len = strlen(SD) + strlen(host) + 8;
	tfname = mktemp("tf", n, len);
	cfname = mktemp("cf", n, len);
	dfname = mktemp("df", n, len);
	inchar = strlen(SD) + 3;
	n = (n + 1) % 1000;
	(void) fseek(fp, 0L, 0);
	fprintf(fp, "%d\n", n);
	(void) fclose(fp);
}

/*
 * Make a temp file name.
 */
char *
mktemp(id, num, len)
	char	*id;
	int	num, len;
{
	register char *s;

	if ((s = malloc(len)) == NULL)
		fatal("out of memory");
	(void) sprintf(s, "%s/%sA%03d%s", SD, id, num, host);
	return(s);
}

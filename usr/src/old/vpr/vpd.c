#define	CONSOLE		"/dev/console"
#define	dprcons		if (debug) prcons
/*
 * vpd.c						updated 11/4/87
 * Varian or Versatec printer daemon
 */
char vpdSCCSid[] = "@(#)vpd.c	1.5\t11/4/87";

#include <stdio.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/stat.h>
#include <sgtty.h>
#include <errno.h>
#include <sys/vcmd.h>
#include <sys/wait.h>

int	debug;
extern	int errno;

#define VRAST		"/usr/local/lib/vrast"
#define VDMP		"/usr/lib/vdmp"
#define VPLTDMP		"/usr/lib/vpltdmp"

#ifdef VARIAN
#define DEVICE		"/dev/va0"
#define DFNAME		"/usr/spool/vad/"
#define SPOOLDIR	"/usr/spool/vad"
#define NAME		"Varian"
#endif

#ifdef VERSATEC
#define DEVICE		"/dev/vp0"
#define DFNAME		"/usr/spool/vpd/"
#define SPOOLDIR	"/usr/spool/vpd"
#define NAME		"Versatec"
#endif

int	prtmode[] =	{VPRINT, 0, 0};

char	line[128];
char    linep[127];
char	banbuf[64];
char	banner[512];
char	printflag;
int	linel;
FILE	*dfb;
char	dfname[33] = DFNAME;
int	waittm = 6;
int	onalrm ();
char	tmplock[] = "lockXXXXXX";
char	fonts[4][50] = {
	"/usr/lib/vfont/R",
	"/usr/lib/vfont/I",
	"/usr/lib/vfont/B",
	"/usr/lib/vfont/S"
};

main(argc, argv)
	int argc;
	char **argv;
{
	char n;
	register char *p1, *p2;
	register int df;
	register struct direct *dirp;
	DIR *dp;
	struct stat stb;
	int offline = 0;
	int i, okreque = 1;

	while (argc > 1) {
		argc--;
		argv++;
		if (argv[0][0] != '-')
			continue;
		switch (argv[0][1]) {
		case 'n':
			if (argc < 2)
				break;
			argv++;
			argc--;
			nice(atol(argv[0]));
			break;
		}
	}
	setuid(getuid());
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
begin:
/*
 * Close all files, open root as 0, 1, 2
 * to assure standard environment
 */
	for (df = 0; df <= 15; df++)
		close(df);
	open("/", 0);
	dup(0);
	dup(0);
	if (chdir(SPOOLDIR) < 0 || (df = creat(mktemp(tmplock), 0)) < 0) {
		close(1);
		prcons("%s: error accessing %s\n", NAME, SPOOLDIR);
		exit(1);
	}
	if (link(tmplock, "lock") < 0) {
		unlink(tmplock);
		exit(0);
	}
	unlink(tmplock);
	close(df);
floop:
	dprcons("floop\n");
	i = fork();
	if (i < 0) {
		sleep(5);
		goto floop;
	}
	if (i != 0)
		exit(0);
reopen:
	dprcons("reopen\n");
	for (;;) {
		if (open(DEVICE, 1) == 3)
			break;
		if (errno != EIO) {
			extern char *sys_errlist[];
			prcons("%s: %s: %s\n", NAME, DEVICE, sys_errlist[errno]);
			unlink("lock");
			exit(1);
		}
		if (offline == 0) {
			int f = open("/dev/tty", 1);

			offline++;
			if (f > 0) {
				write(f, NAME, strlen(NAME));
				write(f, " is offline\n", 12);
				close(f);
			}
			dprcons("offline\n");
		}
		sleep(10);
	}
	dp = opendir(".");
search:
	dprcons("search\n");
	if (okreque == 1) {
		rewinddir(dp);
		do {
			dirp = readdir(dp);
			if (dirp == NULL) {
				if (printflag)
					lastpage();
				unlink("lock");
				dprcons("nomore\n");
				if (printflag==0) {
					dprcons("bye\n");
					exit(0);
				}
				dprcons("one last time\n");
				printflag = 0;
				close(3);
				closedir(dp);
				sleep(30);
				goto begin;
			}
		} while (dirp->d_name[0] != 'd' || dirp->d_name[1] != 'f');
		strcpy(&dfname[15], dirp->d_name);
		dprcons("found %s\n", dirp->d_name);
	}
	dprcons("trying %s\n", dfname);
	printflag = 1;
	if (okreque == 0)
		feedpage();
	if (trysend(dfname, okreque)) {
		okreque = 0;
		closedir(dp);
		printf("reque %s\n", dfname);
		close(3);
		goto reopen;
	}
	dprcons("ok\n");
	okreque = 1;
	goto search;
}

trysend(file, okreque)
char *file;
int okreque;
{
	register int i;
	char plot;
	union wait status;
	int bomb = 0;

	resfonts();
	dfb = fopen(file, "r");
	if (dfb == NULL) {
		unlink(file);
		return (0);
	}
	banner[0] = '\0';
	for(*banbuf= plot= 0; getline();) switch(line[0]) {

	case 'L':
		strcpy(banbuf, line + 1);
		continue;

	case 'B':	/* banner */
		if(banner[0] == '\0')
		    strcat(banner, line + 1);
		  else {
		    strcat(banner,"\n");
		    strcat(banner, line+1);
		    }
		continue;

	case '1':
	case '2':
	case '3':
	case '4':
		strcpy(fonts[line[0]-'1'], line + 1);
		continue;

	case 'C':	/* called by cifplot */
	case 'V':	/* called by vplot */
	case 'F':
	case 'G':	/* Like f, but invoke vpf with -l flag. */
	case 'T':
		status.w_status = send(line[0]);
		break;

	case 'P':
		if (plot) {
			plot = 0;
			status.w_status = send(line[0]);
			break;
		}
		strcpy(linep, line + 1);
		plot++;
		continue;

	case 'U':
		continue;

	case 'M':
		continue;
	}
	/*
	 * If the process that did the work did an exit(1),
	 * we should requeue the file.
	 */
	if (!WIFEXITED(status) || status.w_retcode > 1) {
		ioctl(3, VSETSTATE, prtmode);
		write(3, "\nDAEMON MALFUNCTION\n", 20);
		feedpage();
		bomb++;
	} else if (status.w_retcode == 1 && okreque)
		return (1);
	/*
	 * All done, for better or for worse.
	 */
	fseek(dfb, 0, 0);
	while (getline()) switch (*line) {

	default:
		continue;

	case 'U':
		unlink(line + 1);
		continue;

	case 'M':
		sendmail(bomb);
		continue;
	}
remret:
	fclose(dfb);
	unlink(file);
	return (0);
}

static char ifonts[4][50] = {
	"/usr/lib/vfont/R",
	"/usr/lib/vfont/I",
	"/usr/lib/vfont/B",
	"/usr/lib/vfont/S"
};

resfonts()
{
	int i;
	for (i = 0; i < 4; i++)
		strcpy(fonts[i], ifonts[i]);
}

sendmail(bomb)
	int bomb;
{
	static int p[2];
	register i;
	int stat;

	pipe(p);
	if (fork() == 0) {
		alarm(0);
		close(0);
		dup(p[0]);
		for (i = 3; i <= 15; i++)
			close(i);
		execl("/bin/mail", "mail", &line[1], 0);
		exit(0);
	}
	close(1);
	dup(p[1]);
	printf("Your %s job %s\n", NAME, bomb ? "screwed up" : "is done");
	close(1);
	close(p[0]);
	close(p[1]);
	open("/", 0);
	wait(&stat);
}

getline()
{
	register char *lp;
	register int c;

	lp = line;
	linel = 0;
	while ((c = getc (dfb)) != '\n') {
		if (c < 0)
			return (0);
		if (c == '\t') {
			do {
				*lp++ = ' ';
				linel++;
			} while ((linel & 07) != 0);
			continue;
		}
		*lp++ = c;
		linel++;
	}
	*lp++ = 0;
	return (1);
}

int	pid;

send (c)
char c;
{
	int p, i, rm;

	if (pid = fork ()) {
		if (pid == -1)
			return (1);
		setexit();
		signal(SIGALRM, onalrm);
		alarm(30);
		wait(&p);
		alarm(0);
		return(p);
	}
	ioctl (3, VSETSTATE, prtmode);
	switch (c) {

	case 'F':
		if (banbuf[0]) {
			execl ("/usr/lib/vpf", "vpf",
#ifdef VERSATEC
				"-W",
#endif
				"-b", banbuf, line+1, 0);
			break;
		}
		execl ("/usr/lib/vpf", "vpf",
#ifdef VERSATEC
			"-W",
#endif
			line, 0);
		break;

	case 'G':	/* Like F (vpf), but passes through -l
			   flag to vpf (print control chars). */
		if (banbuf[0]) {
			execl ("/usr/lib/vpf", "vpf", "-l",
#ifdef VERSATEC
				"-W",
#endif
				"-b", banbuf, line+1, 0);
			break;
		}
		execl ("/usr/lib/vpf", "vpf", "-l",
#ifdef VERSATEC
			"-W",
#endif
			line, 0);
		break;

	case 'T':
		unlink(".railmag");
		rm = creat(".railmag", 0666);
		for (i = 0; i < 4; i++) {
			if (fonts[i][0] != '/')
				write(rm, "/usr/lib/vfont/", 15);
			write(rm, fonts[i], strlen (fonts[i]));
			write(rm, "\n", 1);
		}
		close(rm);
		if (banbuf[0]) {
#ifdef VARIAN
			execl("/usr/lib/rvcat", "rvcat",
#endif
#ifdef VERSATEC
			execl("/usr/lib/vcat", "rvcat",
				"-W",
#endif
				"-3", "-b", banbuf, line+1, 0);
			break;
		}
#ifdef VARIAN
		execl("/usr/lib/rvcat", "rvcat",
#endif
#ifdef VERSATEC
		execl("/usr/lib/vcat", "rvcat",
			"-W",
#endif
			"-3", line+1, 0);
		break;

	case 'P':
		close(1);
		dup(3);
		if (banbuf[0]) {
			execl(VRAST, "vrast",
#ifdef VERSATEC
				"-W",
#endif
				"-v", "-b", banbuf, line+1, linep, 0);
			break;
		}
		execl(VRAST, "vrast",
#ifdef VERSATEC
			"-W",
#endif
			"-v", line+1, linep, 0);
		break;
	case 'C':
		execl(VDMP,"vdmp","-n",banbuf,"-b",banner,
#ifdef VERSATEC
			"-W",
#endif
			line+1,0);
		break;
	case 'V':
		execl(VPLTDMP,"vpltdmp","-n",banbuf,"-b",banner,
#ifdef VERSATEC
			"-W",
#endif
			line+1,0);
		break;
	}
	exit(2);	/* execl failed or not one of above cases. */
}

onalrm()
{
	struct stat stb;

	signal(SIGALRM, onalrm);
	if (stat(dfname, &stb) < 0)
		kill(pid, SIGEMT);
	reset();
}

/*
 * skip 16 inches or do two formfeeds.
 */
lastpage()
{
	register int i;

	ioctl(3, VSETSTATE, prtmode);
#ifdef VARIAN
	write(3, "\014\014", 2);
#endif
#ifdef VERSATEC
	for (i = 0; i < 18; i++)
		write(3, "\n\n\n\n\n\n\n\n", 8);
#endif
}

feedpage()
{

	ioctl(3, VSETSTATE, prtmode);
#ifdef VARIAN
	write(3, "\014\0", 2);
#endif
#ifdef VERSATEC
	write(3, "\n\n\n", 8);
#endif
}

prcons(cp, a1, a2, a3, a4, a5)
	char *cp;
{
	char buf[BUFSIZ];
	int f = open(CONSOLE, 1);

	sprintf(buf, cp, a1, a2, a3, a4, a5);
	write(f, buf, strlen(buf));
	close(f);
}

#ifndef lint
static char sccsid[] = "@(#)mail.local.c	4.31 (Berkeley) %G%";
#endif

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>

#include <ctype.h>
#include <stdio.h>
#include <pwd.h>
#include <utmp.h>
#include <signal.h>
#include <setjmp.h>
#include <sysexits.h>

#define SENDMAIL	"/usr/lib/sendmail"

	/* copylet flags */
#define REMOTE		1		/* remote mail, add rmtmsg */
#define ORDINARY	2
#define ZAP		3		/* zap header and trailing empty line */
#define	FORWARD		4

#define	LSIZE		256
#define	MAXLET		300		/* maximum number of letters */
#define	MAILMODE	0600		/* mode of created mail */

char	line[LSIZE];
char	resp[LSIZE];
struct let {
	long	adr;
	char	change;
} let[MAXLET];
int	nlet	= 0;
char	lfil[50];
long	iop, time();
char	*getenv();
char	*index();
char	lettmp[] = "/tmp/maXXXXX";
char	maildir[] = "/usr/spool/mail/";
char	mailfile[] = "/usr/spool/mail/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
char	dead[] = "dead.letter";
char	forwmsg[] = " forwarded\n";
FILE	*tmpf;
FILE	*malf;
char	my_name[60];
char	*getlogin();
int	error;
int	changed;
int	forward;
char	from[] = "From ";
long	ftell();
int	delex();
char	*ctime();
int	flgf;
int	flgp;
int	delflg = 1;
int	hseqno;
jmp_buf	sjbuf;
int	rmail;

main(argc, argv)
char **argv;
{
	register int i;
	char *name;
	struct passwd *pwent;

	if (!(name = getlogin()) || !*name || !(pwent = getpwnam(name)) ||
	    getuid() != pwent->pw_uid) 
		pwent = getpwuid(getuid());
	strncpy(my_name, pwent ? pwent->pw_name : "???", sizeof(my_name)-1);
	if (setjmp(sjbuf))
		done();
	for (i=SIGHUP; i<=SIGTERM; i++)
		setsig(i, delex);
	i = mkstemp(lettmp);
	tmpf = fdopen(i, "r+w");
	if (i < 0 || tmpf == NULL)
		panic("mail: %s: cannot open for writing", lettmp);
	/*
	 * This protects against others reading mail from temp file and
	 * if we exit, the file will be deleted already.
	 */
	unlink(lettmp);
	if (argv[0][0] == 'r')
		rmail++;
	if (argv[0][0] != 'r' &&	/* no favors for rmail*/
	   (argc == 1 || argv[1][0] == '-' && !any(argv[1][1], "rhd")))
		printmail(argc, argv);
	else
		bulkmail(argc, argv);
	done();
}

setsig(i, f)
int i;
int (*f)();
{
	if (signal(i, SIG_IGN) != SIG_IGN)
		signal(i, f);
}

any(c, str)
	register int c;
	register char *str;
{

	while (*str)
		if (c == *str++)
			return(1);
	return(0);
}

printmail(argc, argv)
	char **argv;
{
	int flg, i, j, print;
	char *p, *getarg();
	struct stat statb;

	setuid(getuid());
	cat(mailfile, maildir, my_name);
#ifdef notdef
	if (stat(mailfile, &statb) >= 0
	    && (statb.st_mode & S_IFMT) == S_IFDIR) {
		strcat(mailfile, "/");
		strcat(mailfile, my_name);
	}
#endif
	for (; argc > 1; argv++, argc--) {
		if (argv[1][0] != '-')
			break;
		switch (argv[1][1]) {

		case 'p':
			flgp++;
			/* fall thru... */
		case 'q':
			delflg = 0;
			break;

		case 'f':
			if (argc >= 3) {
				strcpy(mailfile, argv[2]);
				argv++, argc--;
			}
			break;

		case 'b':
			forward = 1;
			break;

		default:
			panic("unknown option %c", argv[1][1]);
			/*NOTREACHED*/
		}
	}
	malf = fopen(mailfile, "r");
	if (malf == NULL) {
		printf("No mail.\n");
		return;
	}
	flock(fileno(malf), LOCK_SH);
	copymt(malf, tmpf);
	fclose(malf);			/* implicit unlock */
	fseek(tmpf, 0, L_SET);

	changed = 0;
	print = 1;
	for (i = 0; i < nlet; ) {
		j = forward ? i : nlet - i - 1;
		if (setjmp(sjbuf)) {
			print = 0;
		} else {
			if (print)
				copylet(j, stdout, ORDINARY);
			print = 1;
		}
		if (flgp) {
			i++;
			continue;
		}
		setjmp(sjbuf);
		fputs("? ", stdout);
		fflush(stdout);
		if (fgets(resp, LSIZE, stdin) == NULL)
			break;
		switch (resp[0]) {

		default:
			printf("usage\n");
		case '?':
			print = 0;
			printf("q\tquit\n");
			printf("x\texit without changing mail\n");
			printf("p\tprint\n");
			printf("s[file]\tsave (default mbox)\n");
			printf("w[file]\tsame without header\n");
			printf("-\tprint previous\n");
			printf("d\tdelete\n");
			printf("+\tnext (no delete)\n");
			printf("m user\tmail to user\n");
			printf("! cmd\texecute cmd\n");
			break;

		case '+':
		case 'n':
		case '\n':
			i++;
			break;
		case 'x':
			changed = 0;
		case 'q':
			goto donep;
		case 'p':
			break;
		case '^':
		case '-':
			if (--i < 0)
				i = 0;
			break;
		case 'y':
		case 'w':
		case 's':
			flg = 0;
			if (resp[1] != '\n' && resp[1] != ' ') {
				printf("illegal\n");
				flg++;
				print = 0;
				continue;
			}
			if (resp[1] == '\n' || resp[1] == '\0') {
				p = getenv("HOME");
				if (p != 0)
					cat(resp+1, p, "/mbox");
				else
					cat(resp+1, "", "mbox");
			}
			for (p = resp+1; (p = getarg(lfil, p)) != NULL; ) {
				malf = fopen(lfil, "a");
				if (malf == NULL) {
					printf("mail: %s: cannot append\n",
					    lfil);
					flg++;
					continue;
				}
				copylet(j, malf, resp[0]=='w'? ZAP: ORDINARY);
				fclose(malf);
			}
			if (flg)
				print = 0;
			else {
				let[j].change = 'd';
				changed++;
				i++;
			}
			break;
		case 'm':
			flg = 0;
			if (resp[1] == '\n' || resp[1] == '\0') {
				i++;
				continue;
			}
			if (resp[1] != ' ') {
				printf("invalid command\n");
				flg++;
				print = 0;
				continue;
			}
			for (p = resp+1; (p = getarg(lfil, p)) != NULL; )
				if (!sendmail(j, lfil, my_name))
					flg++;
			if (flg)
				print = 0;
			else {
				let[j].change = 'd';
				changed++;
				i++;
			}
			break;
		case '!':
			system(resp+1);
			printf("!\n");
			print = 0;
			break;
		case 'd':
			let[j].change = 'd';
			changed++;
			i++;
			if (resp[1] == 'q')
				goto donep;
			break;
		}
	}
   donep:
	if (changed)
		copyback();
}

/* copy temp or whatever back to /usr/spool/mail */
copyback()
{
	register i, c;
	int fd, new = 0, oldmask;
	struct stat stbuf;

#define	mask(s)	(1 << ((s) - 1))
	oldmask = sigblock(mask(SIGINT)|mask(SIGHUP)|mask(SIGQUIT));
#undef mask
	fd = open(mailfile, O_RDWR | O_CREAT, MAILMODE);
	if (fd >= 0) {
		flock(fd, LOCK_EX);
		malf = fdopen(fd, "r+w");
	}
	if (fd < 0 || malf == NULL)
		panic("can't rewrite %s", lfil);
	fstat(fd, &stbuf);
	if (stbuf.st_size != let[nlet].adr) {	/* new mail has arrived */
		fseek(malf, let[nlet].adr, L_SET);
		fseek(tmpf, let[nlet].adr, L_SET);
		while ((c = getc(malf)) != EOF)
			putc(c, tmpf);
		let[++nlet].adr = stbuf.st_size;
		new = 1;
		fseek(malf, 0, L_SET);
	}
	ftruncate(fd, 0);
	for (i = 0; i < nlet; i++)
		if (let[i].change != 'd')
			copylet(i, malf, ORDINARY);
	fclose(malf);		/* implict unlock */
	if (new)
		printf("New mail has arrived.\n");
	sigsetmask(oldmask);
}

/* copy mail (f1) to temp (f2) */
copymt(f1, f2)
	FILE *f1, *f2;
{
	long nextadr;

	nlet = nextadr = 0;
	let[0].adr = 0;
	while (fgets(line, LSIZE, f1) != NULL) {
		if (isfrom(line))
			let[nlet++].adr = nextadr;
		nextadr += strlen(line);
		fputs(line, f2);
	}
	let[nlet].adr = nextadr;	/* last plus 1 */
}

copylet(n, f, type)
	FILE *f;
{
	int ch;
	long k;
	char hostname[MAXHOSTNAMELEN];

	fseek(tmpf, let[n].adr, L_SET);
	k = let[n+1].adr - let[n].adr;
	while (k-- > 1 && (ch = getc(tmpf)) != '\n')
		if (type != ZAP)
			putc(ch, f);
	switch (type) {

	case REMOTE:
		gethostname(hostname, sizeof (hostname));
		fprintf(f, " remote from %s\n", hostname);
		break;

	case FORWARD:
		fprintf(f, forwmsg);
		break;

	case ORDINARY:
		putc(ch, f);
		break;

	case ZAP:
		break;

	default:
		panic("Bad letter type %d to copylet.", type);
	}
	while (k-- > 1) {
		ch = getc(tmpf);
		putc(ch, f);
	}
	if (type != ZAP || ch != '\n')
		putc(getc(tmpf), f);
}

isfrom(lp)
register char *lp;
{
	register char *p;

	for (p = from; *p; )
		if (*lp++ != *p++)
			return(0);
	return(1);
}

bulkmail(argc, argv)
char **argv;
{
	char *truename;
	int first;
	register char *cp;
	char *newargv[1000];
	register char **ap;
	register char **vp;
	int dflag;

	dflag = 0;
	delflg = 0;
	if (argc < 1) {
		fprintf(stderr, "puke\n");
		return;
	}
	for (vp = argv, ap = newargv + 1; (*ap = *vp++) != 0; ap++)
		if (ap[0][0] == '-' && ap[0][1] == 'd')
			dflag++;
	if (!dflag) {
		/* give it to sendmail, rah rah! */
		unlink(lettmp);
		ap = newargv+1;
		if (rmail)
			*ap-- = "-s";
		*ap = "-sendmail";
		setuid(getuid());
		execv(SENDMAIL, ap);
		perror(SENDMAIL);
		exit(EX_UNAVAILABLE);
	}

	truename = 0;
	line[0] = '\0';

	/*
	 * When we fall out of this, argv[1] should be first name,
	 * argc should be number of names + 1.
	 */

	while (argc > 1 && *argv[1] == '-') {
		cp = *++argv;
		argc--;
		switch (cp[1]) {
		case 'r':
			if (argc <= 1)
				usage();
			if (strcmp(my_name, "root") == 0 ||
			    strcmp(my_name, "daemon") == 0 ||
			    strcmp(my_name, "network") == 0 ||
			    strcmp(my_name, "uucp")) {
				gaver++;
				strcpy(truename, argv[1]);
				fgets(line, LSIZE, stdin);
				if (strncmp("From", line, 4) == 0)
					line[0] = '\0';
			}
			argv++;
			argc--;
			break;

		case 'h':
			if (argc <= 1)
				usage();
			hseqno = atoi(argv[1]);
			argv++;
			argc--;
			break;

		case 'd':
			break;
		
		default:
			usage();
		}
	}
	if (argc <= 1)
		usage();
	if (truename == 0)
		truename = my_name;
	time(&iop);
	fprintf(tmpf, "%s%s %s", from, truename, ctime(&iop));
	iop = ftell(tmpf);
	flgf = first = 1;
	for (;;) {
		if (first) {
			first = 0;
			if (*line == '\0' && fgets(line, LSIZE, stdin) == NULL)
				break;
		} else {
			if (fgets(line, LSIZE, stdin) == NULL)
				break;
		}
		if (*line == '.' && line[1] == '\n' && isatty(fileno(stdin)))
			break;
		if (isfrom(line))
			putc('>', tmpf);
		fputs(line, tmpf);
		flgf = 0;
	}
	putc('\n', tmpf);
	nlet = 1;
	let[0].adr = 0;
	let[1].adr = ftell(tmpf);
	if (flgf)
		return;
	while (--argc > 0)
		if (!sendmail(0, *++argv, truename))
			error++;
	if (error && safefile(dead)) {
		setuid(getuid());
		malf = fopen(dead, "w");
		if (malf == NULL) {
			printf("mail: cannot open %s\n", dead);
			fclose(tmpf);
			return;
		}
		copylet(0, malf, ZAP);
		fclose(malf);
		printf("Mail saved in %s\n", dead);
	}
	fclose(tmpf);
}

sendrmt(n, name)
char *name;
{
	FILE *rmf, *popen();
	register char *p;
	char rsys[64], cmd[64];
	register pid;
	int sts;

#ifdef notdef
	if (any('^', name)) {
		while (p = index(name, '^'))
			*p = '!';
		if (strncmp(name, "researc", 7)) {
			strcpy(rsys, "research");
			if (*name != '!')
				--name;
			goto skip;
		}
	}
#endif
	for (p=rsys; *name!='!'; *p++ = *name++)
		if (*name=='\0')
			return(0);	/* local address, no '!' */
	*p = '\0';
	if (name[1]=='\0') {
		printf("null name\n");
		return(0);
	}
skip:
	if ((pid = fork()) == -1) {
		fprintf(stderr, "mail: can't create proc for remote\n");
		return(0);
	}
	if (pid) {
		while (wait(&sts) != pid) {
			if (wait(&sts)==-1)
				return(0);
		}
		return(!sts);
	}
	setuid(getuid());
	if (any('!', name+1))
		(void)sprintf(cmd, "uux - %s!rmail \\(%s\\)", rsys, name+1);
	else
		(void)sprintf(cmd, "uux - %s!rmail %s", rsys, name+1);
	if ((rmf=popen(cmd, "w")) == NULL)
		exit(1);
	copylet(n, rmf, REMOTE);
	exit(pclose(rmf) != 0);
}

usage()
{

	fprintf(stderr, "Usage: mail [ -f ] people . . .\n");
	error = EX_USAGE;
	done();
}

#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

notifybiff(msg)
	char *msg;
{
	static struct sockaddr_in addr;
	static int f = -1;

	if (addr.sin_family == 0) {
		struct hostent *hp = gethostbyname("localhost");
		struct servent *sp = getservbyname("biff", "udp");

		if (hp && sp) {
			addr.sin_family = hp->h_addrtype;
			bcopy(hp->h_addr, &addr.sin_addr, hp->h_length);
			addr.sin_port = sp->s_port;
		}
	}
	if (addr.sin_family) {
		if (f < 0)
			f = socket(AF_INET, SOCK_DGRAM, 0);
		if (f >= 0)
			sendto(f, msg, strlen(msg)+1, 0, &addr, sizeof (addr));
	}
}

sendmail(n, name, fromaddr)
	int n;
	char *name, *fromaddr;
{
	char file[256];
	int mask, fd;
	struct passwd *pw;
#ifdef notdef
	struct stat statb;
#endif
	char buf[128];

	if (*name=='!')
		name++;
	if (any('!', name))
		return (sendrmt(n, name));
	if ((pw = getpwnam(name)) == NULL) {
		printf("mail: can't send to %s\n", name);
		return(0);
	}
	cat(file, maildir, name);
#ifdef notdef
	if (stat(file, &statb) >= 0 && (statb.st_mode & S_IFMT) == S_IFDIR) {
		strcat(file, "/");
		strcat(file, name);
	}
#endif
	if (!safefile(file))
		return(0);
	fd = open(file, O_WRONLY | O_CREAT, MAILMODE);
	if (fd >= 0) {
		flock(fd, LOCK_EX);
		malf = fdopen(fd, "a");
	}
	if (fd < 0 || malf == NULL) {
		close(fd);
		printf("mail: %s: cannot append\n", file);
		return(0);
	}
	fchown(fd, pw->pw_uid, pw->pw_gid);
	(void)sprintf(buf, "%s@%d\n", name, ftell(malf));
	copylet(n, malf, ORDINARY);
	fclose(malf);
	notifybiff(buf);
	return(1);
}

delex(i)
{
	if (i != SIGINT) {
		setsig(i, SIG_DFL);
		sigsetmask(sigblock(0) &~ sigmask(i));
	}
	putc('\n', stderr);
	if (delflg)
		longjmp(sjbuf, 1);
	if (error == 0)
		error = i;
	done();
}

done()
{

	unlink(lettmp);
	exit(error);
}

cat(to, from1, from2)
	char *to, *from1, *from2;
{
	register char *cp, *dp;

	cp = to;
	for (dp = from1; *cp = *dp++; cp++)
		;
	for (dp = from2; *cp++ = *dp++; )
		;
}

/* copy p... into s, update p */
char *
getarg(s, p)
	register char *s, *p;
{
	while (*p == ' ' || *p == '\t')
		p++;
	if (*p == '\n' || *p == '\0')
		return(NULL);
	while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
		*s++ = *p++;
	*s = '\0';
	return(p);
}

safefile(f)
	char *f;
{
	struct stat statb;

	if (lstat(f, &statb) < 0)
		return (1);
	if (statb.st_nlink != 1 || (statb.st_mode & S_IFMT) == S_IFLNK) {
		fprintf(stderr,
		    "mail: %s has more than one link or is a symbolic link\n",
		    f);
		return (0);
	}
	return (1);
}

panic(msg, a1, a2, a3)
	char *msg;
{

	fprintf(stderr, "mail: ");
	fprintf(stderr, msg, a1, a2, a3);
	fprintf(stderr, "\n");
	done();
}

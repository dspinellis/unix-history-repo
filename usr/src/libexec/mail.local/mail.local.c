#include <ctype.h>
#include <stdio.h>
#include <pwd.h>
#include <utmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <whoami.h>
#include <sysexits.h>

static char SccsId[] = "@(#)mail.local.c	4.6	%G%";

#define DELIVERMAIL	"/etc/delivermail"


/*copylet flags */
	/*remote mail, add rmtmsg */
#define REMOTE	1
	/* zap header and trailing empty line */
#define ZAP	3
#define ORDINARY 2
#define	FORWARD	4
#define	LSIZE	256
#define	MAXLET	300	/* maximum number of letters */
#define	MAILMODE (~0644)		/* mode of created mail */
# ifndef DELIVERMAIL
#define	RMAIL	"/usr/net/bin/sendberkmail"
#define LOCNAM1	"csvax"
#define LOCNAM2	"ucbvax"
#define LOCNAM3	"vax"
#define LOCNAM4	"v"
# endif

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
char	*thissys = sysname;
char	*netname = "vax";
char	forwmsg[] = " forwarded\n";
FILE	*tmpf;
FILE	*malf;
char	*my_name;
char	*getlogin();
struct	passwd	*getpwuid();
int	error;
int	changed;
int	forward;
char	from[] = "From ";
long	ftell();
int	delete();
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
	register i;
	char sobuf[BUFSIZ];

	setbuf(stdout, sobuf);
	mktemp(lettmp);
	unlink(lettmp);
	my_name = getlogin();
	if (my_name == NULL || strlen(my_name) == 0) {
		struct passwd *pwent;
		pwent = getpwuid(getuid());
		if (pwent==NULL)
			my_name = "???";
		else
			my_name = pwent->pw_name;
	}
	if(setjmp(sjbuf)) done();
	for (i=0; i<20; i++)
		setsig(i, delete);
	tmpf = fopen(lettmp, "w");
	if (tmpf == NULL) {
		fprintf(stderr, "mail: cannot open %s for writing\n", lettmp);
		done();
	}
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
	if(signal(i, SIG_IGN)!=SIG_IGN)
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
	if (stat(mailfile, &statb) >= 0
	    && (statb.st_mode & S_IFMT) == S_IFDIR) {
		strcat(mailfile, "/");
		strcat(mailfile, my_name);
	}
	for (; argc>1; argv++, argc--) {
		if (argv[1][0]=='-') {
			if (argv[1][1]=='q')
				delflg = 0;
			else if (argv[1][1]=='p') {
				flgp++;
				delflg = 0;
			} else if (argv[1][1]=='f') {
				if (argc>=3) {
					strcpy(mailfile, argv[2]);
					argv++;
					argc--;
				}
			} else if (argv[1][1]=='r') {
				forward = 1;
			} else if (argv[1][1]=='h') {
				forward = 1;
			} else {
				fprintf(stderr, "mail: unknown option %c\n", argv[1][1]);
				done();
			}
		} else
			break;
	}
	malf = fopen(mailfile, "r");
	if (malf == NULL) {
		fprintf(stdout, "No mail.\n");
		return;
	}
	lock(mailfile);
	copymt(malf, tmpf);
	fclose(malf);
	fclose(tmpf);
	unlock();
	tmpf = fopen(lettmp, "r");

	changed = 0;
	print = 1;
	for (i = 0; i < nlet; ) {
		j = forward ? i : nlet - i - 1;
		if(setjmp(sjbuf)) {
			print=0;
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
		fprintf(stdout, "? ");
		fflush(stdout);
		if (fgets(resp, LSIZE, stdin) == NULL)
			break;
		switch (resp[0]) {

		default:
			fprintf(stderr, "usage\n");
		case '?':
			print = 0;
			fprintf(stderr, "q\tquit\n");
			fprintf(stderr, "x\texit without changing mail\n");
			fprintf(stderr, "p\tprint\n");
			fprintf(stderr, "s[file]\tsave (default mbox)\n");
			fprintf(stderr, "w[file]\tsame without header\n");
			fprintf(stderr, "-\tprint previous\n");
			fprintf(stderr, "d\tdelete\n");
			fprintf(stderr, "+\tnext (no delete)\n");
			fprintf(stderr, "m user\tmail to user\n");
			fprintf(stderr, "! cmd\texecute cmd\n");
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
				if(p != 0)
					cat(resp+1, p, "/mbox");
				else
					cat(resp+1, "", "mbox");
			}
			for (p = resp+1; (p = getarg(lfil, p)) != NULL; ) {
				malf = fopen(lfil, "a");
				if (malf == NULL) {
					fprintf(stdout, "mail: cannot append to %s\n", lfil);
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
				if (!sendrmt(j, lfil, "/bin/mail"))	/* couldn't send it */
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

copyback()	/* copy temp or whatever back to /usr/spool/mail */
{
	register i, n, c;
	int new = 0;
	struct stat stbuf;

	signal(SIGINT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	lock(mailfile);
	stat(mailfile, &stbuf);
	if (stbuf.st_size != let[nlet].adr) {	/* new mail has arrived */
		malf = fopen(mailfile, "r");
		if (malf == NULL) {
			fprintf(stdout, "mail: can't re-read %s\n", mailfile);
			done();
		}
		fseek(malf, let[nlet].adr, 0);
		fclose(tmpf);
		tmpf = fopen(lettmp, "a");
		fseek(tmpf, let[nlet].adr, 0);
		while ((c = fgetc(malf)) != EOF)
			fputc(c, tmpf);
		fclose(malf);
		fclose(tmpf);
		tmpf = fopen(lettmp, "r");
		let[++nlet].adr = stbuf.st_size;
		new = 1;
	}
	malf = fopen(mailfile, "w");
	if (malf == NULL) {
		fprintf(stderr, "mail: can't rewrite %s\n", lfil);
		done();
	}
	n = 0;
	for (i = 0; i < nlet; i++)
		if (let[i].change != 'd') {
			copylet(i, malf, ORDINARY);
			n++;
		}
	fclose(malf);
	if (new)
		fprintf(stdout, "new mail arrived\n");
	unlock();
}

copymt(f1, f2)	/* copy mail (f1) to temp (f2) */
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

copylet(n, f, type) FILE *f;
{	int ch, k;
	fseek(tmpf, let[n].adr, 0);
	k = let[n+1].adr - let[n].adr;
	while(k-- > 1 && (ch=fgetc(tmpf))!='\n')
		if(type!=ZAP) fputc(ch,f);
	if(type==REMOTE)
		fprintf(f, " remote from %s\n", thissys);
	else if (type==FORWARD)
		fprintf(f, forwmsg);
	else if(type==ORDINARY)
		fputc(ch,f);
	while(k-->1)
		fputc(ch=fgetc(tmpf), f);
	if(type!=ZAP || ch!= '\n')
		fputc(fgetc(tmpf), f);
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
	char truename[100];
	int first;
	register char *cp;
	int gaver = 0;
# ifdef DELIVERMAIL
	char *newargv[1000];
	register char **ap;
	register char **vp;
	int dflag;

	dflag = 0;
	if (argc < 1)
		fprintf(stderr, "puke\n");
	for (vp = argv, ap = newargv + 1; (*ap = *vp++) != 0; ap++)
	{
		if (ap[0][0] == '-' && ap[0][1] == 'd')
			dflag++;
	}
	if (!dflag)
	{
		/* give it to delivermail, rah rah! */
		unlink(lettmp);
		ap = newargv+1;
		if (rmail)
			*ap-- = "-s";
		*ap = "-delivermail";
		setuid(getuid());
		execv(DELIVERMAIL, ap);
		perror(DELIVERMAIL);
		exit(EX_UNAVAILABLE);
	}
# endif DELIVERMAIL

	truename[0] = 0;
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
			if (argc <= 0) {
				usage();
				done();
			}
			gaver++;
			strcpy(truename, argv[1]);
			fgets(line, LSIZE, stdin);
			if (strcmpn("From", line, 4) == 0)
				line[0] = '\0';
			argv++;
			argc--;
			break;

		case 'h':
			if (argc <= 0) {
				usage();
				done();
			}
			hseqno = atoi(argv[1]);
			argv++;
			argc--;
			break;

# ifdef DELIVERMAIL
		case 'd':
			break;
# endif DELIVERMAIL
		
		default:
			usage();
			done();
		}
	}
	if (argc <= 1) {
		usage();
		done();
	}
	if (gaver == 0)
		strcpy(truename, my_name);
	/*
	if (argc > 4 && strcmp(argv[1], "-r") == 0) {
		strcpy(truename, argv[2]);
		argc -= 2;
		argv += 2;
		fgets(line, LSIZE, stdin);
		if (strcmpn("From", line, 4) == 0)
			line[0] = '\0';
	} else
		strcpy(truename, my_name);
	*/
	time(&iop);
	fprintf(tmpf, "%s%s %s", from, truename, ctime(&iop));
	iop = ftell(tmpf);
	flgf = 1;
	for (first = 1;; first = 0) {
		if (first && line[0] == '\0' && fgets(line, LSIZE, stdin) == NULL)
			break;
		if (!first && fgets(line, LSIZE, stdin) == NULL)
			break;
		if (line[0] == '.' && line[1] == '\n' && isatty(fileno(stdin)))
			break;
		if (isfrom(line))
			fputs(">", tmpf);
		fputs(line, tmpf);
		flgf = 0;
	}
	fputs("\n", tmpf);
	nlet = 1;
	let[0].adr = 0;
	let[1].adr = ftell(tmpf);
	fclose(tmpf);
	if (flgf)
		return;
	tmpf = fopen(lettmp, "r");
	if (tmpf == NULL) {
		fprintf(stderr, "mail: cannot reopen %s for reading\n", lettmp);
		return;
	}
	while (--argc > 0) {
		if (!sendmail(0, *++argv, truename))
			error++;
	}
	if (error) {
		setuid(getuid());
		malf = fopen(dead, "w");
		if (malf == NULL) {
			fprintf(stdout, "mail: cannot open %s\n", dead);
			fclose(tmpf);
			return;
		}
		copylet(0, malf, ZAP);
		fclose(malf);
		fprintf(stdout, "Mail saved in %s\n", dead);
	}
	fclose(tmpf);
}

sendrmt(n, name, rcmd)
char *name;
char *rcmd;
{
	FILE *rmf, *popen();
	register char *p;
	char rsys[64], cmd[64];
	register local, pid;
	int sts;

	local = 0;
	if (index(name, '^')) {
		while (p = index(name, '^'))
			*p = '!';
		if (strncmp(name, "researc", 7)) {
			strcpy(rsys, "research");
			if (*name != '!')
				--name;
			goto skip;
		}
	}
	if (*name=='!')
		name++;
	for(p=rsys; *name!='!'; *p++ = *name++)
		if (*name=='\0') {
			local++;
			break;
		}
	*p = '\0';
	if ((!local && *name=='\0') || (local && *rsys=='\0')) {
		fprintf(stdout, "null name\n");
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
	if (local)
		sprintf(cmd, "%s %s", rcmd, rsys);
	else {
		if (index(name+1, '!'))
			sprintf(cmd, "uux - %s!rmail \\(%s\\)", rsys, name+1);
		else
			sprintf(cmd, "uux - %s!rmail %s", rsys, name+1);
	}
	if ((rmf=popen(cmd, "w")) == NULL)
		exit(1);
	copylet(n, rmf, local ? !strcmp(rcmd, "/bin/mail") ? FORWARD : ORDINARY : REMOTE);
	pclose(rmf);
	exit(0);
}

# ifndef DELIVERMAIL
/*
 * Send mail on the Berkeley network.
 * Sorry Bill, sendrmt() is so awful we just gave up.
 */

sendberkmail(n, name, fromaddr)
	char name[];
	char fromaddr[];
{
	char cmd[200];
	register FILE *cmdf;

	sprintf(cmd, "%s -h %d -f %s -t %s", RMAIL, hseqno, fromaddr, name);
	if ((cmdf = popen(cmd, "w")) == NULL) {
		perror(RMAIL);
		return(0);
	}
	copylet(n, cmdf, ORDINARY);
	pclose(cmdf);
	return(9);
}
# endif

usage()
{

	fprintf(stderr, "Usage: mail [ -f ] people . . .\n");
}

#include <sys/socket.h>
#include <net/in.h>
#include <wellknown.h>
struct sockaddr_in biffaddr = { AF_INET, IPPORT_BIFFUDP };
char *localhost = "localhost";

sendmail(n, name, fromaddr)
int n;
char *name;
char *fromaddr;
{
	char file[100];
	register char *p;
	register mask;
	struct passwd *pw, *getpwnam();
	struct stat statb;
	char buf[128];
	int f;

# ifndef DELIVERMAIL
	stripfx(LOCNAM1, &name);
	stripfx(LOCNAM2, &name);
	stripfx(LOCNAM3, &name);
	stripfx(LOCNAM4, &name);
	if(*name == ':')name++;		/* skip colon in to-name */
	for(p=name; *p!=':' && *p!='!' && *p!='^' &&*p!='\0'; p++);
	/* if(*p == ':') return(sendrmt(n, name, RMAIL)); */
	if (*p == ':')
		return(sendberkmail(n, name, fromaddr));
	else if (*p=='\0' && strcmp(name, "msgs") == 0)
		return(sendrmt(n, "-s", "/usr/ucb/msgs"));
# endif
	for(p=name; *p!='!'&&*p!='^' &&*p!='\0'; p++)
		;
	if (*p == '!'|| *p=='^')
		return(sendrmt(n, name, 0));
	if ((pw = getpwnam(name)) == NULL) {
		fprintf(stdout, "mail: can't send to %s\n", name);
		return(0);
	}
	cat(file, maildir, name);
	if (stat(file, &statb) >= 0 && (statb.st_mode & S_IFMT) == S_IFDIR) {
		strcat(file, "/");
		strcat(file, name);
	}
	mask = umask(MAILMODE);
	if (stat(file, &statb) >= 0 && statb.st_nlink != 1) {
		fprintf(stdout, "mail: %s's mail file has more than one link\n", name);
		return(0);
	}
	malf = fopen(file, "a");
	umask(mask);
	if (malf == NULL) {
		fprintf(stdout, "mail: cannot append to %s\n", file);
		return(0);
	}
	lock(file);
	chown(file, pw->pw_uid, pw->pw_gid);
	{
		f = socket(SOCK_DGRAM, 0, 0, 0);
		sprintf(buf, "%s@%d\n", name, ftell(malf)); 
	}
	copylet(n, malf, ORDINARY);
	fclose(malf);
	if (f >= 0) {
		biffaddr.sin_addr.s_addr = rhost(&localhost);
		send(f, &biffaddr, buf, strlen(buf)+1);
		close(f);
	}
	unlock();
	return(1);
}

delete(i)
{
	setsig(i, delete);
	fprintf(stderr, "\n");
	if(delflg)
		longjmp(sjbuf, 1);
	done();
}

/*
 * Lock the specified mail file by setting the file mailfile.lock.
 * We must, of course, be careful to unlink the lock file by a call
 * to unlock before we stop.  The algorithm used here is to see if
 * the lock exists, and if it does, to check its modify time.  If it
 * is older than 30 seconds, we assume error and set our own file.
 * Otherwise, we wait for 5 seconds and try again.
 */

char	*maillock	= ".lock";		/* Lock suffix for mailname */
char	*lockname	= "/usr/spool/mail/tmXXXXXX";
char	locktmp[30];				/* Usable lock temporary */
char	curlock[50];				/* Last used name of lock */
int	locked;					/* To note that we locked it */

lock(file)
char *file;
{
	register int f;
	struct stat sbuf;
	long curtime;
	int statfailed;

	if (locked || flgf)
		return(0);
	strcpy(curlock, file);
	strcat(curlock, maillock);
	strcpy(locktmp, lockname);
	mktemp(locktmp);
	unlink(locktmp);
	statfailed = 0;
	for (;;) {
		f = lock1(locktmp, curlock);
		if (f == 0) {
			locked = 1;
			return(0);
		}
		if (stat(curlock, &sbuf) < 0) {
			if (statfailed++ > 5)
				return(-1);
			sleep(5);
			continue;
		}
		statfailed = 0;
		time(&curtime);
		if (curtime < sbuf.st_ctime + 30) {
			sleep(5);
			continue;
		}
		unlink(curlock);
	}
}

/*
 * Remove the mail lock, and note that we no longer
 * have it locked.
 */

unlock()
{

	unlink(curlock);
	locked = 0;
}

/*
 * Attempt to set the lock by creating the temporary file,
 * then doing a link/unlink.  If it fails, return -1 else 0
 */

lock1(tempfile, name)
	char tempfile[], name[];
{
	register int fd;

	fd = creat(tempfile, 0);
	if (fd < 0)
		return(-1);
	close(fd);
	if (link(tempfile, name) < 0) {
		unlink(tempfile);
		return(-1);
	}
	unlink(tempfile);
	return(0);
}

done()
{
	if(locked)
		unlock();
	unlink(lettmp);
	unlink(locktmp);
	exit(error);
}

cat(to, from1, from2)
char *to, *from1, *from2;
{
	int i, j;

	j = 0;
	for (i=0; from1[i]; i++)
		to[j++] = from1[i];
	for (i=0; from2[i]; i++)
		to[j++] = from2[i];
	to[j] = 0;
}

char *getarg(s, p)	/* copy p... into s, update p */
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
# ifndef DELIVERMAIL
/*
	stripfx(prefix string, pointer to string)

	takes a ptr to string and compares it to prefix string.
	may be called multiple times
*/
stripfx(pfx, name)
	char *pfx;
	char **name;
{
	register char *cp = *name;

	while (*pfx && (*cp == *pfx || *cp == toupper(*pfx)))
		cp++, pfx++;
	if (*cp != ':' || *pfx != 0)
		return;
	*name = cp;
}
# endif

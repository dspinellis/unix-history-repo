#include <ctype.h>
#include <stdio.h>
#include <pwd.h>
#include <utmp.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <whoami.h>

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
#define	RMAIL	"/usr/net/bin/sendmail"

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
char	mailfile[] = "/usr/spool/mail/xxxxxxxxxxxxxxxxxxxxxxx";
char	dead[] = "dead.letter";
char	*thissys = sysname;
char	*netname = "vax";
char	forwmsg[] = " forwarded\n";
char	*curlock;
int	lockerror;
FILE	*tmpf;
FILE	*malf;
char	*my_name;
char	*getlogin();
struct	passwd	*getpwuid();
int	error;
int	locked;
int	changed;
int	forward;
char	from[] = "From ";
long	ftell();
int	delete();
char	*ctime();
int	flgf;
int	flgp;
int	delflg = 1;
jmp_buf	sjbuf;

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
	if (argv[0][0] != 'r' &&	/* no favors for rmail*/
	   (argc == 1 || argv[1][0] == '-' && argv[1][1] != 'r'))
		printmail(argc, argv);
	else
		sendmail(argc, argv);
	done();
}

setsig(i, f)
int i;
int (*f)();
{
	if(signal(i, SIG_IGN)!=SIG_IGN)
		signal(i, f);
}

printmail(argc, argv)
char **argv;
{
	int flg, i, j, print;
	char *p, *getarg();

	setuid(getuid());
	cat(mailfile, maildir, my_name);
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

sendmail(argc, argv)
char **argv;
{
	char truename[100];
	int first;

	truename[0] = 0;
	line[0] = '\0';
	if (argc > 4 && strcmp(argv[1], "-r") == 0) {
		strcpy(truename, argv[2]);
		strcat(truename, ":");
		strcat(truename, argv[3]);
		argc -= 3;
		argv += 3;
		fgets(line, LSIZE, stdin);
		if (strcmpn("From", line, 4) == 0)
			line[0] = '\0';
	} else
		strcpy(truename, my_name);
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
	while (--argc > 0)
		if (!send(0, *++argv))	/* couldn't send to him */
			error++;
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

send(n, name)	/* send letter n to name */
int n;
char *name;
{
	char file[50];
	register char *p;
	register mask;
	struct passwd *pw, *getpwnam();

	stripfx(netname, &name);
	for(p=name; *p!=':' &&*p!='\0'; p++);
	if(*p == ':') return(sendrmt(n, name, RMAIL));
	else if (strcmp(name, "msgs") == 0) return(sendrmt(n, "-s", "/usr/ucb/msgs"));
	for(p=name; *p!='!'&&*p!='^' &&*p!='\0'; p++)
		;
	if (*p == '!'|| *p=='^')
		return(sendrmt(n, name, 0));
	if ((pw = getpwnam(name)) == NULL) {
		fprintf(stdout, "mail: can't send to %s\n", name);
		return(0);
	}
	cat(file, maildir, name);
	mask = umask(MAILMODE);
	malf = fopen(file, "a");
	umask(mask);
	if (malf == NULL) {
		fprintf(stdout, "mail: cannot append to %s\n", file);
		return(0);
	}
	lock(file);
	chown(file, pw->pw_uid, pw->pw_gid);
	copylet(n, malf, ORDINARY);
	fclose(malf);
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

done()
{
	if(!lockerror)
		unlock();
	unlink(lettmp);
	exit(error+lockerror);
}

lock(file)
char *file;
{
	struct stat stbuf;

	if (locked || flgf)
		return;
	if (stat(file, &stbuf)<0)
		return;
	if (stbuf.st_mode&01) { 	/* user x bit is the lock */
		if (stbuf.st_ctime+60 >= time((long *)0)) {
			fprintf(stderr, "%s busy; try again in a minute\n", file);
			lockerror++;
			done();
		}
	}
	locked = stbuf.st_mode & ~01;
	curlock = file;
	chmod(file, stbuf.st_mode|01);
}

unlock()
{
	if (locked)
		chmod(curlock, locked);
	locked = 0;
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
stripfx(pfx, name)
	char *pfx;
	char **name;
{
	register char *cp = *name;

	while (*pfx && (*cp == *pfx || *cp == toupper(*pfx)))
		cp++, pfx++;
	if (*cp++ != ':')
		return;
	*name = cp;
}

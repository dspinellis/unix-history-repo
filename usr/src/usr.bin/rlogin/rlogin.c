#ifndef lint
static char sccsid[] = "@(#)rlogin.c	4.2 82/04/06";
#endif

#include <stdio.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <errno.h>
#include <pwd.h>

/*
 * rlogin - remote login; this is a hacked version of cu
 */
char	*index(), *rindex(), *malloc(), *getenv();
struct	passwd *getpwuid();
struct	passwd *pwd;
char	*name, *pass;
int	rem;
char	cmdchar = '~';
int	rcmdoptions = 0;
int	eight;
char	*speeds[] =
    { "0", "50", "75", "110", "134", "150", "200", "300",
      "600", "1200", "1800", "2400", "4800", "9600", "19200", "38400" };
char	term[64];

main(argc, argv)
	int argc;
	char **argv;
{
	int pid;
	char *host, *cp, **ap, buf[BUFSIZ];
	register int cc;
	struct sgttyb ttyb;
	struct passwd *pwd;

	host = rindex(argv[0], '/');
	if (host)
		host++;
	else
		host = argv[0];
	argv++, --argc;
	if (!strcmp(host, "rlogin"))
		host = *argv++, --argc;
another:
	if (!strcmp(*argv, "-d")) {
		argv++, argc--;
		rcmdoptions |= SO_DEBUG;
		goto another;
	}
	if (!strcmp(*argv, "-l")) {
		argv++, argc--;
		if (argc == 0)
			goto usage;
		name = *argv++; argc--;
		goto another;
	}
	if (!strncmp(*argv, "-e", 2)) {
		cmdchar = argv[0][2];
		argv++, argc--;
		goto another;
	}
	if (!strcmp(*argv, "-8")) {
		eight = 1;
		argv++, argc--;
		goto another;
	}
	if (host == 0)
		goto usage;
	if (argc > 0)
		goto usage;
	pwd = getpwuid(getuid());
	if (pwd == 0) {
		fprintf(stderr, "Who are you?\n");
		exit(1);
	}
	cc = 0;
	strcpy(term, getenv("TERM"));
	if (gtty(0, &ttyb)==0) {
		strcat(term, "/");
		strcat(term, speeds[ttyb.sg_ospeed]);
	}
        rem = rcmd(&host, IPPORT_LOGINSERVER, pwd->pw_name,
	    name ? name : pwd->pw_name, term, 0);
        if (rem < 0)
                exit(1);
	setuid(getuid());
	cumain();
	exit(0);
usage:
	fprintf(stderr,
	    "usage: rlogin host [ -ex ] [ -l username ]\n");
	exit(1);
}

#include <ctype.h>
#include <signal.h>

#define CRLF "\r\n"
#define wrc(ds) write(ds,&c,1)

char	tkill, terase;	/* current input kill & erase */
int	efk;		/* process of id of listener  */
char	c, oc;
int	pipes[] = {-1,-1};
int	intr, sig2();
int	parent;

int	nhup;
int	done();

struct tchars deftchars;
struct tchars notchars = { 0377, 0377, 'q'&037, 's'&037, 0377, 0377 };
struct ltchars defltchars;
struct ltchars noltchars = { 0377, 0377, 0377, 0377, 0377, 0377 };
char defkill, deferase, defflags;

cumain()
{
	int fk;
	int speed;
	char *telno;
	struct sgttyb stbuf;
	int exit();

	gtty(0, &stbuf);
	defkill = stbuf.sg_kill;
	deferase = stbuf.sg_erase;
	defflags = stbuf.sg_flags & (ECHO | CRMOD);
	ioctl(0, TIOCGETC, &deftchars);
	ioctl(0, TIOCGLTC, &defltchars);
	signal(SIGINT, exit);
	signal(SIGHUP, exit);
	signal(SIGQUIT, exit);
	pipe(pipes);
	parent = getpid();
	fk = fork();
	nhup = (int)signal(SIGINT, SIG_IGN);
	if (fk == 0) {
		rd();
		sleep(1);
		prf("\007Lost connection.");
		exit(3);
	}
	signal(SIGCHLD, done);
	mode(1);
	efk = fk;
	wr();
	if (fk != -1) kill(fk, SIGKILL);
	prf("Disconnected.");
	done();
}

done()
{

	mode(0);
	wait((int *)NULL);
	exit(0);
}

/*
 *	wr: write to remote: 0 -> line.
 *	~.	terminate
 *	~<file	send file
 *	~!	local login-style shell
 *	~!cmd	execute cmd locally
 *	~$proc	execute proc locally, send output to line
 *	~%cmd	execute builtin cmd (put and take)
 *	~^Z	suspend cu process.
 */

wr()
{
	int ds,fk,lcl,x;
	char *p,b[600];
	for (;;) {
		p=b;
		while (rdc(0) == 1) {
			if (p == b) lcl=(c == cmdchar);
			if (p == b+1 && b[0] == cmdchar) lcl=(c!=cmdchar);
			if (!lcl) {
				c = oc;
				if (wrc(rem) == 0) {
					prf("line gone"); return;
				}
				if (eight == 0)
					c &= 0177;
			}
			if (lcl) {
				if (c == 0177) c=tkill;
				if (c == '\r' || c == '\n') goto A;
				wrc(0);
			}
			*p++=c;
			if (c == terase) {
				p=p-2; 
				if (p<b) p=b;
			}
			if (c == tkill || c == 0177 || c == '\4' || c == '\r' || c == '\n') p=b;
		}
		return;
A: 
		echo("");
		*p=0;
		switch (b[1]) {
		case '.':
		case '\004':
			return;
		case '!':
		case '$':
			fk = fork();
			signal(SIGCHLD, SIG_DFL);
			if (fk == 0) {
				char *shell = getenv("SHELL");
				if (shell == 0) shell = "/bin/sh";
				close(1);
				dup(b[1] == '$'? rem:2);
				close(rem);
				mode(0);
				if (!nhup) signal(SIGINT, SIG_DFL);
				if (b[2] == 0) execl(shell,shell,0);
				/* if (b[2] == 0) execl(shell,"-",0); */
				else execl(shell,"sh","-c",b+2,0);
				prf("Can't execute shell");
				exit(~0);
			}
			if (fk!=(-1)) {
				while (wait(&x)!=fk);
			}
			signal(SIGCHLD, done);
			mode(1);
			if (b[1] == '!') echo("!");
			break;
		case '<':
			if (b[2] == 0) break;
			if ((ds=open(b+2,0))<0) {
				prf("Can't divert %s",b+1); 
				break;
			}
			intr=x=0;
			mode(2);
			if (!nhup) signal(SIGINT, sig2);
			while (!intr && rdc(ds) == 1) {
				if (wrc(rem) == 0) {
					x=1; 
					break;
				}
			}
			signal(SIGINT, SIG_IGN);
			close(ds);
			mode(1);
			if (x) return;
			break;
		case '>':
		case ':':
			{
			register char *q;

			q = b+1;
			if(*q=='>') q++;
			write(pipes[1],q,strlen(q)+1);
			if (efk != -1) kill(efk,SIGEMT);
			}
			break;
#ifdef SIGTSTP
#define CTRLZ	26
		case CTRLZ:
			mode(0);
			signal(SIGCHLD, SIG_IGN);
			kill(0, SIGTSTP);
			signal(SIGCHLD, done);
			mode(1);
			break;
#endif
		case '%':
			dopercen(&b[2]);
			break;
		default:
			prf("Use `%c%c' to start line with `%c'", cmdchar, cmdchar, cmdchar);
		}
		continue;
	}
}

dopercen(line)
register char *line;
{
	char *args[10];
	register narg, f;
	int rcount;
	for (narg = 0; narg < 10;) {
		while(*line == ' ' || *line == '\t')
			line++;
		if (*line == '\0')
			break;
		args[narg++] = line;
		while(*line != '\0' && *line != ' ' && *line != '\t')
			line++;
		if (*line == '\0')
			break;
		*line++ = '\0';
	}
	if (equal(args[0], "take")) {
		if (narg < 2) {
			prf("usage: %c%%take from [to]", cmdchar);
			return;
		}
		if (narg < 3)
			args[2] = args[1];
		write(pipes[1], ">/dev/null",sizeof(">/dev/null"));
		if (efk != -1) kill(efk,SIGEMT);
		sleep(5);
		wrln("echo '%c>:", cmdchar);
		wrln(args[2]);
		wrln("'; tee /dev/null <");
		wrln(args[1]);
		wrln(";echo '%c>'\n", cmdchar);
		return;
	} else if (equal(args[0], "put")) {
		prf("%c%%put doesn't work yet (use rsh)", cmdchar);
		return;
/*
		if (narg < 2) {
			prf("usage: %c%%put from [to]", cmdchar);
			return;
		}
		if (narg < 3)
			args[2] = args[1];
		if ((f = open(args[1], 0)) < 0) {
			prf("cannot open: %s", args[1]);
			return;
		}
		wrln("stty -echo;cat >");
		wrln(args[2]);
		wrln(";stty echo\n");
		sleep(5);
		intr = 0;
		if (!nhup)
			signal(SIGINT, sig2);
		mode(2);
		rcount = 0;
		while(!intr && rdc(f) == 1) {
			rcount++;
			if (c == tkill || c == terase)
				wrln("\\");
			if (wrc(rem) != 1) {
				sleep(2);
				if (wrc(rem) != 1) {
					prf("character missed");
					intr = 1;
					break;
				}
			}
		}
		signal(SIGINT, SIG_IGN);
		close(f);
		if (intr) {
			wrln("\n");
			prf("stopped after %d bytes", rcount);
		}
		wrln("\004");
		sleep(5);
		mode(1);
		return;
*/
	}
	prf("%c%%%s unknown\n", cmdchar, args[0]);
}

equal(s1, s2)
register char *s1, *s2;
{
	while (*s1++ == *s2)
		if (*s2++ == '\0')
			return(1);
	return(0);
}

wrln(s, p1, p2, p3)
register char *s;
int p1, p2, p3;
{
	char wbuf[256];

	sprintf(wbuf, s, p1, p2, p3);
	s = wbuf;
	while (*s)
		write(rem, s++, 1);
}
int ds,slnt;
int justrung;

/*
 *	rd: read from remote: line -> 1
 *	catch:
 *	~>[>][:][file]
 *	stuff from file...
 *	~>	(ends diversion)
 */

int ds,slnt,taking;
int justrung;
readmsg(){
	static char dobuff[128], morejunk[256];
	int n;
	justrung = 1;
	signal(SIGEMT,readmsg);
	n = read(pipes[0],morejunk,256);
	dodiver(morejunk);
}

dodiver(msg)
char *msg;
{
	register char *cp = msg; 

	if (*cp=='>') cp++;
	if (*cp==':') {
		cp++;
		if(*cp==0) {
			slnt ^= 1;
			return;
		} else  {
			slnt = 1;
		}
	}
	if (ds >= 0) close(ds);
	if (*cp==0) {
		slnt = 0;
		ds = -1;
		return;
	}
	if (*msg!='>' || (ds=open(cp,1))<0) ds=creat(cp,0644);
	lseek(ds, (long)0, 2);
	if(ds < 0) prf("Creat failed:"), prf(cp);
	if (ds<0) prf("Can't divert %s",cp+1);
}


/*
 *	rd: read from remote: line -> 1
 *	catch: diversion caught by interrupt routine
 */

#define ORDIN 0
#define SAWCR 1
#define EOL   2
#define SAWTL 3
#define DIVER 4

oob()
{
	int mark, cc, out = 1+1;
	char waste[512];

	signal(SIGURG, oob);
	ioctl(1, TIOCFLUSH, &out);
	for (;;) {
		if (ioctl(rem, SIOCATMARK, &mark) < 0) {
			perror("ioctl");
			break;
		}
		if (mark)
			break;
		cc = read(rem, waste, 512);
	}
	ioctl(rem, SIOCRCVOOB, &mark);
	if (mark & TIOCPKT_NOSTOP) {
		notchars.t_stopc = 0377;
		notchars.t_startc = 0377;
		ioctl(0, TIOCSETC, &notchars);
	}
	if (mark & TIOCPKT_DOSTOP) {
		notchars.t_stopc = 's'&037;
		notchars.t_startc = 'q'&037;
		ioctl(0, TIOCSETC, &notchars);
	}
}

rd()
{
	extern int ds,slnt;
	char rb[600], lb[600], *rlim, *llim, c;
	register char *p,*q;
	int cnt, state = 0, mustecho, oldslnt, readmsg();

	signal(SIGEMT,readmsg);  /* set up child for catching diversion msgs
				    from parent */
	signal(SIGURG,oob);
	{ int pid = -getpid();
	  ioctl(rem, SIOCSPGRP, &pid); }
	ds=(-1);
	p = lb; llim = lb+600;
agin:
	for (;;) {
		extern errno;
		errno = 0;
		cnt = read(rem,rb,600);
		if (cnt <= 0) {
			if (errno == EINTR) {
				errno = 0;
				continue;
			}
			break;
		}
		if(!slnt) write(1,rb,cnt);
		if(ds < 0) continue;
		oldslnt = slnt;
		for( q=rb, rlim = rb + cnt - 1; q <= rlim; ) {
			if (eight == 0)
			c &= 0177;
			if(p < llim) *p++ = c;
			switch(state) {
			case ORDIN:
				if(c=='\r') state = SAWCR;
				break;
			case SAWCR:
				if(c=='\n') {
					state = EOL;
					p--;
					p[-1] = '\n';
				} else state = ORDIN;
				break;
			case EOL:
				state = (c==cmdchar ? SAWTL : 
					 (c=='\r' ? SAWCR : ORDIN));
				break;
			case SAWTL:
				state = (c=='>' ? DIVER : 
					 (c=='\r' ? SAWCR : ORDIN));
				break;
			case DIVER:
				if(c=='\r') {
					p--;
				} else if (c=='\n') {
					state = ORDIN;
					p[-1] = 0;
					dodiver(lb+2);
					c = 0; p = lb;
				}
			}
			if(slnt==0 && oldslnt) {
				if(c=='\n') {
					write(rem,lb,p-lb-1);
					write(rem,CRLF,sizeof(CRLF));
				} else if(q==rlim) {
					write(rem,lb,p-lb);
					c = '\n';  /*force flush to file*/
				}
			}
			if(c=='\n') {
				if(ds >= 0)
					write(ds,lb,p-lb);
				p = lb;
			}
		}
	}
	if(justrung) {
		justrung = 0;
		goto agin;
	}
}

struct {char lobyte; char hibyte;};
mode(f)
{
	struct sgttyb stbuf;
	ioctl(0, TIOCGETP, &stbuf);
	if (f == 0) {
		stbuf.sg_flags &= ~CBREAK;
		stbuf.sg_flags |= defflags;
		ioctl(0, TIOCSETC, &deftchars);
		ioctl(0, TIOCSLTC, &defltchars);
		stbuf.sg_kill = defkill;
		stbuf.sg_erase = deferase;
	}
	if (f == 1) {
		stbuf.sg_flags |= CBREAK;
		stbuf.sg_flags &= ~(ECHO|CRMOD);
		ioctl(0, TIOCSETC, &notchars);
		ioctl(0, TIOCSLTC, &noltchars);
		stbuf.sg_kill = 0377;
		stbuf.sg_erase = 0377;
	}
	if (f == 2) {
		stbuf.sg_flags &= ~CBREAK;
		stbuf.sg_flags &= ~(ECHO|CRMOD);
		ioctl(0, TIOCSETC, &deftchars);
		ioctl(0, TIOCSLTC, &defltchars);
		stbuf.sg_kill = 0377;
		stbuf.sg_erase = 0377;
	}
	ioctl(0, TIOCSETN, &stbuf);
}

echo(s)
char *s;
{
	char *p;
	for (p=s;*p;p++);
	if (p>s) write(0,s,p-s);
	write(0,CRLF, sizeof(CRLF));
}

prf(f, a1, a2, a3)
char *f;
{
	fprintf(stderr, f, a1, a2, a3);
	fprintf(stderr, CRLF);
}

exists(devname)
char *devname;
{
	if (access(devname, 0)==0)
		return(1);
	prf("%s does not exist", devname);
	return(0);
}

rdc(ds)
{

	ds=read(ds,&c,1); 
	oc = c;
	if (eight == 0)
		c &= 0177;
	return (ds);
}

sig2()
{
	signal(SIGINT, SIG_IGN); 
	intr = 1;
}

#include <stdio.h>
#include <signal.h>
#include <sgtty.h>
/*
 *	cu telno [-t] [-s speed] [-l line] [-a acu]
 *
 *	-t is for dial-out to terminal.
 *	speeds are: 110, 134, 150, 300, 1200. 300 is default.
 *
 *	Escape with `~' at beginning of line.
 *	Ordinary diversions are ~<, ~> and ~>>.
 *	Silent output diversions are ~>: and ~>>:.
 *	Terminate output diversion with ~> alone.
 *	Quit is ~. and ~! gives local command or shell.
 *	Also ~$ for canned procedure pumping remote.
 *	~%put from [to]  and  ~%take from [to] invoke builtins
 */

#define CRLF "\r\n"
#define wrc(ds) write(ds,&c,1)


char	*devcul	= "/dev/cul0";
char	*devcua	= "/dev/cua0";
char	*lspeed	= "300";

int	ln;	/* fd for comm line */
char	tkill, terase;	/* current input kill & erase */
int	efk;		/* process of id of listener  */
char	c;

char	*connmsg[] = {
	"",
	"line busy",
	"call dropped",
	"no carrier",
	"can't fork",
	"acu access",
	"tty access",
	"tty hung",
	"usage: cu telno [-t] [-s speed] [-l line] [-a acu]"
};

rdc(ds) {

	ds=read(ds,&c,1); 
	c&= 0177; 
	return (ds);
}

int intr;

sig2()
{
	signal(SIGINT, SIG_IGN); 
	intr = 1;
}

int set14;

xsleep(n)
{
	xalarm(n);
	pause();
	xalarm(0);
}

xalarm(n)
{
	set14=n; 
	alarm(n);
}

sig14()
{
	signal(SIGALRM, sig14); 
	if (set14) alarm(1);
}

int	dout;
int	nhup;
int	dbflag;

/*
 *	main: get connection, set speed for line.
 *	spawn child to invoke rd to read from line, output to fd 1
 *	main line invokes wr to read tty, write to line
 */
main(ac,av)
char *av[];
{
	int fk;
	int speed;
	char *telno;
	struct sgttyb stbuf;

	signal(SIGALRM, sig14);
	if (ac < 2) {
		prf(connmsg[8]);
		exit(8);
	}
	telno = av[1];
	av += 2;
	ac -= 2;
	for (; ac > 0; av++) {
		if (equal(*av, "-t")) {
			dout = 1;
			--ac;
			continue;
		}
		if (equal(*av, "-d")) {
			dbflag++;
			continue;
		}
		if (ac < 2)
			break;
		if (equal(*av, "-s"))
			lspeed = *++av;
		else if (equal(*av, "-l"))
			devcul = *++av;
		else if (equal(*av, "-a"))
			devcua = *++av;
		else
			break;
		ac -= 2;
	}
	if (!exists(devcua) || !exists(devcul))
		exit(9);
	ln = conn(devcul, devcua, telno);
	if (ln < 0) {
		prf("Connect failed: %s",connmsg[-ln]);
		exit(-ln);
	}
	switch(atoi(lspeed)) {
	case 110:
		speed = B110;break;
	case 150:
		speed = B150;break;
	default:
	case 300:
		speed = B300;break;
	case 1200:
		speed = B1200;break;
	}
	stbuf.sg_ispeed = speed;
	stbuf.sg_ospeed = speed;
	stbuf.sg_flags = EVENP|ODDP;
	if (!dout)
		stbuf.sg_flags |= RAW;
	ioctl(TIOCSETP, ln, &stbuf);
	ioctl(TIOCEXCL, ln, (struct sgttyb *)NULL);
	ioctl(TIOCHPCL, ln, (struct sgttyb *)NULL);
	prf("Connected");
	if (dout)
		fk = -1;
	else
		fk = fork();
	nhup = (int)signal(SIGINT, SIG_IGN);
	if (fk == 0) {
		chwrsig();
		rd();
		prf("\007Lost carrier");
		exit(3);
	}
	mode(1);
	efk = fk;
	wr();
	mode(0);
	kill(fk, SIGKILL);
	wait((int *)NULL);
	stbuf.sg_ispeed = 0;
	stbuf.sg_ospeed = 0;
	ioctl(TIOCSETP, ln, &stbuf);
	prf("Disconnected");
	exit(0);
}

/*
 *	conn: establish dial-out connection.
 *	Example:  fd = conn("/dev/ttyh","/dev/dn1","4500");
 *	Returns descriptor open to tty for reading and writing.
 *	Negative values (-1...-7) denote errors in connmsg.
 *	Uses alarm and fork/wait; requires sig14 handler.
 *	Be sure to disconnect tty when done, via HUPCL or stty 0.
 */

conn(dev,acu,telno)
char *dev, *acu, *telno;
{
	struct sgttyb stbuf;
	extern errno;
	char *p, *q, b[30];
	int er, fk, dn, dh, t;
	er=0; 
	fk=(-1);
	if ((dn=open(acu,1))<0) {
		er=(errno == 6? 1:5); 
		goto X;
	}
	if ((fk=fork()) == (-1)) {
		er=4; 
		goto X;
	}
	if (fk == 0) {
		open(dev,2); 
		for (;;) pause();
	}
	xsleep(2);
	/*
	 *	copy phone #, assure EON
	 */
	p=b; 
	q=telno;
	while (*p++=(*q++))
		;
	p--;
	if (*(p-1)!='<') {
		if (*(p-1)!='-') *p++='-';
		*p++='<';
	}
	t=p-b;
	xalarm(5*t);
	t=write(dn,b,t);
	xalarm(0);
	if (t<0) {
		er=2; 
		goto X;
	}
	/* close(dn) */
	xalarm(40);		/* was 5; sometimes missed carrier */
	dh = open(dev,2);
	xalarm(0);
	if (dh<0) {
		er=(errno == 4? 3:6); 
		goto X;
	}
	ioctl(TIOCGETP, ln, &stbuf);
	stbuf.sg_flags &= ~ECHO;
	xalarm(10);
	ioctl(TIOCSETP, dh, &stbuf);
	ioctl(TIOCHPCL, dh, (struct sgttyb *)NULL);
	xalarm(0);
X: 
	if (er) close(dn);
	if (fk!=(-1)) {
		kill(fk, SIGKILL);
		xalarm(10);
		while ((t=wait((int *)NULL))!=(-1) && t!=fk);
		xalarm(0);
	}
	return (er? -er:dh);
}

/*
 *	wr: write to remote: 0 -> line.
 *	~.	terminate
 *	~<file	send file
 *	~!	local login-style shell
 *	~!cmd	execute cmd locally
 *	~$proc	execute proc locally, send output to line
 *	~%cmd	execute builtin cmd (put and take)
 */

wr()
{
	int ds,fk,lcl,x;
	char *p,b[600];
	for (;;) {
		p=b;
		while (rdc(0) == 1) {
			if (p == b) lcl=(c == '~');
			if (p == b+1 && b[0] == '~') lcl=(c!='~');
			if (c == 0) c=0177;
			if (!lcl) {
				if (wrc(ln) == 0) {
					prf("line gone"); return;
				}
			}
			if (lcl) {
				if (c == 0177) c=tkill;
				if (c == '\r' || c == '\n') goto A;
				if (!dout) wrc(0);
			}
			*p++=c;
			if (c == terase) {
				p=p-2; 
				if (p<b) p=b;
			}
			if (c == tkill || c == 0177 || c == '\r' || c == '\n') p=b;
		}
		return;
A: 
		if (!dout) echo("");
		*p=0;
		switch (b[1]) {
		case '.':
		case '\004':
			return;
		case '!':
		case '$':
			fk = fork();
			if (fk == 0) {
				char *shell = getenv("SHELL");
				if (shell == 0) shell = "/bin/sh";
				close(1);
				dup(b[1] == '$'? ln:2);
				close(ln);
				mode(0);
				if (!nhup) signal(SIGINT, SIG_DFL);
				if (b[2] == 0) execl(shell,"-",0);
				else execl(shell,"sh","-c",b+2,0);
				prf("Can't execute shell");
				exit(~0);
			}
			if (fk!=(-1)) {
				while (wait(&x)!=fk);
			}
			mode(1);
			if (b[1] == '!') echo("!");
			else {
				if (dout) echo("$");
			}
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
				if (wrc(ln) == 0) {
					x=1; 
					break;
				}
			}
			signal(SIGINT, SIG_IGN);
			close(ds);
			mode(1);
			if (x) return;
			if (dout) echo("<");
			break;
		case '>':
		case ':':
			{
			FILE *fp; char tbuff[128]; register char *q;
			sprintf(tbuff,"/tmp/cu%d",efk);
			if(NULL==(fp = fopen(tbuff,"w"))) {
				prf("Can't tell other demon to divert");
				break;
			}
			fprintf(fp,"%s\n",(b[1]=='>'?&b[2]: &b[1] ));
			if(dbflag) prf("name to be written in temporary:"),prf(&b[2]);
			fclose(fp);
			kill(efk,SIGEMT);
			}
			break;
		case '%':
			dopercen(&b[2]);
			break;
		default:
			prf("Use `~~' to start line with `~'");
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
			prf("usage: ~%%take from [to]");
			return;
		}
		if (narg < 3)
			args[2] = args[1];
		wrln("echo '~>:'");
		wrln(args[2]);
		wrln(";tee /dev/null <");
		wrln(args[1]);
		wrln(";echo '~>'\n");
		return;
	} else if (equal(args[0], "put")) {
		if (narg < 2) {
			prf("usage: ~%%put from [to]");
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
		xsleep(5);
		intr = 0;
		if (!nhup)
			signal(SIGINT, sig2);
		mode(2);
		rcount = 0;
		while(!intr && rdc(f) == 1) {
			rcount++;
			if (c == tkill || c == terase)
				wrln("\\");
			if (wrc(ln) != 1) {
				xsleep(2);
				if (wrc(ln) != 1) {
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
		xsleep(5);
		mode(1);
		return;
	}
	prf("~%%%s unknown\n", args[0]);
}

equal(s1, s2)
register char *s1, *s2;
{
	while (*s1++ == *s2)
		if (*s2++ == '\0')
			return(1);
	return(0);
}

wrln(s)
register char *s;
{
	while (*s)
		write(ln, s++, 1);
}
/*	chwrsig:  Catch orders from wr process 
 *	to instigate diversion
 */
int whoami;
chwrsig(){
	int dodiver(); 
	whoami = getpid();
	signal(SIGEMT,dodiver);
}
int ds,slnt;
int justrung;
dodiver(){
	static char dobuff[128], morejunk[256]; register char *cp; 
	FILE *fp;
	justrung = 1;
	signal(SIGEMT,dodiver);
	sprintf(dobuff,"/tmp/cu%d",whoami);
	fp = fopen(dobuff,"r");
	if(fp==NULL) prf("Couldn't open temporary");
	unlink(dobuff);
	if(dbflag) {
		prf("Name of temporary:");
		prf(dobuff);
	}
	fgets(dobuff,128,fp); fclose(fp);
	if(dbflag) {
		prf("Name of target file:");
		prf(dobuff);
	}
	for(cp = dobuff-1; *++cp; ) /* squash newline */
		if(*cp=='\n') *cp=0;
	cp = dobuff;
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
	if (*dobuff!='>' || (ds=open(cp,1))<0) ds=creat(cp,0644);
	lseek(ds, (long)0, 2);
	if(ds < 0) prf("Creat failed:"), prf(cp);
	if (ds<0) prf("Can't divert %s",cp+1);
}


/*
 *	rd: read from remote: line -> 1
 *	catch:
 *	~>[>][:][file]
 *	stuff from file...
 *	~>	(ends diversion)
 */

rd()
{
	extern int ds,slnt;
	char *p,*q,b[600];
	p=b;
	ds=(-1);
agin:
	while (rdc(ln) == 1) {
		if (!slnt) wrc(1);
		*p++=c;
		if (c!='\n') continue;
		q=p; 
		p=b;
		if (b[0]!='~' || b[1]!='>') {
			if (*(q-2) == '\r') {
				q--; 
				*(q-1)=(*q);
			}
			if (ds>=0) write(ds,b,q-b);
			continue;
		}
		if (ds>=0) close(ds);
		if (slnt) {
			write(1, b, q - b);
			write(1, CRLF, sizeof(CRLF));
		}
		if (*(q-2) == '\r') q--;
		*(q-1)=0;
		slnt=0;
		q=b+2;
		if (*q == '>') q++;
		if (*q == ':') {
			slnt=1; 
			q++;
		}
		if (*q == 0) {
			ds=(-1); 
			continue;
		}
		if (b[2]!='>' || (ds=open(q,1))<0) ds=creat(q,0644);
		lseek(ds, (long)0, 2);
		if (ds<0) prf("Can't divert %s",b+1);
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
	if (dout) return;
	ioctl(TIOCGETP, 0, &stbuf);
	tkill = stbuf.sg_kill;
	terase = stbuf.sg_erase;
	if (f == 0) {
		stbuf.sg_flags &= ~RAW;
		stbuf.sg_flags |= ECHO|CRMOD;
	}
	if (f == 1) {
		stbuf.sg_flags |= RAW;
		stbuf.sg_flags &= ECHO|CRMOD;
	}
	if (f == 2) {
		stbuf.sg_flags &= ~RAW;
		stbuf.sg_flags &= ~(ECHO|CRMOD);
	}
	ioctl(TIOCSETP, 0, &stbuf);
}

echo(s)
char *s;
{
	char *p;
	for (p=s;*p;p++);
	if (p>s) write(0,s,p-s);
	write(0,CRLF, sizeof(CRLF));
}

prf(f, s)
char *f;
char *s;
{
	fprintf(stderr, f, s);
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

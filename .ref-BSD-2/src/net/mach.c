/* Copyright (c) 1979 Regents of the University of California */
/*
   This file is meant to handle all the machine
   dependencies in the network code.
   Everything is conditionally compiled.

   It can be uses w/o network stuff to simulate
   v7 for other programs, too.

Table of machine dependencies (not isolated in mach.h, mach.c)

VAX			CC			CORY
---			--			----
			M_CC stuff		mail -r
	
	
	
*/
# include <stdio.h>
# include "mach.h"

# ifndef CC
submit(a) {}
# endif

# ifdef FUID
setgid() {};
# endif

char vaxtovax;
long fixuplong(a)
	long a; {
# ifdef VAX
	register char *p,c1,c2;
	char c3,c4;
	if(!vaxtovax){
		p = (char*) &a;
		c1 = *p++;
		c2 = *p++;
		c3 = *p++;
		c4 = *p++;
		p = (char*) &a;
		*p++ = c3;
		*p++ = c4;
		*p++ = c1;
		*p++ = c2;
		}
# endif
	return(a);
	}
/* always returns a string */
char *getun(uid){
	struct passwd *pwd;
	static int ouid = -1;
	static char oresult[20] = "";
	if(uid != ouid){
# ifdef HPASSWD
		if(getname(uid,oresult) != 0)
# endif
			{
			pwd = getpwuid(uid);
			strcpy(oresult,pwd == NULL ? "UNKNOWN" : pwd->pw_name);
			}
		}
	ouid = uid;
	return(oresult);
	}
/* handle the regular unix and local mods difference for user id's */
/* this call returns the 1 word uid = to what getuid will return */
guid(uid,gid){
# ifdef FUID
	return((uid & 0377) | (gid << 8));
# else
	return(uid);
# endif
	}

# ifdef OLDTTY
isatty(i){
	return(ttyn(i) != 'x');
	}
char *ttyname(i){		/* return NULL if not TTY */
	char c;
	static char ttystr[] = "/dev/ttyx";
	c = ttyn(i);
	ttystr[8] = c;
	return(c == 'x' ? NULL : ttystr);
	}
# endif

# ifdef CCTTY
# undef ttyname()
myttyname(i){		/* return NULL for non tty */
	static char s[15],*p;
	p = ttyname(i);
	if(p == NULL)return(NULL);
	strcpy(s,"/dev/");
	strcat(s,p);
	return(s);
	}
# define ttyname(S) myttyname(S)
# endif

/* get passwd from passwdf */
getpwdf(pwd)
  struct passwd *pwd; {
# ifdef PASSWDF
# ifndef TESTING
	register char *p, *q;
	char buf1[BUFSIZ], found;
	FILE *pw;
	pwd->pw_passwd[0] = 0;
	pw = fopen("/etc/passwdf","r");
	if(pw == NULL){
		/*
		error("/etc/passwdf: %s",sys_errlist[errno]);
		*/
		return;
		}
	found = 0;
	while(fgets(buf1,BUFSIZ,pw) != NULL){
		for(p=buf1; *p && *p != ':'; p++);
		*p = 0;
		if(strcmp(buf1,pwd->pw_name) == 0){
			found = 1;
			break;
			}
		}
	fclose(pw);
	if(!found)return;
	q = ++p;
	for(;*p && *p != ':';p++);
	*p = 0;
	strcpy(pwd->pw_passwd,q);
	/*
	debug("user %s passwd %s %s",pwd->pw_name,pwd->pw_passwd);
	*/
# endif
# endif
	}
/*
   these are all the v7 routines not available on the v6 machines
*/

# ifndef V7

char **environ;			/* global environment pointer */

ioctl(a,b,c){
	return(0);		/* always succeeds */
	}
long atol(s)
  register char *s; {
	long i = 0;
	while('0' <= *s && *s <= '9')
		i = i * 10 + (*s++ - '0');
	return(i);
	}
long gettime(){
	long tt;
	time(&tt);
	return(tt);
	}
long getsize(str)
  struct stat *str; {
	long wk;
	wk = ((long)(str->st_size0 & 0377)) << 16;
	wk += (long)((unsigned)str->st_size1);
	return(wk);
	}
char *getenv(){	/* always returns home directory */
	char *hdir = 0, tstr[20];
	struct passwd *pwd;
	int t;
	t = ttyn(2);
# ifdef OLDTTY
	if(t == 'x')t = ttyn(1);
	if(t == 'x')t = ttyn(0);
	if(t != 'x' && hget(t) == 0)hdir = hgethome();
# endif
# ifdef CCTTY
	if(t == -1)t = ttyn(1);
	if(t == -1)t = ttyn(0);
	if(t != -1 && hget(t) == 0)hdir = hgethome();
# endif
	if(hdir == 0){
		pwd = getpwuid(getuid());
		if(pwd != NULL)hdir = pwd->pw_shell;
		}
	return(hdir);
	}

/* doesn't handle split passwd files */
struct passwd *
getpwuid(uid)
register uid;
{
	register struct passwd *p;
	struct passwd *getpwent();

	setpwent();
	while( (p = getpwent()) && guid(p->pw_uid,p->pw_gid) != uid );
	endpwent();
	return(p);
}

static char PASSWD[]	= "/etc/passwd";
static char EMPTY[] = "";
static FILE *pwf = NULL;
static char line[BUFSIZ+1];
static struct passwd passwd;

setpwent()
{
	if( pwf == NULL )
		pwf = fopen( PASSWD, "r" );
	else
		rewind( pwf );
}

endpwent()
{
	if( pwf != NULL ){
		fclose( pwf );
		pwf = NULL;
	}
}

static char *
pwskip(p)
register char *p;
{
	while( *p && *p != ':' )
		++p;
	if( *p ) *p++ = 0;
	return(p);
}

struct passwd *
getpwent()
{
	register char *p;

	if (pwf == NULL) {
		if( (pwf = fopen( PASSWD, "r" )) == NULL )
			return(0);
	}
	p = fgets(line, BUFSIZ, pwf);
	if (p==NULL)
		return(0);
	passwd.pw_name = p;
	p = pwskip(p);
	passwd.pw_passwd = p;
	p = pwskip(p);
	passwd.pw_uid = atoi(p);
	p = pwskip(p);
	passwd.pw_gid = atoi(p);
	passwd.pw_quota = 0;
	passwd.pw_comment = EMPTY;
	p = pwskip(p);
	passwd.pw_gecos = p;
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
	passwd.pw_shell = p;
	while(*p && *p != '\n') p++;
	*p = '\0';
	return(&passwd);
}

struct passwd *
getpwnam(name)
char *name;
{
	register struct passwd *p;
	struct passwd *getpwent();

	setpwent();
	while( (p = getpwent()) && strcmp(name,p->pw_name) );
	endpwent();
	return(p);
}
/* returns NULL if not found */
char *getlogin(){	/* get login name of current person */
	static struct utmp utmpstr;
	char *s, *res;
	FILE *fp;
	res = NULL;
	s = ttyname(2);
	if(s == NULL)s = ttyname(1);
	if(s == NULL)s = ttyname(0);
	if(s == NULL)return(NULL);
	fp = fopen("/etc/utmp","r");
	if(fp == NULL)return(NULL);
	while(fread(&utmpstr,1,sizeof utmpstr,fp) == sizeof utmpstr)
# ifdef OLDTTY
		if(utmpstr.ut_tty == s[8])
# else
		if(strcmp(utmpstr.ut_line,s+5) == 0)
# endif
			res = utmpstr.ut_name;
	fclose(fp);
	return(res);
	}
/*
 * Unix routine to do an "fopen" on file descriptor
 * The mode has to be repeated because you can't query its
 * status
 */

FILE *
fdopen(fd, mode)
register char *mode;
{
	extern int errno;
	register FILE *iop;
	extern FILE *_lastbuf;

	for (iop = _iob; iop->_flag&(_IOREAD|_IOWRT); iop++)
		if (iop >= _lastbuf)
			return(NULL);
	iop->_cnt = 0;
	iop->_file = fd;
	if (*mode != 'r') {
		iop->_flag |= _IOWRT;
		if (*mode == 'a')
			lseek(fd, 0L, 2);
	} else
		iop->_flag |= _IOREAD;
	return(iop);
}
system(s)
char *s;
{
	int status, pid, w;
	register int (*istat)(), (*qstat)();

	if ((pid = fork()) == 0) {
		execl("/bin/sh", "sh", "-c", s, 0);
		_exit(127);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	return(status);
}
/* getpw - one in -lS doesn't work at Berkeley for gid > 127 */
getpw(uid, buf)
int uid;
char buf[];
{
	static FILE *pwf;
	register n, c;
	register char *bp;
	int n1;
	int id;

	if(pwf == 0)
		pwf = fopen("/etc/passwd", "r");
	if(pwf == NULL)
		return(1);
	rewind(pwf);

	for (;;) {
		bp = buf;
		while((c=getc(pwf)) != '\n') {
			if(c <= 0)
				return(1);
			*bp++ = c;
		}
		*bp++ = '\0';
		bp = buf;
		n = 3;
		while(--n)
			while((c = *bp++) != ':')
				if(c == '\n')
					return(1);
		while((c = *bp++) != ':') {
			if(!isdigit(c))
				continue;
			n = n*10+c-'0';
		}
		n1 = 0;
		while((c = *bp++) != ':') {
			if(!isdigit(c))
				continue;
			n1 = n1*10 + c - '0';
		}
		id = (n1 << 8) | (n&0377);
		if(id == uid)
			return(0);
	}
	return(1);
}
char *
getpass(prompt)
char *prompt;
{
	struct sgttyb ttyb;
	int flags;
	register char *p;
	register c;
	FILE *fi = NULL;
	static char pbuf[9];
	int (*signal())();
	int (*sig)();

	/*	modified because Cory needs super-user to stty /dev/tty */
# ifndef CORY
	if ((fi = fopen("/dev/tty", "r")) == NULL)
		fi = stdin;
	else
		setbuf(fi, (char *)NULL);
	gtty(fileno(fi), &ttyb);
# else
	if(gtty(0,&ttyb) >= 0)fi = stdin;
	else if(gtty(2,&ttyb) >= 0)fi = stderr;
	else {
		pbuf[0] = 0;
		return(pbuf);
		}
# endif
	sig = signal(SIGINT, SIG_IGN);
	flags = ttyb.sg_flags;
	ttyb.sg_flags &= ~ECHO;
	if(stty(fileno(fi), &ttyb) < 0) perror("stty:");
	fprintf(stderr, prompt);
	for (p=pbuf; (c = getc(fi))!='\n' && c!=EOF;) {
		if (p < &pbuf[8])
			*p++ = c;
	}
	*p = '\0';
	fprintf(stderr, "\n");
	ttyb.sg_flags = flags;
	stty(fileno(fi), &ttyb);
	signal(SIGINT, sig);
# ifndef CORY
	if (fi != stdin)
		fclose(fi);
# endif
	return(pbuf);
}
/* end of non-vax v7 routines */
# endif

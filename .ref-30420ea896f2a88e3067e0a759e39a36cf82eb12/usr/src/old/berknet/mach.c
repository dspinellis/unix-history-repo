static char sccsid[] = "@(#)mach.c	4.2	(Berkeley)	%G%";

/* sccs id variable */
static char *mach_sid = "@(#)mach.c	1.6";
/*

   This file is meant to handle all the machine
   dependencies in the network code.
   Everything is conditionally compiled.

   It can be uses w/o network stuff to simulate
   v7 for other programs, too.
*/
# include <stdio.h>
# include "mach.h"

char shomedir[100];

int debugflg;

/* the CC and SRC machines have the submit() call */
# ifndef CC
# ifndef SRC
submit(a) {}
# endif
# endif

# ifdef FUID
setgid() {};
# endif

/* 
   Set the owner uid/gid of a file.
   On v7, this is done by the chown command
   with three args - (file, uid, gid).
   On Vanilla V6 this is done using the
   top byte of the second parameter as the gid byte.
   On Berkeley Funny uids on V6, no gid is specified.
*/
mchown(sfn,uid,gid)
	char *sfn;
	int uid;
	int gid;
{
# ifndef V6
	chown(sfn,uid,gid);
# else
# ifndef FUID
		uid = uidmask(uid);
		uid = ((gid&0377) << 8) | (uid & 0377);
# endif
	chown(sfn,uid);
	if(debugflg)
		fprintf(stderr, "chown %s to %d(%o)\n",sfn,uid,uid);
# endif
}
	

/*
	SnFromuid(uid)

	The login name corresponding to uid.
	Reads the password file.
	Successive calls overwrite the static string returned.
	Returns NULL if error.
*/
char *SnFromUid(uid)
	register int uid;
{
	register struct passwd *pwd;
	static int ouid = -1;
	static char oresult[20] = "";
	uid = uidmask(uid);
	if(uid == ouid)
		return(oresult);
# ifdef HPASSWD
	if(getname(uid,oresult) == 0){
		ouid = uid;
		return(oresult);
	}
# endif
	pwd = getpwuid(uid);
	if(pwd != NULL){
		strcpy(oresult,pwd->pw_name);
		ouid = uid;
		return(oresult);
	}
	return(NULL);
}
uidfromsn(sn)
register char *sn;
{
	register int him = -1;
	register struct passwd *pwd;
# ifdef HPASSWD
	him = getuserid(sn);
# endif
	if(him == -1){
		pwd = getpwnam(sn);
		if(pwd != NULL)him = guid(pwd->pw_uid,pwd->pw_gid);
		}
	return(him);
}

/* handle the regular unix and local mods difference for user id's */
/* this call returns the 1 word uid = to what getuid will return */
guid(uid,gid){
	uid = uidmask(uid);
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
char *myttyname(i){		/* return NULL for non tty */
	static char s[15],*p;
	p = ttyname(i);
	if(p == NULL)return(NULL);
	strcpy(s,"/dev/");
	strcat(s,p);
	return(s);
	}
# define ttyname(S) myttyname(S)
# endif

/* expand control chars in string s */
expandcc(s)
  register char *s; {
	char stemp[100];
	register char *p;

	if(s == NULL)return;
	strcpy(stemp,s);
	p = stemp;
	while(*p){
		if(!isprint(*p)){
			*s++ = '^';
			*s++ = *p++ + 0140;
		}
		else *s++ = *p++;
	}
}

/* get passwd from passwdf */
getpwdf(pwd)
  struct passwd *pwd; {
# ifdef PASSWDF
# ifndef TESTING
	register char *p, *q;
	char buf1[BUFSIZ], found;
	FILE *pw;
	debug("reading passwdf\n");
	pwd->pw_passwd[0] = 0;
	pw = fopen("/etc/passwdf","r");
	if(pw == NULL) return;
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
	getutmp()
	return a pointer to the system utmp structure associated with
	terminal sttyname, e.g. "/dev/tty3"
	Is version independent-- will work on v6 systems
	return NULL if error
*/
struct utmp *getutmp(sttyname)
char *sttyname;
{
# ifdef OLDTTY
	struct v6utmp {
		char v6ut_name[8];
		char v6ut_tty;
		char v6ut_fill;
		long v6ut_time;
		int  v6ut_fl1;
		} v6utmpstr;
# endif
	static struct utmp utmpstr;
	FILE *fdutmp;

	debug("reading utmp\n");
	if(sttyname == NULL || sttyname[0] == 0)return(NULL);

	fdutmp = fopen("/etc/utmp","r");
	if(fdutmp == NULL)return(NULL);

# ifndef OLDTTY
	while(fread(&utmpstr,1,sizeof utmpstr,fdutmp) == sizeof utmpstr)
		if(strcmp(utmpstr.ut_line,sttyname+5) == 0){
			fclose(fdutmp);
			return(&utmpstr);
		}
# else
	while(fread(&v6utmpstr,1,sizeof v6utmpstr,fdutmp) == sizeof v6utmpstr)
		if(v6utmpstr.v6ut_tty == sttyname[8]){
			strcpy(utmpstr.ut_name,v6utmpstr.v6ut_name);
			strcpy(utmpstr.ut_line,"ttyx");
			utmpstr.ut_line[3] = v6utmpstr.v6ut_tty;
			utmpstr.ut_time = v6utmpstr.v6ut_time;
			fclose(fdutmp);
			return(&utmpstr);
		}
# endif
	fclose(fdutmp);
	return(NULL);
}

/*
   these are all the v7 routines not available on the v6 machines
*/

# ifdef V6

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
/*
	getenv("HOME")

	always returns home directory.
	returns NULL if there is error.
*/
char *getenv(){
	register char *shdir = NULL;
	register struct passwd *pwd;
	register int it;
	if(shomedir[0] != 0)return(shomedir);
# ifdef BERKELEY
	/* hget only works on Berkeley machines */
	it = ttyn(2);
# ifdef OLDTTY
	if(it == 'x')it = ttyn(1);
	if(it == 'x')it = ttyn(0);
	if(it != 'x' && hget(it) == 0)shdir = hgethome();
# endif
# ifdef CCTTY
	if(it == -1)it = ttyn(1);
	if(it == -1)it = ttyn(0);
	if(it != -1 && hget(it) == 0)shdir = hgethome();
# endif
# endif
	if(shdir == NULL){
		pwd = PwdCurrent();
		if(pwd != NULL)shdir = pwd->pw_dir;
		}
	if(shdir != NULL)strcpy(shomedir,shdir);
	return(shdir);
	}

/* doesn't handle split passwd files */
struct passwd *
getpwuid(uid)
register uid;
{
	register struct passwd *p;
	struct passwd *getpwent();

	uid = uidmask(uid);
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
	debug("reading passwd\n");
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
	passwd.pw_uid = uidmask(passwd.pw_uid);
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
/*
	getlogin()

	Return current user name by looking at /etc/utmp (calls getutmp()).
	Returns NULL if not found.
*/
char *getlogin()
{
	register struct utmp *putmp;
	register char *s;
	char sttyname[30];

	if(isatty(2))strcpy(sttyname,ttyname(2));
	else if(isatty(0))strcpy(sttyname,ttyname(0));
	else if(isatty(1))strcpy(sttyname,ttyname(1));
	else return(NULL);

	putmp = getutmp(sttyname);
	if(putmp == NULL)return(NULL);
	s = putmp->ut_name;
	while(*s != 0 && *s != ' ')s++;
	*s = 0;
	if(putmp->ut_name[0] == 0)return(NULL);
	return(putmp->ut_name);
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

	while((pid = fork()) == -1)sleep(2);
	if (pid == 0) {
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
	if ((fi = fopen("/dev/tty", "r")) == NULL)
		fi = stdin;
	else
		setbuf(fi, (char *)NULL);
	if(gtty(fileno(fi),&ttyb) < 0){
		pbuf[0] = 0;
		return(pbuf);
	}
	/*
	if(gtty(0,&ttyb) >= 0)fi = stdin;
	else if(gtty(2,&ttyb) >= 0)fi = stderr;
	else {
		pbuf[0] = 0;
		return(pbuf);
		}
	*/
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
	if (fi != stdin)
		fclose(fi);
	return(pbuf);
}
/*
 * Compare strings (at most n bytes):  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */

strncmp(s1, s2, n)
register char *s1, *s2;
register n;
{

	while (--n >= 0 && *s1 == *s2++)
		if (*s1++ == '\0')
			return(0);
	return(n<0 ? 0 : *s1 - *--s2);
}

/* set the umask, ignore in v6 */

umask(n){}

/* end of non-vax v7 routines */
# endif
/*
	PwdCurrent()

	Read the password file and return pwd to
	entry for current user.
	Return NULL if error.

	This code is a little screwed up because of the conventions
	regarding the state of the utmp file after someone su's--
	either to root or to another person.
	The final decision was to return getpwuid(getuid) if
	the machine has one login name per userid,
	and if there are multiple login names per userid, to
	search the passwd file for the getlogin() name and return
	the passwd file entry for that.
	If there is no utmp entry, just use the userid.
	This means that people who su on machine with multiple
	user-id's will get the passwd entry for the account recorded
	in the utmp file, not their current userid.
*/
struct passwd *
PwdCurrent()
{
	register struct passwd *pwd;
	register char *sn;

# ifdef MULTNAMS
	sn = getlogin();
	if(sn != NULL && sn[0] != 0 && sn[0] != ' '){
		pwd = getpwnam(sn);
		if(pwd != NULL)return(pwd);
	}
# endif

	return(getpwuid(uidmask(getuid())));
}
/*VARARGS0*/
debug(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t)
char *s; {
	if(debugflg){
		printf(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t);
		putchar('\n');
		}
	}

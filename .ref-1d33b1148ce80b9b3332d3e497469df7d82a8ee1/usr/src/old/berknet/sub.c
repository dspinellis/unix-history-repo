static char sccsid[] = "@(#)sub.c	4.5	(Berkeley)	%G%";

/*
	sub.c

	support procedures

	the following procedures end up reading the passwd file
	or the passwdf file and are to be avoided.

	getpwuid(uid)
	getpwnam(sn)
	PwdCurrent()
	getenv("HOME")		maybe if hget, hgethome don't work
	SnFromUid(uid)		maybe if hashed passwd stuff doesn't work
	SnCurrent()		maybe if getlogin fails calls SnFromUid(uid)
	getpwf()
	passwdent(uid,sn)
*/

# include "defs.h"
# include "config.h"
# include <varargs.h>

/* global variables */
int debugflg = DBV;	/* debug flag */
char local = LOCAL;	/* the machine we're on */
struct userinfo status;

char netcmd[]  =	NETCMD;
char resfile[] = 	RESFILE;
char senddir[] =	SENDDIR;
char Bsh[] =		BINSH;

char shomedir[100];

/*
	passwdent()

	Read the password file looking for current user's entry.
	Fill in the status structure.
	Has the (dangerous) side effect of giving a value to getenv("HOME").
*/
passwdent()
{
	register char *u;
	register struct passwd *pwd;
#ifdef CRN
	register struct gecos *gcos;
#endif
	pwd = PwdCurrent();
	if(pwd == NULL){
		err("Bad uid/username\n");
		return;
	}
	strcpy(status.localname,pwd->pw_name);
	status.muid = guid(pwd->pw_uid,pwd->pw_gid);
	status.mgid = pwd->pw_gid;
#ifdef CRN
	if( (gcos=pwgecos( pwd->pw_gecos) ) == NULL )
		strcpy( status.jobno, MAGICCRN );
	else {
		if( debugflg )
			debug( "crn found = %s\n", gcos->gc_crn );
		if( isalpha( gcos->gc_crn[0] ) ||
			isdigit( gcos->gc_crn[0] ) )
			strcpy( status.jobno, gcos->gc_crn );
		else
			strcpy( status.jobno, MAGICCRN );
	}
#else
	strcpy( status.jobno, "XYZZ" );
#endif
	strcpy(status.dir,pwd->pw_dir);
	strcpy(shomedir,pwd->pw_dir);		/* side effect */
	u = pwd->pw_shell;
	if(u[0] == 0 || strcmp(u,"/bin/sbash") == 0)u= Bsh;
	strcpy(status.loginshell,u);
	}
/*
	promptlogin(mchto)

	ask user for login and passwd on mchto.
	make sure status.localname has a value before calling
	this.  One way is to call passwdent().
*/
promptlogin(mchto)
	char mchto;
{
	char buf[BUFSIZ], mch;
	FILE *wf;
	int c;
	if(status.login[0]==0 || status.force){
		buf[0] = 0;
		wf = fopen("/dev/tty","r");
		if(wf != NULL){
			fprintf(stderr,"Name (%s:%s): ",longname(mchto),
				status.localname);
			if(fgets(buf, BUFSIZ, wf) != buf){
				perror("fgets");
				exit(EX_OSERR);
				}
			c = strlen(buf);
			buf[c > 0 ? c-1 : 0] = 0;
			if(c > 10){
				err("Login name too long.\n");
				exit(EX_USAGE);
				}
			if(FMemberSCh(buf,' ')){
				err("Login names don't have blanks in them.\n");
				exit(EX_USAGE);
				}
			fclose(wf);
			}
		if(buf[0] == 0)strcpy(buf,status.localname);
		mch = MchSFromAddr(status.login,buf);
		if(mch != local && mch != mchto){
			err("Must specify login name on %s machine\n",
				longname(mchto));
			exit(EX_USAGE);
			}
		}
	if(strcmp(status.login,"network") != 0
		&& (status.mpasswd[0]== 0 || status.force)){
		sprintf(buf,"Password (%s:%s):",
			longname(mchto), status.login);
		strcpy(status.mpasswd,getpass(buf));
		}
	if(status.login[0] == 0) strcpy(status.login,status.localname);
	if(status.mpasswd[0] == 0)strcpy(status.mpasswd,"\"\"");
	status.force = 0;
	}

#define	tst(a,b)	(*mode == 'r'? (b) : (a))
#define	RDR	0
#define	WTR	1
static	int	popen_pid[20];

/* return a file descriptor suitable for writing, send to 
  user toaddress from fromaddress,
  if cautious != 0 then don't do any forwarding
  hopcnt is passed thru the mail program.
	  normal value is 0
  */
FILE *
mailopen(toaddress, fromaddress, cautious, hopcnt)
char *toaddress, *fromaddress;
int cautious, hopcnt;
{
	char	cmd[100];
	char	*mode = "w";
	int p[2];
	register myside, hisside, pid;
	char shopcnt[20];

	if(pipe(p) < 0)
		return NULL;
	myside = tst(p[WTR], p[RDR]);
	hisside = tst(p[RDR], p[WTR]);
	while((pid = fork()) == -1)sleep(2);
	if(pid == 0) {
		/* myside and hisside reverse roles in child */
		close(myside);
		/*
		dup2(hisside, tst(0, 1));
		*/
		close(0);
		dup(hisside);
		close(hisside);
		sprintf(shopcnt,"%d",hopcnt);
		if(fromaddress != NULL){
			/* by convention, MAILFWD1 may forward this mail
			   and response messages shouldn't be forwarded */
			if(!cautious && !FMemberSCh(toaddress,'/')){
# ifdef SENDMAIL
				mexecl("/usr/lib/sendmail",
					"sendmail", "-oee", "-r", fromaddress,
					"-h",shopcnt, toaddress, 0);
# endif SENDMAIL
				mexecl(MAILFWD1, "mail","-r",fromaddress,
					"-h",shopcnt,toaddress,0);
			}
			mexecl(SYSMAIL2, "mail","-d","-r",fromaddress,
				"-h", shopcnt,toaddress,0);
		} else {
			if(!cautious && !FMemberSCh(toaddress,'/')){
# ifdef SENDMAIL
				mexecl("/usr/lib/sendmail",
					"sendmail", "-ee", "-h", shopcnt,
					toaddress, 0);
# endif
				mexecl(MAILFWD1, "mail","-h", shopcnt,
					toaddress,0);
			}
			mexecl(SYSMAIL2, "mail","-d","-h", shopcnt,toaddress,0);
		}
		perror(SYSMAIL2);
		exit(EX_UNAVAILABLE);
	}
	if(pid == -1)
		return NULL;
	popen_pid[myside] = pid;
	close(hisside);
	return(fdopen(myside, mode));
}

mailclose(ptr)
FILE *ptr;
{
	register f, r, (*hstat)(), (*istat)(), (*qstat)();
	int status;

	f = fileno(ptr);
	fclose(ptr);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while((r = wait(&status)) != popen_pid[f] && r != -1)
		;
	if(r == -1)
		status = -1;
	signal(SIGINT, istat);
	signal(SIGQUIT, qstat);
	signal(SIGHUP, hstat);
	return(status);
}
	
/* 
	ch means 'a'-'z', inx in 0..25
	ch means '0'-'9', inx in 26..35
*/
chtoinx(ch) {
	if('a' <= ch && ch <= 'z')
		return(ch - 'a');
	if('0' <= ch && ch <= '9')
		return((ch - '0') + 26);
	err("bad ch");
}

/* 
	inx is 0..25 means 'a'-'z'
	inx is 26..35 means '0'-'9'
*/
inxtoch(inx) {
	if(0 <= inx && inx <= 25)
		return(inx + 'a');
	if(26 <= inx && inx <= 35)
		return('0' + (inx - 26));
	err("bad ch");
}

/* determine through machine */
gothru(from,to){
	register int i;
	switch(from){
# ifdef RAND
	case 'a':	i = configA[chtoinx(to)]; break;
	case 'b':	i = configB[chtoinx(to)]; break;
	case 'c':	i = configC[chtoinx(to)]; break;
# endif
# ifdef NOSC
	case 'a':	i = configA[chtoinx(to)]; break;
	case 'c':	i = configC[chtoinx(to)]; break;
	case 'm':	i = configM[chtoinx(to)]; break;
# endif
# ifdef BERKELEY
	/* for Berkeley */
	case 'a':	i = configA[chtoinx(to)]; break;
	case 'b':	i = configB[chtoinx(to)]; break;
	case 'c':	i = configC[chtoinx(to)]; break;
	case 'd':	i = configD[chtoinx(to)]; break;
	case 'e':	i = configE[chtoinx(to)]; break;
	case 'f':	i = configF[chtoinx(to)]; break;
	case 'g':	i = configG[chtoinx(to)]; break;
	case 'h':	i = configH[chtoinx(to)]; break;
	case 'i':	i = configI[chtoinx(to)]; break;
	case 'j':	i = configJ[chtoinx(to)]; break;
	case 'k':	i = configK[chtoinx(to)]; break;
	case 'l':	i = configL[chtoinx(to)]; break;
	case 'm':	i = configM[chtoinx(to)]; break;
	case 'n':	i = configN[chtoinx(to)]; break;
	case 'o':	i = configO[chtoinx(to)]; break;
	case 'p':	i = configP[chtoinx(to)]; break;
	case 'r':	i = configR[chtoinx(to)]; break;
	case 's':	i = configS[chtoinx(to)]; break;
	case 't':	i = configT[chtoinx(to)]; break;
	case 'u':	i = configU[chtoinx(to)]; break;
	case 'v':	i = configV[chtoinx(to)]; break;
	case 'w':	i = configW[chtoinx(to)]; break;
	case 'x':	i = configX[chtoinx(to)]; break;
	case 'y':	i = configY[chtoinx(to)]; break;
	case 'z':	i = configZ[chtoinx(to)]; break;
# endif
	default:	i = 0; break;
	}
	return(i);
	}

# define NPARMS 20
/* prints out commands before executing them */
/*VARARGS0*/
mexecl(va_alist)
  	va_dcl
{
	char *arr[NPARMS], *file, *f;
	va_list ap;
	register int i;
	va_start(ap);
	file = va_arg(ap, char *);
	i = 0;
	while(f = va_arg(ap, char *)){
		if(i >= NPARMS){
			err("too many args");
			arr[NPARMS-1] = NULL;
			break;
			}
		if(debugflg) err("'%s' ",f);
		arr[i++] = f;
		}
	arr[i] = NULL;
	va_end(ap);
	if(debugflg) putc('\n',stderr);
	execv(file, arr);
	}

/* prints out commands before executing them */
mexecv(s,p)
  register char *s, **p;{
	register int i;
	if(debugflg){
		err("'%s' ",s);
		for(i=0; p[i]; i++)err("'%s' ",p[i]);
		putc('\n',stderr);
		}
	execv(s,p);
	}

/*VARARGS0*/
/* fills in -l - -p from commands like rcp */
/* must be called with at least two arguments */
kexecl(va_alist)
  va_dcl
{
	char *a[NPARMS], i = 1, *file;
	va_list ap;
	va_start(ap);
	file = va_arg(ap, char *);
	a[0] = va_arg(ap, char *);
	if(status.login[0]){
		a[i++] = "-l";
		a[i++] = status.login;
		}
	if(status.mpasswd[0]){
		a[i++] = "-p";
		a[i++] = status.mpasswd;
		}
	if(status.nonotify)a[i++] = "-b";
	if(status.force)   a[i++] = "-f";
	if(status.quiet)   a[i++] = "-q";
	if(status.nowrite) a[i++] = "-n";
	while (a[i++] = va_arg(ap, char *)){
		if(i >= NPARMS){
			err("too many args");
			a[NPARMS-1] = NULL;
			break;
			}
		};
	mexecv(file, a);
	}

/*
	MchSFromAddr(sn,addr)

	take an address of the form "mach:username"
	and return mch as the 1 char code of "mach" and
	in sn put "username".
	If addr has no colon in it, return mch==local, sn==addr.
	Return 0 for mch if host unknown.
*/
MchSFromAddr(sn,addr)
	char *sn, *addr;
{
	char fcolon = 0, *s, mch, stemp[BUFSIZ];

	/* assume addr is a local address */

	strcpy(stemp,addr);
	s = stemp;
	while(*s){
		if(*s == ':'){
			fcolon = 1;
			*s++ = 0;
			break;
		}
		s++;
	}
	if(fcolon != 1){
		/* sn better be the right size for addr */
		mch = local;
		strcpy(sn,addr);
		return(mch);
	}

	/* addr has a colon in it, s pts to name */
	mch = lookup(stemp);
	strcpy(sn,s);
	return(mch);
}


/* returns a single character for machine S */
/* returns 0 for unknown host */
lookup(s)
  register char *s; {
	register struct tt *t;
	if(strlen(s) == 1)return(isupper(*s) ? tolower(*s) : *s);
	for(t = table; t->bigname; t++)
		if(streql(s,t->bigname) == 0)return(t->lname);
	return(0);
	}

/* returns a long name (string) for single character machine c */
char *longname(c)
  register char c;
	{
	register struct tt *t;
	if(c == 0)return("UNKNOWN");
	for(t = table; t->bigname; t++)
		if(c == t->lname)return(t->bigname);
	return("UNKNOWN");
	}
/*
	FMemberSCh(s,ch)

	return 1 if ch is a character in string s.
	0 otherwise.
*/
FMemberSCh(s,ch)
	register char *s, ch; 
{
	while(*s)if(*s++ == ch)return(1);
	return(0);
}

/* return a static string with the form "X hrs X mins X secs" */
/* t is # of secs */
char *comptime(t)
  long t; {
	static char str[30];
	char buf[20];
	long w;
	str[0] = 0;
	w = t/3600L;
	if(w > 0L){
		sprintf(buf,"%ld hr ",w);
		strcat(str,buf);
		}
	t = t % 3600L;
	w = t/60L;
	if(w > 0L){
		sprintf(buf,"%ld min ",w);
		strcat(str,buf);
		}
	t = t % 60L;
	sprintf(buf,"%ld sec",t);
	strcat(str,buf);
	return(str);
	}
/*
	parseparmlist(string)

	parses variable parameter lists in string,
	as defined in genparmlist in net.c
*/
parseparmlist(parmlist)
	char *parmlist;
{
	while(*parmlist && *parmlist != '(')parmlist++;
}

/* just like strcmp except upper- and lower-case are ignored */
streql(s1,s2)
  char *s1, *s2; {
	char a,b;
	while(*s1 && *s2){
		a = isupper(*s1) ? tolower(*s1) : *s1;
		b = isupper(*s2) ? tolower(*s2) : *s2;
		if(a < b)return(-1);
		if(a > b)return(1);
		s1++, s2++;
		}
	if(*s2)return(-1);
	if(*s1)return(1);
	return(0);
	}
/* VARARGS0 */
err(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) {
	fprintf(stderr,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r);
	}

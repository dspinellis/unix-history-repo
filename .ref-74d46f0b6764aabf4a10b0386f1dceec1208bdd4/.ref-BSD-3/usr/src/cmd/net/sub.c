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

int debugflg = DBV;	/* debug flag */
char local = LOCAL;	/* the machine we're on */

char netcmd[]  =	NETCMD;
char resfile[] = 	RESFILE;
char senddir[] =	SENDDIR;
char Bsh[] =		BINSH;

char shomedir[BUFSIZ];

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
	pwd = PwdCurrent();
	if(pwd == NULL){
		err("Bad uid/username\n");
		return;
	}
	strcpy(status.localname,pwd->pw_name);
	status.muid = guid(pwd->pw_uid,pwd->pw_gid);
	status.mgid = pwd->pw_gid;
	if(isdigit(pwd->pw_gecos[0]))status.jobno = atoi(pwd->pw_gecos);
	else status.jobno = 32767;
	strcpy(status.dir,pwd->pw_dir);
	strcpy(shomedir,pwd->pw_dir);		/* side effect */
	u = pwd->pw_shell;
	if(u[0] == 0)u= Bsh;
	strcpy(status.loginshell,u);
	}
/*
	promptlogin(mchto)

	ask user for login and passwd on mchto.
	make sure status.localname has a value before calling
	this.  One way is to call passwdent(), another is to
	strcpy(status.localname,SnCurrent()).
*/
promptlogin(mchto)
	char mchto;
{
	char buf[BUFSIZ], mch;
	FILE *wf;
	int c;
	if(status.mpasswd[0] == 0 || status.login[0] == 0 || status.force){
		wf = fopen("/dev/tty","r");
		if(wf != NULL){
			if(status.login[0]==0 || status.force){
				printf("Name (%s:%s): ",longname(mchto),
					status.localname);
				if(fgets(buf, BUFSIZ, wf) != buf){
					perror("fgets");
					exit(1);
					}
				c = strlen(buf);
				buf[c > 0 ? c-1 : 0] = 0;
				if(c > 10){
					err("Login name too long.\n");
					exit(1);
					}
				if(FMemberSCh(buf,' ')){
					err("Login names don't have blanks in them.\n");
					exit(1);
					}
				if(buf[0] == 0)strcpy(buf,status.localname);
				mch = MchSFromAddr(status.login,buf);
				if(mch != local && mch != mchto){
					err(
				"Must specify login name on %s machine\n",
						longname(mchto));
					exit(1);
				}
			}
			if(strcmp(status.login,"network") != 0
				&& (status.mpasswd[0]== 0 || status.force)){
				sprintf(buf,"Password (%s:%s):",
					longname(mchto), status.login);
				strcpy(status.mpasswd,getpass(buf));
				}
			fclose(wf);
			}
		}
	if(status.login[0] == 0) strcpy(status.login,status.localname);
	if(status.mpasswd[0] == 0)strcpy(status.mpasswd,"\"\"");
	status.force = 0;
	}
	
/* determine through machine */
gothru(from,to){
	register int i;
	switch(from){
	case 'a':	i = configA[to-'a']; break;
	case 'b':	i = configB[to-'a']; break;
	case 'c':	i = configC[to-'a']; break;
	case 'd':	i = configD[to-'a']; break;
	case 'e':	i = configE[to-'a']; break;
	case 'f':	i = configF[to-'a']; break;
	case 'i':	i = configI[to-'a']; break;
	case 'j':	i = configJ[to-'a']; break;
	case 'm':	i = configM[to-'a']; break;
	case 'o':	i = configO[to-'a']; break;
	case 'q':	i = configQ[to-'a']; break;
	case 's':	i = configS[to-'a']; break;
	case 'v':	i = configV[to-'a']; break;
	case 'y':	i = configY[to-'a']; break;
	case 'z':	i = configZ[to-'a']; break;
	default:	i = 0; break;
	}
	return(i);
	}
/*
	harg(string,pargc,pargv)

	A curious procedure which takes a pointer to an argc, and a 
	pointer to an argv, and parses them so that the
	argument following the flag is copied into string.
	pargv[0] must be the flag argument.
	handles both
		-my
	and
		-m y
	for the net command.
*/
harg(ans,pargc,pargv)
  char *ans,*pargc,***pargv;{
	if((*pargv)[0][2])		/* no space */
		strcpy(ans,(*pargv)[0] + 2);
	else {				/* space, get next arg */
		strcpy(ans,(*pargv)[1]);
		(*pargc)--;
		(*pargv)++;
		}
	}

/* prints out commands before executing them */
/*VARARGS0*/
mexecl(s)
  char *s;{
	int *p = (int *)&s;
	register int i;
	if(debugflg){
		for(i=0; p[i]; i++)err("%s ",p[i]);
		putc('\n',stderr);
		}
	execl(p[0],p[1],p[2],p[3],p[4],p[5],p[6],p[7],p[8],p[9],p[10],p[11],
	p[12],p[13],p[14],p[15],0);
	perror(p[0]);
	}
/* prints out commands before executing them */
mexecv(s,p)
  register char *s, **p;{
	register int i;
	if(debugflg){
		err("%s ",s);
		for(i=0; p[i]; i++)err("%s ",p[i]);
		putc('\n',stderr);
		}
	execv(s,p);
	perror(s);
	}

/*VARARGS0*/
/* fills in -l - -p from commands like rcp */
/* must be called with at least two arguments */
kexecl(s)
  char *s;	{
	char *a[20], i = 2, j = 2;
	char **p = (char **)&s;
	a[0] = p[0];
	a[1] = p[1];
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
	while(p[j])a[i++] = p[j++];
	a[i] = 0;
	mexecl(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11],
	a[12],a[13],a[14],a[15],0);
	}

/*
	MchSFromAddr(sn,addr)

	take an address of the form "mach:username"
	and return mch as the 1 char code of "mach" and
	in sn put "username".
	If addr has no colon in it, return mch==local, sn==addr.
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
/*VARARGS0*/
debug(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t)
char *s; {
	if(debugflg){
		printf(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t);
		putchar('\n');
		}
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
/*
	SnCurrent()

	Return the name of the current user.
	If not in the /etc/utmp file, will read the passwd file.
	Returns NULL if error.
	Note that detached processes on V7 have getlogin() return
	a pointer to a null string, not NULL.
*/
char *SnCurrent()
{
	static char snBuf[NS];
	register char *sn;

	sn = getlogin();
	if(sn == NULL || sn[0] == 0 || sn[0] == ' ')
		sn = SnFromUid(getuid());
	if(sn != NULL){
		strcpy(snBuf,sn);
		sn = snBuf;
	}
	return(sn);
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

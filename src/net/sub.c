/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
# include "config.h"

char netcmd[] =		NETCMD;
char resfile[]= 	RESFILE;
char senddir[] =	SENDDIR;
char logfile[]= 	LOGFILE;
char Bsh[]= 		BINSH;
char mailcmd[]= 	MMAILCMD;
char writecmd[]=  	MWRITECMD;


int debugflg = DBV;		/* debug flag */
int datasize = SIZE;		/* best if mult of 512 */
char tokval[BFS];
FILE *cfile;
char vaxtovax = 0;
/*
	speeds baud setting
	300	7
	1200	9
	9600	13
	*/
int linkspeed = LINKS;
char local = LOCAL;


struct tokstruct toktab[]= {
	"machine",	MACHINE,
	"login",	LOGIN,
	"password",	PASSWORD,
	"notify",	NOTIFY,
	"command",	COMMAND,
	"yes",		YES,
	"y",		YES,
	"no",		NO,
	"n",		NO,
	"default",	DEFAULT,
	"write",	WRITE,
	"force",	FORCE,
	"local",	LOCALTOK,
	"speed",	SPEED,
	"link",		LINK,
	"vaxtovax",	VAXTOVAX,
	"length",	LENGTH,
	"debug",	DEBUGTOK,
	"time",		ALTIME,
	"count",	ALCOUNT,
	0,		0
	};
passwdent(){
	register char *u;
	register struct passwd *pwd;
	pwd = getpwuid(getuid());
	if(pwd == NULL){
		err("Bad uid\n");
		return;
		}
	strcpy(status.localname,pwd->pw_name);
	status.muid = guid(pwd->pw_uid,pwd->pw_gid);
	status.mgid = pwd->pw_gid;
	if(isdigit(pwd->pw_gecos[0]))status.jobno = atoi(pwd->pw_gecos);
	else status.jobno = 32767;
	strcpy(status.dir,pwd->pw_dir);
	u = pwd->pw_shell;
	if(u[0] == 0 || strcmp("sh",u+strlen(u)-2) != 0)u= Bsh;
	strcpy(status.loginshell,u);
	}
promptlogin(){
	register char *p;
	char buf[BFS];
	FILE *wf;
	int c;
	struct sgttyb stt;
	int oflag;
	if(status.mpasswd[0] == 0 || status.login[0] == 0 || status.force){
		wf = fopen("/dev/tty","r");
		if(wf != NULL){
			if(status.login[0]==0 || status.force){
				printf("Name (%s): ",status.localname);
				if(fgets(buf, BFS, wf) != buf){
					perror("fgets");
					exit(1);
					}
				c = strlen(buf);
				buf[c > 0 ? c-1 : 0] = 0;
				if(c > 10){
					err("Login name too long.\n");
					exit(1);
					}
				if(member(buf,' ')){
					err("Login names don't have blanks in them.\n");
					exit(1);
					}
				if(buf[0] == 0)strcpy(buf,status.localname);
				strcpy(status.login,buf);
				}
			if(strcmp(status.login,"network") != 0
				&& (status.mpasswd[0]== 0 || status.force)){
				sprintf(buf,"Password (%s):",status.login);
				strcpy(status.mpasswd,getpass(buf));
				}
			fclose(wf);
			}
		}
	if(status.login[0] == 0) strcpy(status.login,status.localname);
	if(status.mpasswd[0] == 0)strcpy(status.mpasswd,"\"\"");
	status.force = 0;
	}
	
/*
	called in netdaemon and debugging software,
	handles parameter lists to setup
	remote machine and pipes
*/
setupdaemon(argc,argv)
  char **argv; {
	remote = argc > 1 ? lookup(argv[1]) : getremote(local);
	if(argc == 4){	/* simulate using pipes */
		readfd = atoi(argv[2]);
		writefd = atoi(argv[3]);
		pipesim++;
		}
	initdaemon();
	}
setup(str)
  char *str; {
	struct sgttyb stt;
	static char readbuf[BUFSIZ],writebuf[BUFSIZ];
	if(str == 0 || str[0] == 0){
		err("invalid net device\n");
		exit(1);
		}
	masterseqno = 1;
	readtty = pipesim ? fdopen(readfd,"r") : fopen(str,"r");
	if(readtty == NULL){
		perror(str);
		exit(1);
		}
	writetty = pipesim ? fdopen(writefd,"w") : fopen(str,"w");
	if(writetty == NULL){
		perror(str);
		exit(1);
		}
	if(!pipesim){
		/* set exclusive use for line */
		if(ioctl(fileno(readtty),TIOCEXCL,&stt) != 0 ||
			gtty(fileno(readtty),&stt) < 0){
			perror(str);
			exit(1);
			}
		stt.sg_ispeed = stt.sg_ospeed = linkspeed;  /* user-set baud */
		stt.sg_erase = stt.sg_kill = 0;		/* erase and kill off */
		stt.sg_flags = ANYP;	/* even and odd parity, off everything else */
		if(stty(fileno(readtty),&stt) < 0){
			perror(str);
			exit(1);
			}
		}
	setbuf(readtty,readbuf);
	setbuf(writetty,writebuf);
	}
/* passwords work as follows:
   passwd = "\n" means no password
   */
/* table of netrc options
	option			default
	------			-------
	default			default machine
	login string		current login
	password string		-
	notify yes/no		yes
	write yes/no		yes
	command string		-
	force yes/no		no
*/

/*
	Fabry has suggested that machine names be more general:
	that you be able to say:
	cory:	fabry on Cory
	caf:	caf on Cory
	c:	fabry on C

	so the formulation would look like:

	default key
	key: machine login passwd ...
	key: ....

	and so on

	Gould has suggested the format be:

	pseudo cory 	real Cory 	login fabry
	pseudo caf 	real Cory 	login caf
	pseudo c 	real C 		login fabry
*/

/* init file format local C remote A
	default A
	machine A    local C link /dev/net-A    speed 9
	machine Cory local C link /dev/net-Cory speed 9
	
	if remote == 0, default is A
	also options:
		vaxtovax, length, debug
	*/
initdaemon(){
	long timev;
	int timei;
	cfile = fopen(INITFILE,"r");
	getfile();
	err("remote %c local %c link %s speed %d vtov %d length %d\n",
	remote,local,device,linkspeed,vaxtovax,datasize);
	err("debug %d time %d count %d\n",debugflg,atime,maxbread);
	setup(device);
	timev = gettime();
	timei = timev >> 16;
	srand(timei);
# ifdef VAX
	if(machtype[local - 'a'] != M_VAX)
# endif
# ifdef CORY
	if(machtype[local - 'a'] != M_CORY)
# endif
# ifdef CC
	if(machtype[local -'a'] != M_CC && machtype[local - 'a'] != M_SRC)
# endif
		err("Machine type disagrees with local machine\n");
	}
commandfile(){
	char *hdir, buf[BFS*2];
	hdir = getenv("HOME");
	if(hdir == 0)hdir = ".";
	if(strcmp(hdir,"/") == 0)return;
	sprintf(buf,"%s/.netrc",hdir);
/*
	debug("file %s",buf);
*/
	cfile = fopen(buf,"r");
	getfile();
	}
getfile(){
	int t;
	if(cfile == NULL)return;
	if(fstat(fileno(cfile),&statbuf) < 0 || (statbuf.st_mode & 0444) == 0)
		return;
	while((t = token())){
		switch(t){
		case DEFAULT:
			if(token() == ID && remote == 0)remote = lookup(tokval);
			/*
			debug("rem %c\n",remote);
			*/
			break;
		case MACHINE:
			if(remote == 0)remote = getremote(local);
			if(token() != ID)continue;
			if(remote != lookup(tokval))continue;
			/* this is the entry for the remote mach we want */
			getnetline();
			goto out;
			break;
		}
		}
out:
	fclose(cfile);
	return;
	}
getnetline(){
	int t;
	while((t = token())){
		switch(t){
		case MACHINE: return;
		case LOGIN:
			if(token() && status.login[0] == 0)
				strcpy(status.login,tokval);
			break;
		case PASSWORD:
			if(fstat(fileno(cfile),&statbuf) >= 0
			&& (statbuf.st_mode & 077) != 0){
				err("Error - .netrc file not correct mode.\n");
				err("Remove password or correct mode.\n");
				exit(1);
				}
			if(token() && status.mpasswd[0] == 0)
				strcpy(status.mpasswd,tokval);
			/*
			debug("mp:%s:%s\n",status.mpasswd,tokval);
			*/
			break;
		case NOTIFY:
			status.nonotify = token() == NO;
			break;
		case WRITE:
			status.nowrite = token() == NO;
			break;
		case COMMAND:
			if(status.defcmd[0] == 0 && token())
				strcpy(status.defcmd,tokval);
			break;
		case FORCE:
			status.force = token() == YES;
			break;
		case LOCALTOK:
			if(token())local = lookup(tokval);
			break;
		case LINK:
			if(token())strcpy(device,tokval);
			break;
		case SPEED:
			if(token())linkspeed = atoi(tokval);
			break;
		case VAXTOVAX:
			vaxtovax++;
			break;
		case LENGTH:
			if(token())datasize = atoi(tokval);
			break;
		case DEBUGTOK:
			debugflg++;
			break;
		case ALTIME:
			if(token())atime = atoi(tokval);
			break;
		case ALCOUNT:
			if(token())maxbread = atoi(tokval);
			break;
		default:
			err("Unknown .netrc option %s\n",tokval);	
			break;
		}
		}
	}
token(){	/* returns next token in cfile, 0 on EOF */
	char *p;
	int c;
	if(feof(cfile))return(0);
	while((c = getc(cfile)) != EOF && (c == '\n' || c == '\t'
		|| c == ' ' || c == ','));
	/* next char begins token */
	if(c == EOF)return(0);
	p = tokval;
	if(c == '"'){	/* process quoted string */
		while((c = getc(cfile)) != EOF && c != '"'){
			if(c == '\\')c = getc(cfile);
			*p++ = c;
			}
		}
	else {
		*p++ = c;
		while((c = getc(cfile)) != EOF && c != '\n' && c != '\t' 
			&& c != ' ' && c != ','){
			if(c == '\\')c = getc(cfile);
			*p++ = c;
			}
		}
	*p = 0;
	if(tokval[0] == 0)return(0);
/*
	debug("tok %s",tokval);
*/
	return(tlookup(tokval));
	}
tlookup(str)
  char *str; {
	struct tokstruct *p;
	for(p = toktab; p->tokstr; p++)
		if(streql(p->tokstr,str) == 0){
			return(p->tval);
			}
	return(ID);
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
/* determine through machine */
gothru(from,to){
	register int i;
	switch(from){
	case 'a':	i = configA[to-'a']; break;
	case 'b':	i = configB[to-'a']; break;
	case 'c':	i = configC[to-'a']; break;
	case 'd':	i = configD[to-'a']; break;
	case 'e':	i = configE[to-'a']; break;
	case 'i':	i = configI[to-'a']; break;
	case 'q':	i = configQ[to-'a']; break;
	case 's':	i = configS[to-'a']; break;
	case 'v':	i = configV[to-'a']; break;
	case 'y':	i = configY[to-'a']; break;
	default:	i = 0; break;
	}
	return(i);
	}
/* note the pointers to returned values */
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

static struct stat x;
static struct direct y;
static FILE *file;
static int off = -1;


/* these three routines gwd, cat, ckroot and 
   data structures x, y, off, do a pwd to string name */
gwd(name)
  register char *name; {
	*name = 0;
	for(;;){
		stat(".",&x);
		if((file = fopen("..","r")) == NULL)break;
		do {
			if(fread(&y,1,sizeof y,file) != sizeof y)break;
			} while(y.d_ino != x.st_ino);
		fclose(file);
		if(y.d_ino == ROOTINO){
			ckroot(name);
			break;
			}
		if(cat(name))break;
		chdir("..");
		}
	chdir(name);
	}

cat(name)
  register char *name; {		/* return 1 to exit */
	register int i,j;
	i = -1;
	while(y.d_name[++i] != 0);
	if((off+i+2) > 511)return(1);
	for(j = off +1; j >= 0; --j)name[j+i+1] = name[j];
	off = i + off + 1;
	name[i] = '/';
	for(--i; i>= 0; --i)name[i] = y.d_name[i];
	return(0);
	}

ckroot(name)
  char *name; {
	register int i;
	if(stat(y.d_name,&x) < 0)return;
	i = x.st_dev;
	if(chdir("/") < 0)return;
	if((file = fopen("/","r")) == NULL)return;
	do {
		if(fread(&y,1,sizeof y,file) != sizeof y)return;
		if(y.d_ino == 0)continue;
		if(stat(y.d_name,&x) < 0)return;
		} while(x.st_dev!=i || (x.st_mode&S_IFMT)!=S_IFDIR);
	if(strcmp(y.d_name,".") != 0 && strcmp(y.d_name,"..") != 0)
		if(cat(name))return;
	i = strlen(name);
	name[i+1] = 0;
	while(--i >= 0)name[i + 1] = name[i];
	name[0] = '/';
	return;
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
	perror("execv");
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
	if(status.nonotify)a[i++] = "-n";
	if(status.force)a[i++] = "-f";
	while(p[j])a[i++] = p[j++];
	a[i] = 0;
	mexecl(a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9],a[10],a[11],
	a[12],a[13],a[14],a[15],0);
	}

analyze(S,f)
  char *S;
  register struct fd *f; {
	register char *file;
	char work[FNS], *s, c0,c1,c2,c3;
	s = work;
	strcpy(s,S);
	f->mach = local;
	file = s;
	if(member(s,':')){	/* name specified */
		while(*s && *s != ':')s++;
		*s = 0;
		f->mach = lookup(file);
		if(f->mach == 0){
			err("Unknown machine %s",file);
			exit(1);
			}
		while(*++s && *s == ':');
		file = s;
		}
	else {
		c0 = *s++;
		c1 = *s++;
		c2 = *s++;
		c3 = *s++;
		if(c0 == '/' && c1 != '/' && islower(c1))
			if(c2 == '/')f->mach = 'y';		/* Cory name */
			else if(c3 == '/')f->mach = c1;		/* CC name */
		}
	f->fn = calloc(strlen(file)+1,1);
	strcpy(f->fn,file);
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
error(s,a,b,c,d,e,f,g,h)
char *s; {
	char buf[10];
	if(remote != 0) sprintf(buf,"%s",longname(remote));
	else buf[0] = 0;
	fflush(stdout);
	if(debugflg){
		fprintf(stderr,s,a,b,c,d,e,f,g,h);
		putc('\n',stderr);
		}
	addtolog(remote,"Err %s: ",buf);
	addtolog(remote,s,a,b,c,d,e,f,g,h);
	addtolog(remote,"\n");
	}
/*VARARGS0*/
debug(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t)
char *s; {
	if(debugflg){
		printf(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t);
		putchar('\n');
		}
	}

member(s,c)
register char *s, c; {
	while(*s)if(*s++ == c)return(1);
	return(0);
	}
/* this is really not right - we should use the rcslog format */
/* also, the user must be able to write on the
   public logfile to get error messages such as
   directory not found after he has
   setuid'd from root
*/
/*VARARGS0*/
addtolog(mach,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
char *s;
{
	static FILE *log = NULL;
	logfile[strlen(logfile)-1] = mach;
	if(log == NULL){
		if(stat(logfile,&statbuf) < 0)return;
		log = fopen(logfile,"a");
		}
	if(log == NULL)return;
	fseek(log,0L,2);
	fprintf(log,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n);
	fflush(log);
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
/* VARARGS0 */
static err(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) {
	fprintf(stderr,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r);
	}

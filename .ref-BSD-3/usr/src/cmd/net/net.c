# include "defs.h"
/* must be setuid root */
/*
	net - -b -c cmd -f -i file -l name -mmach -n -o file -p passwd
		-r file -s file -u uid -w -x -y -z command
		
	-	take from standard input
	-b	never send anything back
	-c cmd	think of this as a "cmd" *
	-f	force prompting of user name and password
	-i file	remote stdin *
	-l name remote login name
	-m Mach	remote machine
	-n	do not write back anything, always mail them back
	-o file	remote stdout & stderr *
	-p pass remote password
	-q 	quiet option, send back only if rcode !=0 or if there is stdout
	-r file	local response file
	-s file	local stdin file *
	
	(super users only, always skip login/passwd check:)
	-u uid	net queue files should be owned by uid (16 bits)
	-w	this is a write/mail response cmd *
	-x	this is being forwarded through us to another machine *
	-y	skip login/password check *
	-z	this is a response file being returned *

	* = not documented in net(NEW)
	
*/
/*
	code	option	reason
	q		normal request
	w	-w	message to be written back
	 	-x	being forwarded through us
	y	-y	simply skips login check (used by netlpr)
	s	-z	normal response
*/
static char dfname[]=		DFNAME;

main(argc, argv)
  char **argv; {
	register int i;
	int outerror(),uid;
	char *genparmlist();
	char resp[FNS], infile[FNS], outfile[FNS], localin[FNS];
	char buf[BUFSIZ], suid[10];
	char sin =0, code, zopt = 0, wopt = 0, yopt = 0, xopt = 0;
	char *s,*sn;
	char sTtyname[20], sCmdAct[BUFSIZ], sCmdVirt[BUFSIZ];
	long cnt = 0L, maxfile = MAXFILE, lTtytime;
	char cflag = 'a';
	FILE *file, *temp, *rfile;
	struct utmp utmpstr;
	struct stat statbuf;

	debugflg = DBV;
	argv[argc] = 0;
	sCmdAct[0] = resp[0] = outfile[0] = infile[0] = 0;
	sCmdVirt[0] = localin[0] = 0;
	sTtyname[0] = 0;
	suid[0] = 0;

	if(isatty(0)) strcat(sTtyname,ttyname(0));
	else if(isatty(2)) strcat(sTtyname,ttyname(2));
	remote = 0;
	signal(SIGHUP,outerror);
	signal(SIGQUIT,outerror);
	signal(SIGINT,outerror);
	signal(SIGTRM,outerror);

	while(argc > 1 && argv[1][0] == '-'){
		argc--; argv++;
		switch(argv[0][1]){
		case 0:   sin++; break;
		case 'b': status.nonotify++; break;
		case 'c': harg(sCmdVirt,&argc,&argv); break;
		case 'f': status.force++; break;
		case 'i': harg(infile,&argc,&argv); break;
		case 'l': harg(status.login,&argc,&argv); break;
		case 'm': harg(buf,&argc,&argv); remote = lookup(buf); break;
		case 'n': status.nowrite++; break;
		case 'o': harg(outfile,&argc,&argv); break;
		case 'p':
			  harg(status.mpasswd,&argc,&argv);
			  if(status.mpasswd[0] == 0)
			  	strcpy(status.mpasswd,"\n\n");
			  break;
		case 'q': status.quiet++; break;
		case 'r': harg(buf,&argc,&argv); addir(resp,buf); break;
		case 's': harg(localin,&argc,&argv); break;
		case 'u': harg(suid,&argc,&argv); break;
		case 'w': wopt++; break;
		case 'x': xopt++; break;
		case 'y': yopt++; break;
		case 'z': zopt++; break;
		default:
			fprintf(stderr,"Unknown option %s\n",argv[0]);
			break;
		}
		}
	while(argc > 1){
		argc--; argv++;
		strcat(sCmdAct,argv[0]);
		strcat(sCmdAct," ");
		}
	uid = getuid();
	code = 'q';
	if(zopt || wopt || yopt || xopt || suid[0] != 0){
		/* check z or w or y or x option permission */
# ifndef TESTING
		if(uid != SUPERUSER){
			fprintf(stderr,"Error: Not super-user");
			outerror();
			}
# endif
		code = zopt ? 's' : 'w';
		code = yopt ? 'y' : code;
		if(status.mpasswd[0] == 0)	/* no passwd required */
			strcpy(status.mpasswd,"\n");
		}

	status.jobno = 32767;		/* default (invalid) job number */
	if(code == 'q' && !xopt){
		if((sn = SnCurrent()) == NULL
		/* || machtype[local-'a'] == M_CC */)
			/* turns out we never use jobno, except in netlpr */
			/* read passwd file, get status.localname & jobno */
			passwdent();
		else
			/* don't bother reading passwd file, don't need jobno */
			strcpy(status.localname,sn);
	}

	/* sets remote,status.login,status.force,status.mpasswd,
		status.nonotify, status.nowrite */
	/* may read passwd file if getenv(HOME) reads it */
	commandfile();

	if(remote == 0)remote = getremote(local);
# ifndef TESTING
	if(remote == local){
		fprintf(stderr,"Request sent to local machine - doesn't make sense\n");
		/* outerror(); */
		}
# endif
	strcat(status.defcmd," ");
	if(strlen(sCmdAct) == 0)strcpy(sCmdAct,status.defcmd);
	sCmdAct[strlen(sCmdAct)-1] = 0;
	mktemp(dfname);
	/* determine through machine */
	i = gothru(local,remote);
	if(i == 0){
		s = longname(remote);
		if(s != 0)fprintf(stderr,"No path to %s machine.\n",s);
		else fprintf(stderr,"Unknown machine\n");
		outerror();
		}
	dfname[strlen(dfname)-11] = i;		/* set directory */
	dfname[strlen(dfname)-7] = i;		/* set file (unused) */
	/* check to see if data files are directories */
	if(isdirectory(resp) || isdirectory(infile) || isdirectory(outfile)){
		fprintf(stderr,"%s is a directory, must be a file\n",
			isdirectory(resp)    ? resp :
			isdirectory(infile)  ? infile :
			outfile);
		outerror();
	}
	if(suid[0] != 0)uid = atoi(suid);
	if(resp[0]){
		if(strcmp(resp,"/dev/tty") == 0){
		fprintf(stderr,"Can't have /dev/tty as response file.\n");
			outerror();
			}
		if(stat(resp,&statbuf) == -1){
			strcpy(buf,resp);
			s = &buf[0];
			s = s + strlen(buf) - 1;
			while(*s != '/' && s > &(buf[0]))s--;
			*s = 0;
			debug("chkdir %s",buf);
			if(strlen(buf) == 0)strcpy(buf,".");
			if(access(buf,2) == -1){
				perror(buf);
				outerror();
				}
			if((rfile=fopen(resp,"w")) == NULL){
				perror(resp);
				outerror();
				}
			chmod(resp,0600);
			fclose(rfile);
			mchown(resp,uid,getgid());
			}
		else if(access(resp,2) == -1){
			perror(resp);
			outerror();
			}
		}
	/* go ahead and prompt for login name and passwd, if neccessary,
	   as long as the X option has not been specified */
	if(code == 'q' && !xopt)promptlogin(remote);

	/* at this point, we create the dfa... file */
	file = fopen(dfname,"w");
	if(file == NULL){
		perror(dfname);
		outerror();
		}
	chmod(dfname,0600);
	mchown(dfname,uid,getgid());
	if(xopt)goto stickit;
	if(status.mpasswd[0] == '\n')
		status.mpasswd[0] = 0;
# ifndef NEWPROT
	if(machtype[local-'a'] == M_CC && machtype[remote-'a'] == M_CC
		&& status.mpasswd[0] != 0){
		s = crypt(status.mpasswd);
		strcpy(status.mpasswd,s);
		}
# endif
	if(status.mpasswd[0] == 0 && code == 'q' &&
		strcmp(status.login,"network") != 0){
		fprintf(stderr,"Zero-length password not allowed\n");
		outerror();
		}
	if(code == 'q' && (streql(status.login,"root") == 0 ||
		streql(status.login,"ruut") == 0)){
		fprintf(stderr,"Can't login as root through the network\n");
		outerror();
		}
# ifdef SPACCT
	/* handle special accounts */
	/* give a value for mgid and muid */
	strcpy(status.mpasswd,handlesp(status.login,status.mpasswd,
		status.localname,status.muid,status.mgid));
# endif
	enmask(status.mpasswd);
	lTtytime = 0;
	if(sTtyname[0] && status.nowrite == 0){
		temp = fopen("/etc/utmp","r");
		if(temp == NULL){
			perror("/etc/utmp");
			outerror();
			}
		while(fread(&utmpstr,1,sizeof utmpstr,temp) == sizeof utmpstr)
# ifdef OLDTTY
			if(utmpstr.ut_tty == sTtyname[8]){
# else
			if(strcmp(utmpstr.ut_line,sTtyname+5) == 0){
# endif
				lTtytime = utmpstr.ut_time;
				break;
				}
		}
/*
	debug("p:%s:\n",status.mpasswd);
*/
	/* cflag is initially 'a'. Add the flags as needed. */
	if(status.nonotify)cflag += F_NONOTIFY;
	if(status.quiet)cflag += F_QUIET;
/*
	protocol:
	code, remote mach, local mach, version stamp (2), remote login name,
	password, -i, -o, -r files,
	local login name, terminal, flag, utmp tty login time,
	cc jobno(variable parameter list), current time,
	command '\n' real command '\n'
	any data
	
	changes:
	1) remove header
	3) use ascii length instead of 4 bytes
	4) encrypt the login name, command, and part of data as well
*/

	fprintf(file,
	"%c :%c :%c :%c :%c :%s :%s :%s :%s :%s :%s :%s :%c :%lo :%d%s :%ld :",
		code,remote,local,VMAJOR+'a',VMINOR+'a',status.login,
		status.mpasswd,infile,outfile,resp,
		status.localname,sTtyname,cflag,lTtytime,
		status.jobno,genparmlist(),gettime()-TIMEBASE);
	fputs(sCmdAct,file);
	putc('\n',file);
	fputs(sCmdVirt,file);
	putc('\n',file);
stickit:
	/* between ingres machines, allow long files */
	/* this should be parametrized on a per machine pair basis */
	if(machtype[local  - 'a'] == M_INGRES &&
	   machtype[remote - 'a'] == M_INGRES)
		maxfile = MAXFILELARGE;
	if(sin)
		while((i = fread(buf,1,BUFSIZ,stdin)) > 0){
			if(fwrite(buf,1,i,file) != i){
				perror("net queue file");
				outerror();
				}
			if((cnt += i) > maxfile)goto toobig;
			if(feof(stdin))break;
			}
	else if(localin[0]){
		if(access(localin,4) == -1){
			perror(localin);
			outerror();
			}
		temp = fopen(localin,"r");
		if(temp == NULL){
			perror(localin);
			outerror();
			}
		while((i = fread(buf,1,BUFSIZ,temp)) > 0){
			if((cnt += i) > maxfile)goto toobig;
			if(fwrite(buf,1,i,file) != i){
				perror("net queue file");
				outerror();
				}
			}
		fclose(temp);
		}
	fclose(file);
	chmod(dfname,0400);
	dfname[strlen(dfname)-9] = 'c';
	file = fopen(dfname,"w");
	chmod(dfname,0400);
	fclose(file);
	mchown(dfname,uid,getgid());
	exit(0);
toobig:
	fprintf(stderr,"No more than %ld bytes can be sent\n",maxfile);
	outerror();		/* no return */
	}
/* 
   called if there is an error, makes sure that the files created
   are deleted and the terminal is reset to echo
*/
outerror(){
	register int i;
	struct sgttyb stt;
	signal(SIGHUP,SIG_IGN); signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,SIG_IGN); signal(SIGTRM,SIG_IGN);
	unlink(dfname);
	i = strlen(dfname) - 9;
	dfname[i] = (dfname[i] == 'c' ? 'd' : 'c');
	unlink(dfname);
	if(gtty(0,&stt) >= 0){
		stt.sg_flags |= ECHO;
		stty(0,&stt);
		}
	exit(1);
	}
enmask(s)
  register char *s; {
# ifdef NEWPROT
	static char buf[20];
	strcpy(s,nbsencrypt(s,THEKEY,buf));
# else
	while(*s){
		*s &= 0177;		/* strip quote bites */
		*s++ ^= 040;		/* invert upper-lower */
		}
# endif
	}
addir(s,t)
  register char *s, *t; {
	if(t[0] == '/')strcpy(s,t);
	else {
		gwd(s);
		strcat(s,t);
		}
	}
/* returns pass if not special, otherwise returns funny passwd */
/* list of special accounts must be consistent - with netdaemon.c */
char *handlesp(log,pass,localname,luid,lgid)
char *log,*pass,*localname;{
	/* experimental */
# ifdef SPACCT
	long lt;
	char str[20];
	if(strcmp(log,localname) == 0 && luid != 0 && lgid == 0 && (
	strcmp(log,"source") == 0
	|| strcmp(log,"daemon") == 0
	)) {
		lt = lgid;
		lt = (lt << 16) | luid;
		sprintf(str,"%ld",lt);
		return(str);
		}
# endif
	return(pass);
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
/*
	this function takes a file name and tells whether it is a 
	directory or on. Returns 1 if so, 0 otherwise.
	null strings etc. return 0.
*/
isdirectory(fn)
	char *fn;
{
	int i,ret=0;
	if(fn == NULL || *fn == 0)return(0);
	i = strlen(fn);
	if(i == 1){
		if(strcmp(fn,".")       == 0)ret = 1;
		if(strcmp(fn,"/")       == 0)ret = 1;
	}
	else if(i == 2){
		if(strcmp(fn,"..")      == 0)ret = 1;
		if(strcmp(fn,"/.")      == 0)ret = 1;
	}
	else {
		if(strcmp(fn+i-2,"/.")  == 0)ret = 1;
		if(strcmp(fn+i-3,"/..") == 0)ret = 1;
	}
	return(ret);
}
/*
	generate a variable parameter list
	the format is:
		(name value, name value, ..., name value)
	where names are unquoted single words and values
	are unquoted if a single alphanumeric word, and are
	surrounded by {} otherwise. \ quotes { and }.
	the values are escape-processed, e.g. \n becomes 012.
	this function returns such a list.
	Returns the null parm list if nothing to give, i.e. "()" 

	Should also default so single keywords can have on/off
	states, and so do not require a value.

	Things this variable protocol should specify:
		EPASSWD 	encrypted passwd
		FILEMODE 	file mode
		FROMUID  	from users' uid
		FROMGID  	from users' gid
		COMPRESS 	use colin's compression
		SPACCT	 	handle special accounts.
		MESSAGEID	unique number identifying this request.
		VTOUSERNAME	name netq should display as being "To:"
		FILENAME	when omitted by netcp, will use FILENAME ext.
		MACHINE2	a second machine (e.g. 3way netcp)
		LOGIN2		a second login name
		PASSWD2		a second passwd
		REPLYTO		the person the response should be sent to

*/
char *genparmlist(){
	static char returnstr[PARMLIST];
	strcpy(returnstr,"()");
	return(returnstr);
}

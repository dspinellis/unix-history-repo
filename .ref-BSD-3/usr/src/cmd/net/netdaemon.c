/*
	The daemon program that runs the network.

	netdaemon mach readfd writefd

	Must be started by root.
*/

# include "defs.h"

/* global variables */
extern char **environ;
short masterseqno, lastseqno;
struct dumpstruc dump;
struct bstruct btable[];
int onlyuid;

/* local variables */
static long length;
static char nsendfail;
static FILE *dir;
static char header[] = 		"ABCDE";
static char tempfile[]= 	TEMPFILE;
static char publogfile[]=  	PUBLOGFILE;
static char dumpfile[]=  	DUMPFILE;
static char namefile[]=  	NAMEFILE;
static struct stat statbuf;
static struct direct dirbuf;
int handlekill();
static char frommach;
static char buf[BUFSIZ];
/* addrfrom is the person who sent this to us,
   addrto is the person who received the command, i.e.
   addrto is on this machine */
static char addrto[BUFSIZ], addrfrom[BUFSIZ];

main(argc,argv)
  char **argv; {
	register int i;
	long ltime,t;

	signal(SIGTRM,handlekill);
	debugflg = DBV;
	setupdaemon(argc,argv);
	/* now running alone as a daemon */
		/*
		for(i=0; i<15; i++)close(i);
		signal(SIGHUP,SIG_IGN);
		signal(SIGQUIT,SIG_IGN);
		signal(SIGINT,SIG_IGN);
		*/
	senddir[strlen(senddir)-1] = remote;		/* choose dir */
	if(chdir(senddir) < 0){
		perror(senddir);
		exit(1);
		}
	dir = fopen(senddir,"r");
	if(dir == NULL){
		perror(senddir);
		exit(1);
		}
	mktemp(tempfile);
	tempfile[strlen(tempfile) - 7] = remote;
	ltime = gettime();
	sprintf(buf,"net restarted to %s %d %s",longname(remote),
		getpid(),ctime(&ltime));
	dump.longtime = dump.shorttime = ltime;
	addtolog(remote,buf);
	addtodump(remote,buf);
	addtopublic(buf);
	fprintf(stderr,buf);
	if(!debugflg)fclose(stderr);
	sendpurge();
	nsendfail = 0;
	for(;;){	/* begin reading file */
		debug("daemon %c %d\nreceive",remote,getpid());
		i = getreset();
		dump.waittime = 0L;
		if(i == BROKENREAD){
			if(nsendfail < NSEND) netsend();
			else nsendfail++;
			}
		else {
			i = netrcv();
			if(i >= 0){
				nsendfail = 0;	/* it sent, it is up */
				dump.waittot += dump.waittime;
				}
			if(i == -1)dump.nabnormal++;
			}
		t = gettime();
		if(t - dump.shorttime > SAMPL)pload(t);
		if(t - dump.longtime > BIGSAMPL)dumpit(t);
		dump.nloop++;
		/* count up to NSEND, sending, then wait NSEND */
		if(nsendfail >= NSEND*2)nsendfail = 0;	
		}
	}
netsend(){
	static long lasttime = 0;
	static char nleft = 1;
	long lFileLen,diff;
	double drate;
	int uid,uidBest;
	char *sdate,*sn,*swait;
	long ot,nt,filesize;
	register int i;
	char stemp[20];
	static char jname[FNS];

	debug("ck send");
	if(stat(senddir,&statbuf) < 0){
		error("%s %s",senddir,sys_errlist[errno]);
		return;
		}
	if(statbuf.st_mtime == lasttime && nleft == 0)return;	/* no need to search */
	lasttime = statbuf.st_mtime;
	fseek(dir,0L,0);
	lFileLen = 10000000L;
	nleft = 0;
	while(fread(&dirbuf,1,sizeof dirbuf,dir) == sizeof dirbuf){
		if(dirbuf.d_ino == 0
		   || dirbuf.d_name[0] != 'c'
		   || dirbuf.d_name[1] != 'f'
		   || dirbuf.d_name[2] != remote
		   || stat(dirbuf.d_name,&statbuf) < 0
		   || statbuf.st_mode == 0)
			continue;
		dirbuf.d_name[0] = 'd';
		if(stat(dirbuf.d_name,&statbuf) < 0 || statbuf.st_mode == 0)
			continue;
		uid = guid(statbuf.st_uid,statbuf.st_gid);
		if(onlyuid != 0 && uid != onlyuid && uid != SUPERUSER)continue;
		nleft++;
		filesize = getsize(&statbuf);
		if(lFileLen > filesize){
			lFileLen = filesize;
			for(i=0; i<DIRSIZ; i++)
				jname[i] = dirbuf.d_name[i];
			uidBest = uid;
		}
	}
	if(lFileLen == 10000000L)return;
	strcpy(stemp,jname);
	stemp[0] = 'c';
	sn = SnFromUid(uidBest);
	if(sn == NULL){
		addtolog(remote,"Unknown userid %d\n",uidBest);
		return;
	}
	addtolog(remote,"^S %s %c: %s ",sn,remote,jname+2);
	ot = gettime();
	dump.waittime = 0L;
	if(send(jname) == 0)return;
	nt = gettime();
	dump.waittot += dump.waittime;
	filesize = getsize(&statbuf);
	unlink(jname);
	unlink(stemp);
	diff = nt - ot;
	if(diff < 1)diff = 1;		/* avoid dividing by zero */
	sdate = ctime(&nt)+4;
	sdate[strlen(sdate) -9] = 0;
	drate = (double)filesize / (double)diff;
	swait = comptime(ot - statbuf.st_mtime);
	jname[3] = jname[2];
	addtolog(remote,"^T%c(%s, %ldb, %ldsec, %4.1fb/sec, w %s)\n",
		remote,sdate,filesize, diff,drate, swait);
	addtopublic("%s: sent %-8s to %c (%s, %ld b, wait %s)\n",
		sdate,sn,remote,jname+3,filesize,swait);
	dump.nsend++;
	dump.bytetot += filesize;
	dump.elaptot += diff;
	/* add this users name to /usr/net/usernames */
	addtoname(sn);
	}
send(jname)
	char *jname;
{	/* push those bytes */
	/* returns 0 if send fails, 1 otherwise */
	register int n;
	int i;
	long lsize;
	char mbuf[BUFSIZ];
	register char *p;
	FILE *jfile;

	debug("send %s",jname);
	if(stat(jname,&statbuf) < 0)goto sfail;
	lsize = getsize(&statbuf);
	if(lsize < MINSIZE){		/* all files are at least this long */
		unlink(jname);
		jname[0] = 'c';
		unlink(jname);
		return(1);
		}
	jfile = fopen(jname,"r");
	if(jfile == NULL)goto sfail;
	strcpy(mbuf,header);
	i = strlen(header);
	p = (char *)&lsize;
	lsize = fixuplong(lsize);
	mbuf[i] = *p++;
	mbuf[i+1] = *p++;
	mbuf[i+2] = *p++;
	mbuf[i+3] = *p++;
	i = i + 4;
	sendreset();
	masterseqno = 1;
	lastseqno = 0;
	if(xwrite(mbuf,1,i) == WRITEFAIL)goto bwrite;
	while((n=fread(buf,1,BLOCKSIZE,jfile)) > 0)
		if(xwrite(buf,1,n) == WRITEFAIL)goto bwrite;
	fclose(jfile);
	debug("end send");
	nsendfail = 0;
	return(1);
bwrite:
	nsendfail++;
	dump.nsendfail++;
	fclose(jfile);
	addtolog(remote,"^F%c\n",remote);
	return(0);
sfail:
	error("%s: %s",jname,sys_errlist[errno]);
	dump.nsendfail++;
	return(0);
	}
netrcv(){
	/* returns -2 in normal fail, -1 in abnormal fail, >= 0 otherwise */
	char code, sin;
	char tmach, fmach;
	char mgetc(), cflag, *s;
	register int n,i;
	char vmajor, vminor, c;
	int rcode, dummy, pid, uid;
	char buf1[BUFSIZ], cmdstr[BUFSIZ];
	static char parmlist[PARMLIST];
	long timesent,otime,olength,diff,rcvfinish,nt,maxfile = MAXFILE;
	double r;
	static char resp[FNS], infile[FNS], outfile[FNS];
	static char hbuf[10];
	static FILE *temp;

	n = nread(hbuf,1,strlen(header));
	if(n == BROKENREAD)return(-2);
	if(n != strlen(header) || strcmp(header,hbuf) != 0){
		error("wrong head %d %s",n,hbuf);
		return(-1);
		}
	n = nread(&length,1,4);
	if(n == BROKENREAD)return(-2);
	if(n != 4){
		error("bad length nread %d",n);
		return(-1);
		}
	length = fixuplong(length);
	olength = length;
	otime = gettime();
	debug("length = %ld\n",length);
	i = 0;

/* 
	begin parsing header

	from local to remote (requests)
	code	net option	reason
	q			normal request
	y	-y		simply skips login check (used by netlpr)

	from remote to local
	code	net option	reason
	w	-w		message to be written/mailed back
	s	-z		normal response
*/

	code = mgetc();
	tmach = mgetc();
	if(tmach < 'a' || 'z' < tmach){
		error("bad tmach");
		return(-1);
		}
	if(tmach != local)goto forw;	/* being forwarded through us */
	fmach = mgetc();
	vmajor = mgetc();
	vminor = mgetc();
	i += mgets(status.login,NS);
	i += mgets(status.mpasswd,20);
	demask(status.mpasswd);
	i += mgets(infile,FNS);
	i += mgets(outfile,FNS);
	i += mgets(resp,FNS);
	i += mgets(status.localname,NS);

	/* addrfrom is the person who sent this to us,
	   addrto is the person who received the command, i.e.
	   addrto is on this machine */
	if(status.localname[0] == 0)strcpy(status.localname,"root");
	sprintf(addrfrom,  "%s:%s",longname(fmach),status.localname);
	sprintf(addrto,    "%s:%s",longname(tmach),status.login);

	i += mgets(status.sTtyname,20);
	if(status.sTtyname[0] == 0)strcpy(status.sTtyname,"/dev/ttyx");
	cflag = mgetc();
	if(!fmach || !code || !cflag || !vmajor || !vminor){
		error("mgetc fails");
		return(-1);
		}
	cflag -= 'a';
	vmajor -= 'a';
	vminor -= 'a';
	if(vmajor != VMAJOR || vminor != VMINOR){
		/*
		error("versions dont agree (%d,%d) vs. (%d,%d) remote",
			VMAJOR,VMINOR,vmajor,vminor);
		return(-1);
		*/
		}
	i += mgets(buf,BUFSIZ);
	status.lTtytime = 0;
	sscanf(buf,"%lo",&status.lTtytime);

	i += mgets(parmlist,PARMLIST);
	status.jobno = atoi(parmlist);
	/* keep variable parameter list in jobno slot */
	parseparmlist(parmlist);

	i += mgets(buf1,BUFSIZ);		/* time sent */
	sscanf(buf1,"%ld",&timesent);
	i += mgetcmd(status.sCmdAct);
	i += mgetcmd(status.sCmdVirt);
	if(i != 0){error("mgets fails"); return(-1);}
	if(status.sCmdVirt[0] == 0)strcpy(status.sCmdVirt,status.sCmdAct);
	s = status.sCmdVirt;
	while(*s && *s != ' ')s++;
	c = *s;
	*s = 0;
	if(strcmp(status.sCmdVirt,"netlpr") == 0)dump.nnetlpr++;
	else if(strcmp(status.sCmdVirt,"netmail") == 0)dump.nnetmail++;
	else if(strcmp(status.sCmdVirt,"mail") == 0)dump.nsmail++;
	else if(strcmp(status.sCmdVirt,"netcp") == 0)dump.nnetcp++;
	else if(strcmp(status.sCmdVirt,"response") == 0)dump.nresp++;
	else dump.nnet++;
	*s = c;
	debug("%c %c %c (%c,%c) %s %s %s %s (%s) %s %c %lo %d %s\n",
		code,tmach,fmach,vmajor+'a',vminor+'a',status.login,
		infile,outfile,resp,status.localname,
		status.sTtyname,cflag+'a',status.lTtytime,status.jobno,status.sCmdAct);


	/* any chars left are data */
forw:
	sin = 0;
	if(length > 0){	/* make a temp input file */
		increment(tempfile);
		temp = fopen(tempfile,"w");
		if(temp == NULL){
			error("%s %s",tempfile,sys_errlist[errno]);
			return(-1);
			}
		chmod(tempfile,0600);
		if(tmach != local)fprintf(temp,"%c :%c :",code,tmach);
		while((n = mread(buf,1,BLOCKSIZE)) > 0)
			if(fwrite(buf,1,n,temp) != n){
				error("%s %s",tempfile,sys_errlist[errno]);
				fclose(temp);
				unlink(tempfile);
				return(-1);
				};
		fclose(temp);
		if(n == BROKENREAD || length > 0){
			unlink(tempfile);
			return(-2);
			}
		sin = 1;
		if(tmach != local){
			diff = gettime() - otime;
			if(diff < 1)diff = 1;	/* avoid dividing by 0 */
			r = olength;
			r = r/diff;
			addtolog(remote,"^P(to %c, %ldb, %ldsec, %4.1fb/sec)\n",
				tmach,olength,diff,r);
			dump.npass++;
			dump.bytetot += olength;
			dump.elaptot += diff;
			while((pid = fork()) == -1)sleep(2);
			if(pid == 0){
				execl(netcmd,"net","-x","-m",longname(tmach),
					"-s",tempfile,buf,0);
				exit(1);
				}
			wait(&dummy);
			unlink(tempfile);
			return(1);
			}
		}
	if(length > 0){error("file too short"); return(-1); }
	rcvfinish = gettime();

	while((pid = fork()) == -1)sleep(2);
	if(pid > 0){
		wait(&dummy);
		return(1);	/* normal return */
	}

	while((pid = fork()) == -1)sleep(2);
	if(pid != 0)exit(0);

	/* child process which forks and waits */
	mktemp(resfile);
	while((pid = fork()) == -1)sleep(2);
	if(pid == 0){
		/* child */
		strcpy(status.loginshell,Bsh);
		frommach = fmach;
		n = check(status.login,status.mpasswd,(code == 'q'));
		if(!n)errormsg(fmach,"Bad remote login/password '%s'",status.login);
		temp = fopen(resfile,"w");
		if(temp == NULL)
			errormsg(fmach,"creat %s %s",resfile,sys_errlist[errno]);
		chmod(resfile,0600);
		fclose(temp);
		mchown(resfile,status.muid,status.mgid);
		if(sin)
			mchown(tempfile,status.muid,status.mgid);
		setuid(status.muid);
		setgid(status.mgid);
		uid = getuid();
		uid = uidmask(uid);
		status.muid = uidmask(status.muid);
		if(uid != status.muid)error("setuid fails");
		debug("uid: %o\n",uid);
		/* check for allowed root commands, for security reasons */
		if(uid == SUPERUSER){
			s = status.sCmdAct;
			while(*s && *s != ' ')s++;
			c = *s;
			*s = 0;
			/* these are the only commands root may execute */
			if(strcmp(status.sCmdAct,"cat")            != 0
			&& strcmp(status.sCmdAct,CATCMD)           != 0
			&& strcmp(status.sCmdAct,FILECAT)          != 0
			&& strcmp(status.sCmdAct,MWRITECMD)        != 0
			&& strcmp(status.sCmdAct,"/usr/lib/tq")    != 0
			&& strcmp(status.sCmdAct,"/usr/lib/rtrrm") != 0
			&& strcmp(status.sCmdAct,"lpr")            != 0)
				errormsg(fmach,
					"Not allowed to execute '%s' as root",
					status.sCmdAct);
			*s = c;
			}
		if(chdir(status.dir) < 0)
			errormsg(fmach,"chdir %s %s",status.dir,sys_errlist[errno]);
		setenv(status.dir);	/* set up v7 environment */
		if(sin)mreopen(fmach,tempfile,"r",stdin);
		else if(infile[0])mreopen(fmach,infile,"r",stdin);
		else mreopen(fmach,"/dev/null","r",stdin);
		if(code == 's' && outfile[0]){
			if(stat(outfile,&statbuf) < 0
			   || getsize(&statbuf) != 0
			   || !(statbuf.st_mode&0600))
				errormsg(local,"Bad result file '%s'",outfile);
			mreopen(fmach,outfile,"w",stdout);
			}
		else if(outfile[0]){
			temp = fopen(outfile,"w");
			if(temp == NULL)
				errormsg(fmach,"fopen %s %s",outfile,sys_errlist[errno]);
			fclose(temp);
			mreopen(fmach,outfile,"w",stdout);
			}
		else mreopen(fmach,resfile,"a",stdout);
		debug("exec '%s'\n",status.sCmdAct);
		if(debugflg == 0){
			/* cheat */
			close(2);
			dup(1);
			/*
			mreopen(fmach,resfile,"a",stderr);
			*/
			}
		for(i=3;i<15;i++)close(i);
		do {
			mexecl(status.loginshell,"sh","-c",status.sCmdAct,0);
			sleep(2);
			} while(errno == ETXTBSY);
		exit(1);
		}
	/* parent */
	wait(&rcode);
	rcode >>= 8;
	/*
	fclose(stdin);
	fclose(stdout);
	fclose(stderr);
	*/
	if(sin)unlink(tempfile);
	/* 
	   now send something back to the sender 
	   unless this was a response (file or message)
	*/
	if((code == 'q' || code == 'y') && (resp[0] || !(cflag&F_NONOTIFY))){
		/* send response back if a response file
		was given or if mail/write is allowed */
		/* should give an error message for non-zero return codes */
		if(stat(resfile,&statbuf) < 0){
			error("%s %s",resfile,sys_errlist[errno]);
			goto next;
			}
		/* allow larger files between the Ingres machines */
		if(machtype[local  - 'a'] == M_INGRES
		&& machtype[remote - 'a'] == M_INGRES)
			maxfile = MAXFILELARGE;
		if(getsize(&statbuf) >= maxfile){
			errormsg(fmach,"Result file too large - not sent");
			goto next;
			}
		if(getsize(&statbuf) == 0){
			/* response file specified, no output generated */
			if(resp[0] != 0) goto next;
			/* quiet option - no output and a rcode of 0 */
			if(rcode == 0 && (cflag&F_QUIET))goto next;
		}
		/* use both old and new mwrite parm lists */

		if(resp[0])sprintf(cmdstr,"-o %s %s",resp,CATCMD);
		else sprintf(cmdstr,
"%s %s %s %lo %c %s \"'%s'\" %ld -t %s -f %s -x %ld -c \"'%s'\" -y %s -e %ld -r %d",
MWRITECMD, status.localname,status.sTtyname,status.lTtytime,tmach,status.login, status.sCmdVirt,timesent,
addrfrom, addrto, status.lTtytime, status.sCmdVirt, status.sTtyname, timesent, rcode);
		sprintf(buf,"%s -m%c -z -b -l %s -s %s -c response %s",
			netcmd,fmach,status.localname,resfile,cmdstr);
		dummy = system(buf);		/* execute command buf */
		}
next:
	unlink(resfile);
	s = ctime(&rcvfinish);
	s += 4;
	s[strlen(s) -8] = 0;
	diff = rcvfinish - otime;
	if(diff < 1)diff = 1;		/* avoid dividing by zero */
	r = olength;
	r = r/diff;
	dump.bytetot += olength;
	dump.elaptot += diff;
	sprintf(buf,"%s rcv  %c:%-8s (%s)",
		s,fmach,status.localname,status.login);
	addtolog(remote,"%s C: %s\n",buf,status.sCmdVirt);
	addtopublic("%s R: %d C: %s\n",buf,rcode,status.sCmdVirt);
	nt = rcvfinish - timesent - TIMEBASE;
	buf[0] = 0;
	if(nt > 0L)sprintf(buf," took (%s)",comptime(nt));
	addtolog(remote,"\t\tR: %d%s %ldb %ldsec %4.1fb/sec\n",
		rcode,buf,olength,diff,r);
	exit(0);
	/*UNREACHED*/
	}

/* 
   check() -- verify login name and password
   snTo    = login,
   pass    = passwd,
   verify  = 1 if password must check
   Returns 1 if password is ok, 0 if not.
*/
check(snTo,pass,verify)	/* 1 if OK, 0 if not */
  int verify;
  char *snTo, *pass; {
	int ver;
	char *s, *u, *nullstr = "";
	struct passwd *pwd;
	ver = (verify == 0);		/* ver is true if password verification
						was not requested */
	if(snTo[0] == 0)return(ver);
	if(!goodacctname(snTo))return(ver);
	pwd = getpwnam(snTo);
	if(pwd == NULL)return(ver);
# ifndef NEWPROT
	if(machtype[local-'a'] == M_CC && machtype[frommach-'a'] == M_CC)
		s = pass;
	else
# endif
	if(*pass)s = crypt(pass,pwd->pw_passwd);
	else s = nullstr;
	status.muid = guid(pwd->pw_uid,pwd->pw_gid);
	status.mgid = pwd->pw_gid;
	if(isdigit(pwd->pw_gecos[0]))status.jobno = atoi(pwd->pw_gecos);
	else status.jobno = 32767;
	strcpy(status.dir,pwd->pw_dir);
	strcpy(status.loginshell,pwd->pw_shell);
	u = status.loginshell;
	if(u[0] == 0 /* || strcmp("sh",u+strlen(u)-2) != 0 */) strcpy(u,Bsh);

	getpwdf(pwd);
	/* ignore network passwd */
	if(!spacct(status.login,s,status.localname,status.muid,status.mgid)
	&& strcmp(pwd->pw_passwd,s) && verify)
		return(0);
	return(1);
	}
mread(b,i,n)
  register int n; {
	if(length <= 0)return(0);
	if(length < n)n = length;
	n = nread(b,i,n);
	if(n != BROKENREAD)length -= n;
	return(n);
	}
char mgetc(){			/* returns 0 if fail */
	register char c;
	register int n;
	char buf[3];
	if((n=nread(buf,1,3)) == BROKENREAD)return(0);
	if(n != 3){error("bad read %d",n); return(0); }
	c = buf[0];
	if(buf[1] != ' ' && buf[1] != ':'){error("Bad char %c",buf[1]); return(0); }
	length -= 3;
	if(length < 0){error("length wrong2 %ld",length); return(0); }
	return(c);
	}
/* read in string over the network wire */
/* put string in s, max length is maxlen */
mgets(s,maxlen)			/* returns 0 if ok, 1 if not */
  int maxlen;
  register char *s; {
	register char *q;
	register int n;
	char c;
	q = s;
	for(;;) {
		if((n=nread(&c,1,1)) == BROKENREAD){
			*s = 0;
			error("mgets %s",s);
			return(1);
			}
		if(n == 0)break;
		if(c == '\\'){
			if((n=nread(&c,1,1)) == BROKENREAD){
				*s = 0;
				error("mgets %s",s);
				return(1);
				}
			if(n == 0)break;
			}
		if(c == ' ')break;
		if(maxlen-- > 0) *s++ = c;
		}
	*s = 0;
	if(nread(&c,1,1) == BROKENREAD){
		error("mgets %s",s);
		return(1);
		}
	length -= (s - q + 2);
	if(length < 0){error("length wrong1 %ld %s",length,q); return(-1); }
	if(maxlen < 0)
		error("mgets - string too long");
	return(0);
	}
mgetcmd(s)			/* returns 0 if succeed, 1 otherwise */
  char *s; {
	int i,n;
	char c;
	i = 0;
	for(;;){
		if((n=nread(&c,1,1)) == BROKENREAD){
			s[i] = 0;
			error("mgetcmd %s",s);
			return(1);
			}
		if(n <= 0 || c == '\n')break;
		if(c == '\\'){
			if(nread(&c,1,1) == BROKENREAD){
				s[i] = 0;
				error("mgetcmd %s",s);
				return(1);
				}
			length--;
			}
		s[i++] = c;
		length--;
		}
	s[i] = 0;
	length--;
	return(0);
	}
increment(s)
 char *s; {
	int i;
	char *p;
	i = strlen(s) - 1;
	while(s[i] == '9')i--;
	if(s[i] < '0' || s[i] > '9'){
		p = s+i+1;
		while(*p)*p++ = '0';
		return;
		}
	(s[i])++;
	i++;
	while(s[i])s[i++] = '0';
	return;
	}
pload(currt)
long currt; {
	struct tms tbf;
	static long out = 0L, ocut = 0L, ost = 0L, ocst = 0L;
	long u, s, elapt;
	double br,r,ru,rs;
	char *str,buf[BUFSIZ];
	currt = gettime();
	elapt = currt - dump.shorttime;
	times(&tbf);
	u = tbf.tms_utime-out + tbf.tms_cutime-ocut;
	s = tbf.tms_stime-ost + tbf.tms_cstime-ocst;
	dump.outime += u;
	dump.ostime += s;
	r = u + s;
	if(elapt > 0)r = (r/elapt)*100.0;
	else r = 0.0;
	/* adjust to be seconds */
	r = r/60.0;
	ru = u/60.0;
	rs = s/60.0;
	str = ctime(&currt);
	str[strlen(str) - 5] = 0;
	sprintf(buf,"%s (%s):\t%4.1fu\t%4.1fs\t%5.2f\t%s\n",
		str,longname(remote),ru,rs,r,comptime(elapt));
	addtodump(remote,"%s",buf);
	br = dump.bytetot;
	if(dump.elaptot > 0)br = br/dump.elaptot;
	else br = 0.0;
	addtodump(remote, "\tTrans.\t%.6ld bytes\t%4.1f bytes/sec\t%s",
		dump.bytetot,br,comptime(dump.elaptot));
	addtodump(remote," %s\n",comptime(dump.waittot));
	dump.shorttime = currt;
	dump.waittot = dump.elaptot = dump.bytetot = 0L;
	out = tbf.tms_utime;
	ocut = tbf.tms_cutime;
	ost = tbf.tms_stime;
	ocst = tbf.tms_cstime;
	sprintf(buf,"%s %c",NETQSTAT,remote);
	system(buf);				/* gather stats on netq len. */
	}
/* should also gather stats on # error msgs */
dumpit(currt)
  long currt; {
	register int ntot;
	long elapt;
	double r,ru,rs;
	register struct dumpstruc *p = &dump;
	register char *tt;
	tt = ctime(&currt);
	tt[strlen(tt) - 9] = 0;
	elapt = currt - dump.longtime;
	r = dump.outime + dump.ostime;
	if(elapt > 0)r = (r/elapt) * 100.0;
	else r = 0.0;
	ru = dump.outime/60.0;
	rs = dump.ostime/60.0;
	r = r/60.0;
	dump.longtime = dump.shorttime;
	ntot = p->nnetcp + p->nnetmail + p->nsmail + p->nnetlpr
		+ p->nresp + p->nnet;
	addtodump(remote,"Daily statisics\t(%s):\nSummary:\n",tt);
	addtodump(remote,"\t# sent %d\t# pass-thru %d\t# rcv %d:\t# netcp %d\n",
		p->nsend,p->npass,ntot,p->nnetcp);
	addtodump(remote,"\t# netlpr %d\t# netmail %d\t# sendmail %d\t# resp %d\n",
		p->nnetlpr,p->nnetmail,p->nsmail,p->nresp);
	addtodump(remote,"Protocol summary:\n");
	addtodump(remote,"\t# pk sent %d\t# pk rcv %d\t# b sent %ld\t# b rcv %ld\n",
		p->npacksent,p->npackrcv,p->nbytesent, p->nbytercv);
	addtodump(remote,
		"\t# send fails %d\t# retrans %d\t# abn %d\t\t# cksum errs %d\n",
		p->nsendfail,p->nretrans, p->nabnormal,p->ncksum);
	addtodump(remote,"Load:\t\t\t\t%4.1fu\t%4.1fs\t%5.2f%%\t%s\n",
		ru,rs,r,comptime(elapt));
	p->nbytesent = p->nbytercv = p->outime = p->ostime = 0L;
	p->nretrans = p->nloop = p->nabnormal = p->ncksum = 0;
	p->npacksent = p->npackrcv = p->nnetcp = p->nnetmail = 0;
	p->nsmail = p->nnetlpr = p->nnet = p->npass = 0;
	p->nsend = p->nsendfail = 0;
	}
/* returns 1 if n is ok, 0 if not */
goodacctname(n)
  char *n; {
	int i;
	i = -1;
	while(btable[++i].bname)
		if(strcmp(btable[i].bname,n) == 0 &&
			local == btable[i].bmach)return(0);
	return(1);
	}
demask(s)
  register char *s; {
# ifdef NEWPROT
	static char buf[20];
	strcpy(s,nbsdecrypt(s,THEKEY,buf));
# else
	while(*s){
		*s &= 0177;		/* strip quote bites */
		*s++ ^= 040;		/* invert upper-lower */
		}
# endif
	}
/*VARARGS0*/
mreopen(f,a,b,c){
/* simply handles errors by giving error msg */
	if(freopen(a,b,c) == NULL)errormsg(f,"%s: %s",a,sys_errlist[errno]);
	}
/* 
	addtopub(string, args)

	add a message to the public logfile /usr/net/logfile.
	note that the file must be writeable by everyone
	if error messages from the netrcv subroutine
	such as chdir errors are to be noticed.
*/
/*VARARGS0*/
addtopublic(s,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
char *s;
{
	static FILE *log = NULL;
	if(log == NULL){
		if(stat(publogfile,&statbuf) < 0)return;
		log = fopen(publogfile,"a");
		if(log == NULL)return;
		}
	fseek(log,0L,2);
	fprintf(log,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n);
	fflush(log);
	}
/*VARARGS0*/
addtodump(mach,str,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t,u)
char *str;
{
	static FILE *log = NULL;
	dumpfile[strlen(dumpfile)-1] = mach;
	if(log == NULL){
		if(stat(dumpfile,&statbuf) < 0)return;
		log = fopen(dumpfile,"a");
		if(log == NULL)return;
		}
	fseek(log,0L,2);
	fprintf(log,str,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,t,u);
	fflush(log);
	}
/* set up a dummy environment for v7 /bin/sh */
setenv(home)
  char *home; {
	static char *env[3],benv[2][50];
	env[0] = benv[0];
	env[1] = benv[1];
	strcpy(env[0],"PATH=:/bin:/usr/bin");
	sprintf(env[1],"HOME=%s",home);
	env[2] = 0;
	environ = env;
	}
/* errormsg- sends error message to user-
   may be on local or remote machine.
   Sends to addrfrom if fmach != local,
   or to addrto if fmach=local, on the assumption
   that addrfrom is where it came from and addrto is where it
   was destined on this machine.  The only time fmach=local is
   if the result file is screwed up.

   Note that errormsg can be called by the netrcv subroutine
   after the setuid() call to the specific user, so the 
   user must be able to get off an error msg back to him,
   and to write in the two log files.
   Can't use -w,-x,-y,-z for the net cmd because must be root for those.
*/
/*VARARGS0*/
errormsg(fmach,s,a,b,c,d,e,f,g,h)
char *s;
{
	int rcode;
	char errstr[BUFSIZ], cmdstr[BUFSIZ], rcmd[BUFSIZ];
	char toadd[FNS], fromadd[FNS], mchto, mchfrom;

	if(fmach < 'a' || 'z' < fmach)fmach = local;
	if(status.sTtyname[0] == 0)strcpy(status.sTtyname,"/dev/ttyx");
	/* will send to toadd, from fromadd */
	if(fmach == local || strcmp(status.sCmdVirt,"response") == 0){
		/* send to local mach, thus send to toaddr. */
		/* if this is an error during a response, send to local mach. */
		strcpy(toadd,  addrto);
		strcpy(fromadd,addrfrom);
	}
	else {		/* send to remote mach, thus send back to addrfrom*/
		strcpy(toadd,  addrfrom);
		strcpy(fromadd,addrto);
	}
	sprintf(errstr,"Error: ");
	sprintf(cmdstr,s,a,b,c,d,e,f,g,h);
	strcat(errstr,cmdstr);
	strcat(errstr,"\n");
	addtolog(remote,errstr);
	addtopublic(errstr);

	mchto =   MchSFromAddr(status.localname,toadd);
	mchfrom = MchSFromAddr(status.login,    fromadd);

	sprintf(rcmd,
"%s %s %s %lo %c %s \"'%s'\" %ld -t %s -f %s -x %ld -y %s -c \"'%s'\" -e %ld",
	MWRITECMD, 
	status.localname, status.sTtyname, status.lTtytime, local, status.login,status.sCmdVirt, gettime(),
	toadd, fromadd, status.lTtytime, status.sTtyname, status.sCmdVirt, gettime());
	if(fmach == local)
		sprintf(cmdstr, "echo \"%s\" | %s", errstr,rcmd);
	else sprintf(cmdstr,
	"echo \"%s\" | %s -m%c -b -c errormessage -l network - %s",
		errstr,netcmd,fmach,rcmd);
	rcode = system(cmdstr);
	exit(1);
	}
handlekill(){	/* SIGTRM signal */
	long t;
	addtodump(remote,"Netdaemon terminated.\n");
	t = gettime();
	pload(t);
	dumpit(t);
	exit(0);	/* kill myself */
	}
spacct(log,pass,localname,luid,lgid) /* returns 1 if login ok, 0 if not */
char *log,*pass,*localname; {
	long lt;
	int u,g;
	if(strcmp(log,"network") == 0)return(1);
	/* experimental */
# ifdef SPACCT
	if(strcmp(log,localname) == 0 && luid != 0 && lgid == 0
	&& isdigit(pass[0]) &&  (
	   strcmp(log,"source") == 0
	|| strcmp(log,"daemon") == 0)
	){
		/* login name is same on both machines, userid is group 0,
		non-root, the password is numeric
		and the login name is one of a select few */
		lt = atol(pass);
		u = (lt & 0177777);
		g = (lt >> 16);
		if((luid == u) && (lgid == g))return(1);
		}
# endif
	return(0);
	}
/* add the user's name to our name list */
/*VARARGS0*/
addtoname(name)
char *name;
{
	static FILE *log = NULL;
	if(name == NULL ||
	   name[0] == 0 ||
	   strcmp(name,"root") == 0 ||
	   strcmp(name,"network") == 0 ||
	   strcmp(name,"schmidt") == 0)return;
	if(log == NULL){
		if(stat(namefile,&statbuf) < 0)return;
		log = fopen(namefile,"a");
		if(log == NULL)return;
		}
	fseek(log,0L,2);
	fprintf(log,"%s:%s\n",longname(local),name);
	fflush(log);
	}

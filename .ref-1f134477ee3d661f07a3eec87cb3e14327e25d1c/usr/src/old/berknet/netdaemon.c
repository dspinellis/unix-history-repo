static char sccsid[] = "@(#)netdaemon.c	4.5	(Berkeley)	%G%";

/* sccs id variable */
static char *netdaemon_sid = "@(#)netdaemon.c	1.10";

/*

	The daemon program that runs the network.

Usage:
	netdaemon -m mach [-r readfd] [-w writefd] [-d] [-h]
		[-os] [-or] [-ou num] [-p len] [-8] [-l]

Must be started by root.
Options:
	-d		turn debugging on
	-h		use high-speed link (not implemented yet)
	-l		don't use net line discipline, even if available
	-m mach		remote machine is mach (required)
	-os		only send
	-or		only receive
	-ou num		only send things with uid = num
	-p num		length of packet
	-r num		if simulute w/pipes, read from num
	-w num		if simulate w/pipes, write on num
*/

# include "defs.h"
/* take a time, adjust to be in PST, and divide by no of secs in a day */
/* adjust by 10 mins, and day is considered to begin at 3AM */
/* (6*3600 = 21600) + 17400 = 39000 */
/* number of seconds in a day, usually 86400L */
# define nsecday 86400L
/* number of days since time began */
# define numdays(S) ((S - 39000L)/nsecday)
/* set my priority to normal */
# define RENICE0() { if (getuid() == 0) { nice(-40); nice(20); nice(0); } }

/* global variables */
extern char **environ;
struct dumpstruc dump;
struct bstruct btable[];
struct daemonparms netd;
struct userinfo status;

/* local variables */
static long length;
static DIR *dir;
/* static char sheader[] = 		"ABCDE"; */
static char tempfile[]= 	TEMPFILE;
static char publogfile[]=  	PUBLOGFILE;
static struct stat statbuf;
int handlekill();
static char frommach;
long linechars();

main(argc,argv)
  char **argv; {
	register int i;
	long ltime,t;
	char buf[100];

	nice(-1);
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, handlekill);
	if (signal(SIGQUIT, SIG_IGN) != SIG_IGN)
		signal(SIGQUIT, handlekill);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, handlekill);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM, handlekill);
	debugflg = DBV;
	setupdaemon(argc,argv);
	/* now running alone as a daemon */
		/*
		for(i=0; i<15; i++)close(i);
		signal(SIGHUP,SIG_IGN);
		signal(SIGQUIT,SIG_IGN);
		signal(SIGINT,SIG_IGN);
		*/
	/* set the umask to a reasonable value */
	umask( 022 );
	senddir[strlen(senddir)-1] = remote;		/* choose dir */
	if(chdir(senddir) < 0){
		perror(senddir);
		exit(EX_OSFILE);
		}
	dir = opendir(senddir);
	if(dir == NULL){
		perror(senddir);
		exit(EX_OSFILE);
		}
	mktemp(tempfile);
	tempfile[strlen(tempfile) - 7] = remote;
	ltime = gettime();
	if(ltime == 0L)
		fprintf(stderr,"The network says 'The clock is set wrong.'\n");
	sprintf(buf,"net restarted to %s %d %s",longname(remote),
		getpid(),ctime(&ltime));
	dump.longtime = ltime;
	dump.lastndays = numdays(ltime);
	addtolog(remote,buf);
	addtopublic(buf);
	fprintf(stderr,buf);
	if(!debugflg)fclose(stderr);
	sendpurge();
	mainloop();
	/* never returns */
}
/* the main loop of the daemon, alternatively rcv then send, if poss.*/
mainloop(){
	register int i;

	for(;;){	/* begin reading file */
		debug("daemon %c %d\n",remote,getpid());
		/* first receive */
		if(netd.dp_sndorcv >= 0){	/* if we can receive */
			i = netrcv();
			if(i == -1)dump.nabnormal++;
		}
		/* now look to send */
		if(netd.dp_sndorcv <= 0)	/* if we can send */
			netsend();
		/* print out statistics if the right time */
		printstat();
		dump.nloop++;
	}
}
	/* this code is a little strange because some machines
	   seem to have trouble having the date set, and time()
	   returns 0 until somebody remembers to set the date */
printstat(){
	long thisndays, thistime;
	thistime = gettime();
	thisndays = numdays(thistime);
	if(dump.longtime == 0L){
		dump.longtime = thistime;
		dump.lastndays = thisndays;
		return;
		}
	if(thisndays == dump.lastndays + 1L) dumpit(thistime);
	dump.lastndays = thisndays;
}
/* look for files to send */
netsend(){
	static long lasttime = 0;
	static char nleft = 1;
	long lFileLen,diff;
	double drate;
	register int uid,uidBest;
	char *sdate,*sn,*swait;
	long ot,nt,filesize;
	register int i;
	char stemp[20];
	static char jname[FNS];
	register struct direct *dp;

	debug("ck send");
	if(stat(senddir,&statbuf) < 0){
		error("%s %s",senddir,sys_errlist[errno]);
		return;
		}
	if(statbuf.st_mtime == lasttime && nleft == 0)return;	/* no need to search */
	lasttime = statbuf.st_mtime;
	rewinddir(dir);
	lFileLen = 10000000L;
	nleft = 0;
	while((dp = readdir(dir)) != NULL){
		if(dp->d_name[0] != 'c'
		   || dp->d_name[1] != 'f'
		   || dp->d_name[2] != remote
		   || stat(dp->d_name,&statbuf) < 0
		   || statbuf.st_mode == 0)
			continue;
		dp->d_name[0] = 'd';
		if(stat(dp->d_name,&statbuf) < 0 || statbuf.st_mode == 0)
			continue;
		uid = guid(statbuf.st_uid,statbuf.st_gid);
		if(netd.dp_onlyuid != 0 && uid != netd.dp_onlyuid && uid != SUPERUSER
			&& uid != NUID)continue;
		nleft++;
		filesize = getsize(&statbuf);
#ifndef DONTHOLDBIG
		if( (filesize > MAXDAYFILE) && day() ) {
			if( !debugflg )
				continue;
			else
				debug("sending large file %s\n", dp->d_name );
		}
#endif DONTHOLDBIG
		if(lFileLen > filesize){
			lFileLen = filesize;
			strcpy(jname,dp->d_name);
			uidBest = uid;
		}
# ifdef MAXSENDQ
		if(nleft > MAXSENDQ)break;
# endif MAXSENDQ
	}
	if(lFileLen == 10000000L)return;
	strcpy(stemp,jname);
	stemp[0] = 'c';
	sn = SnFromUid(uidBest);
	if(sn == NULL){
		addtolog(remote,"Unknown userid %d\n",uidBest);
		addtolog(remote,"Removing %s\n",stemp);
		unlink(stemp);
		return;
	}
	addtolog(remote,"^S %s %c: %s ",sn,remote,jname+2);
	ot = gettime();
	if(send(jname) == 0)return;
	nt = gettime();
	filesize = getsize(&statbuf);
	unlink(jname);
	unlink(stemp);
	diff = nt - ot;
	if(diff < 1)diff = 1;		/* avoid dividing by zero */
	sdate = ctime(&nt)+4;
	sdate[strlen(sdate) -9] = 0;
	swait = comptime(ot - statbuf.st_mtime);
	jname[3] = jname[2];
# ifndef NOFP
	drate = (double)filesize / (double)diff;
	addtolog(remote,"^T%c(%s, %ldb, %ldsec, %4.1fb/sec, w %s)\n",
		remote,sdate,filesize, diff,drate, swait);
# else NOFP
	addtolog(remote,"^T%c(%s, %ldb, %ldsec, w %s)\n",
		remote,sdate,filesize, diff,swait);
# endif NOFP
	addtopublic("%s: sent %-8s to %s (%s, %ld b, wait %s)\n",
		sdate,sn,longname(remote),jname+3,filesize,swait);
	dump.nsend++;
	dump.bytetot += filesize;
	dump.elaptot += diff;
	}

/*
   day() returns 1 if the time is between 6AM and 12PM
*/
day()
{
	int hour;
	long t;
	char *ctime();

	time( &t );
	sscanf( ctime( &t ), "%*s%*s%*s%2d", &hour );
	if( (hour>=0) && (hour<6) )
		return( 0 );		/* night */
	else
		return( 1 );		/* day */
}

send(jname)
	char *jname;
{	/* push those bytes */
	/* returns 0 if send fails, 1 otherwise */
	register int n;
	int i;
	long lsize;
	char mbuf[20], buf[MAXNBUF];
	register char *p;
	register FILE *jfile;

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
	/*
	strcpy(mbuf,sheader);
	i = strlen(sheader);
	p = (char *)&lsize;
	lsize = fixuplong(lsize);
	mbuf[i] = *p++;
	mbuf[i+1] = *p++;
	mbuf[i+2] = *p++;
	mbuf[i+3] = *p++;
	i = i + 4;
	sendreset();
	*/
	initseqno();
	sprintf(mbuf,"|%08ld|",lsize);
	i = 10;
	if(xwrite(mbuf,i) == WRITEFAIL)goto bwrite;
	while((n=read(fileno(jfile),buf,MAXNBUF)) > 0)
		if(xwrite(buf,n) == WRITEFAIL)goto bwrite;
	fclose(jfile);
	debug("end send");
	return(1);
bwrite:
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
	char sin;
	char mgetc(), *s;
	register int n;
	char c;
	int i, dummy, pid;
	unsigned rcode;
	long otime,olength,diff,rcvfinish,nt;
	double r;
	char hbuf[20], buf[MAXNBUF];
	register FILE *temp;
	static struct header hd;

	initseqno();
	/*
	n = nread(hbuf,strlen(sheader));
	if(n == BROKENREAD)return(-2);
	if(n != strlen(sheader) || strcmp(sheader,hbuf) != 0){
		error("wrong head %d %s",n,hbuf);
		return(-1);
		}
	n = nread(&length,4);
	length = fixuplong(length);
	*/
	n = nread(hbuf,10);
	if(n == BROKENREAD)return(-2);
	if(n != 10){
		error("bad length nread %d",n);
		return(-1);
		}
	hbuf[10] = 0;
	if(hbuf[0] != '|' || hbuf[9] != '|'){
		error("poor format %s",hbuf);
		return(-1);
		}
	hbuf[9] = 0;
	length = atol(hbuf+1);
	if(length < 0 || length > 100000000L){
		error("bad length %ld",length);
		return(-1);
		}
	dump.braw = 4;
	olength = length;
	otime = gettime();
	debug("length = %ld\n",length);

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

	i = readhd(&hd);
	if(i == -3)goto forw;			/* being forwarded thru us */
	if(i != 0)return(i);

	strcpy(status.login, hd.hd_snto);
	strcpy(status.localname,hd.hd_snfrom);

	demask(hd.hd_spasswd);

	s = hd.hd_scmdvirt;
	while(*s && *s != ' ')s++;
	c = *s;
	*s = 0;
	if(strcmp(hd.hd_scmdvirt,"netlpr") == 0)dump.nnetlpr++;
	else if(strcmp(hd.hd_scmdvirt,"netmail") == 0)dump.nnetmail++;
	else if(strcmp(hd.hd_scmdvirt,"mail") == 0)dump.nsmail++;
	else if(strcmp(hd.hd_scmdvirt,"netcp") == 0)dump.nnetcp++;
	else if(strcmp(hd.hd_scmdvirt,"response") == 0)dump.nresp++;
	else dump.nnet++;
	*s = c;

	printhd(&hd);

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
		if(hd.hd_mchto != local){
			fprintf(temp,"%c :%c :",hd.hd_code,hd.hd_mchto);
			fflush(temp);
		}
		/* this is the loop to read in all the data */
		while((n = mread(buf,MAXNBUF)) > 0)
			if(write(fileno(temp),buf,n) != n){
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
		if(hd.hd_mchto != local){
			diff = gettime() - otime;
			if(diff < 1)diff = 1;	/* avoid dividing by 0 */
# ifndef NOFP
			r = olength;
			r = r/diff;
			addtolog(remote,"^P(to %c, %ldb, %ldsec, %4.1fb/sec)\n",
				hd.hd_mchto,olength,diff,r);
# else NOFP
			addtolog(remote,"^P(to %c, %ldb, %ldsec)\n",
				hd.hd_mchto,olength,diff);
# endif NOFP
			dump.npass++;
			dump.bytetot += olength;
			dump.elaptot += diff;
			while((pid = fork()) == -1)sleep(2);
			if(pid == 0){
				RENICE0();
#ifdef CCV7
				/* make sure the spawned child has it's own
					group process to avoid the nasty
					"try again" message
				*/
				setpgrp();
#endif CCV7
				execl(netcmd,"net","-x","-m",longname(hd.hd_mchto),
					"-s",tempfile,0);
				error("%s: %s",netcmd,sys_errlist[errno]);
				exit(EX_UNAVAILABLE);
				}
			wait(&rcode);
			unlink(tempfile);
			rcode >>= 8;
			if(rcode != 0)
				error("pass-thru rcode %d", rcode);
			debug("passthru to %c code %c rcode %d",
				hd.hd_mchto,hd.hd_code,rcode);
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
	/* this is a child, who will go ahead and execute the command */
	/* running uid=0 at this point */
	RENICE0();
	/* nice(0 set back to 0 */
#ifdef CCV7
	/* separate group process */
	setpgrp();
#endif CCV7

	while((pid = fork()) == -1)sleep(2);
	if(pid != 0)exit(EX_OK);

	/* child process which forks and waits */
	mktemp(resfile);
	while((pid = fork()) == -1)sleep(2);
	if(pid == 0){
		/* child */
		strcpy(status.loginshell,Bsh);
		frommach = hd.hd_mchfrom;
		n = check(&hd,(hd.hd_code == 'q'));
		if(!n)errormsg(TRUE,&hd,NULL,
			"Bad remote login/password '%s'",hd.hd_snto);
		temp = fopen(resfile,"w");
		if(temp == NULL)
			errormsg(TRUE,&hd,NULL,
			"Create file %s: %s",resfile,sys_errlist[errno]);
		fclose(temp);
		chmod(resfile,0600);
		mchown(resfile,status.muid,status.mgid);
		if(sin)
			mchown(tempfile,status.muid,status.mgid);
		else tempfile[0] = 0;
		setgid(status.mgid);
		setuid(status.muid);
		/* after this point our gid, uid is the target user's */
		excmd(&hd,resfile,tempfile);
	}
	/* parent */
	wait(&rcode);
	rcode = (((rcode&077400) >>8) &0177);
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
	if((hd.hd_code == 'q' || hd.hd_code == 'y')
	&& (hd.hd_srespfile[0] || !hd.hd_fnonotify))
		sndresponse(&hd,rcode);
	unlink(resfile);
	s = ctime(&rcvfinish);
	s += 4;
	s[strlen(s) -8] = 0;
	diff = rcvfinish - otime;
	if(diff < 1)diff = 1;		/* avoid dividing by zero */
	dump.bytetot += olength;
	dump.elaptot += diff;
	sprintf(buf,"%s rcv  %c:%-8s (%s)",
		s,hd.hd_mchfrom,hd.hd_snfrom,hd.hd_snto);
	addtolog(remote,"%s C: %s\n",buf,hd.hd_scmdvirt);
	addtopublic("%s R: %d C: %s\n",buf,rcode,hd.hd_scmdvirt);
	nt = rcvfinish - hd.hd_ltimesent;
	buf[0] = 0;
	if(nt > 0L)sprintf(buf," took (%s)",comptime(nt));
# ifndef NOFP
	r = olength;
	r = r/diff;
	addtolog(remote,"\t\tR: %d%s %ldb %ldsec %4.1fb/sec\n",
		rcode,buf,olength,diff,r);
	r = dump.braw;
	r = r/diff;
	addtolog(remote,"\t\t%4.1frb/sec %4.1f%% use\n",r,(r/linechars())*100L);
# else NOFP
	addtolog(remote,"\t\tR: %d%s %ldb %ldsec\n",
		rcode,buf,olength,diff);
# endif NOFP
	exit(EX_OK);
	/*UNREACHED*/
	}
long linechars(){
	if(netd.dp_inspeed == 13)return(960L);
	else return(120L);
	}
/* 
	execute the user's command
	this procedure is executed with uid, gid of the user
*/
excmd(phd,tempresfile,tempinfile)
	register struct header *phd;
	char *tempresfile, *tempinfile;
{
	FILE *fd;
	int i, uid;
	register char *s, c;

	uid = getuid();
	uid = uidmask(uid);
	status.muid = uidmask(status.muid);
	if(uid != status.muid)error("setuid fails");
	debug("uid: %u, gid: %u\n",uid,status.mgid);
	/* check for allowed root commands, for security reasons */
	if(uid == SUPERUSER){
		s = phd->hd_scmdact;
		while(*s && *s != ' ')s++;
		c = *s;
		*s = 0;
		/* these are the only commands root may execute */
		if(strcmp(phd->hd_scmdact,"cat")            	!= 0
		&& strcmp(phd->hd_scmdact,MWRITECMD)        	!= 0
		&& strcmp(phd->hd_scmdact,"/bin/cat")       	!= 0
		&& strcmp(phd->hd_scmdact,"netrm")          	!= 0
		&& strcmp(phd->hd_scmdact,"/usr/lib/tq")    	!= 0
		&& strcmp(phd->hd_scmdact,"/usr/cc/lib/tq") 	!= 0
		&& strcmp(phd->hd_scmdact,"/usr/lib/rtrrm") 	!= 0
		&& strcmp(phd->hd_scmdact,"/usr/cc/lib/rtrrm")	!= 0
		&& strcmp(phd->hd_scmdact,"lpr")            	!= 0)
			errormsg(TRUE,phd,tempresfile,
				"Not allowed to execute '%s' as root",
				phd->hd_scmdact);
		*s = c;
		}
	if(chdir(status.dir) < 0)
		errormsg(TRUE,phd,tempresfile,
			"chdir %s: %s",status.dir,sys_errlist[errno]);
	setenv(status.dir);	/* set up v7 environment */
	if(tempinfile[0])mreopen(TRUE,phd,tempresfile,tempinfile,"r",stdin);
	else if(phd->hd_sinfile[0])mreopen(TRUE,phd,tempresfile,phd->hd_sinfile,"r",stdin);
	else mreopen(TRUE,phd,tempresfile,"/dev/null","r",stdin);
	if(phd->hd_code == 's' && phd->hd_soutfile[0]){
		if(stat(phd->hd_soutfile,&statbuf) < 0
		   || getsize(&statbuf) != 0)
			errormsg(FALSE,phd,tempresfile,"Bad result file '%s'",phd->hd_soutfile);
		mreopen(TRUE,phd,tempresfile,phd->hd_soutfile,"w",stdout);
		}
	else if(phd->hd_soutfile[0]){
		fd = fopen(phd->hd_soutfile,"w");
		if(fd == NULL)
			errormsg(TRUE,phd,tempresfile,"Open file %s: %s",
				phd->hd_soutfile,sys_errlist[errno]);
		fclose(fd);
		mreopen(TRUE,phd,tempresfile,phd->hd_soutfile,"w",stdout);
		}
	else mreopen(TRUE,phd,tempresfile,tempresfile,"a",stdout);
	debug("exec '%s'\n",phd->hd_scmdact);
	if(debugflg == 0){
		/* cheat */
		close(2);
		dup(1);
		/*
		mreopen(TRUE,phd,tempresfile,tempresfile,"a",stderr);
		*/
		}
	for(i=3;i<15;i++)close(i);
	if(strcmp(phd->hd_scmdact,"cat") == 0
	|| strcmp(phd->hd_scmdact,"/bin/cat") == 0)excat();
	do {
		mexecl(status.loginshell,"sh","-c",phd->hd_scmdact,0);
		sleep(2);
		} while(errno == ETXTBSY);
	perror(status.loginshell);
	exit(EX_UNAVAILABLE);
}
/* 
	send back a response

	if errormsg was called the resfile should be unlinked,
	to avoid two messages being sent there
*/
sndresponse(phd,rcode)
unsigned rcode;
struct header *phd;
{
	char cmdstr[BUFSIZ], buf[BUFSIZ];
	int dummy;
	long maxfile = MAXFILELARGE;
	/* send response back if a response file
	was given or if mail/write is allowed */
	if(stat(resfile,&statbuf) < 0){
		error("%s %s",resfile,sys_errlist[errno]);
		return;
		}
	if(getsize(&statbuf) >= maxfile){
		errormsg(TRUE,phd,"Result file too large - not sent");
		return;
		}
	if(getsize(&statbuf) == 0){
		/* response file specified, no output generated */
		if(phd->hd_srespfile[0] != 0)return;
		/* quiet option - no output and a rcode of 0 */
		if(rcode == 0 && phd->hd_fquiet)return;
	}
	/* use both old and new mwrite parm lists */

	if(phd->hd_srespfile[0])
		sprintf(cmdstr,"-o %s cat",phd->hd_srespfile);
	else sprintf(cmdstr,
"%s %s %s %lo %c %s \"'%s'\" %ld -t %s -f %s -x %ld -c \"'%s'\" -y %s -e %ld -r %d",
	MWRITECMD, phd->hd_snfrom,phd->hd_sttyname,phd->hd_lttytime,
	phd->hd_mchto,phd->hd_snto, phd->hd_scmdvirt,phd->hd_ltimesent-TIMEBASE,
	phd->hd_addrfrom, phd->hd_addrto, phd->hd_lttytime,
	phd->hd_scmdvirt, phd->hd_sttyname, phd->hd_ltimesent-TIMEBASE, rcode);

	sprintf(buf,"%s -m%c -z -b -l %s -s %s -c response %s",
		netcmd,phd->hd_mchfrom,phd->hd_snfrom,resfile,cmdstr);
	dummy = system(buf);		/* execute command buf */
}
	
/*

	excat
	does nothing more than copy standard input to standard
	output, like the cat command, but reports write errors.
	Uses getc and putc rather than fwrite and fread because
	the latter call getc and putc.
*/
excat(){
	register int n;
	char buf[BUFSIZ];

	errno = 0;
	while((n = read(0,buf,BUFSIZ)) > 0){
		if(write(1,buf,n) != n){
			perror("filecat: stdout");
			exit(EX_OSFILE);
			}
		}
	if(errno){
		perror("filecat: stdin");
		exit(EX_OSFILE);
	}
	exit(EX_OK);
}
/* returns errors for netrcv() */
static readhd(phd)
register struct header *phd;
{
	char cflag, sbuf[BUFSIZ], parmlist[PARMLIST], *cptr;
	int i, code;
	code = mgetc();
	phd->hd_mchto = mgetc();
	if(code != 'q' && code != 'y' && code != 'w' && code != 's'){
		error("bad code");
		return(-1);
		}
	phd->hd_code = code;
	for(i = 0; i < MAXINX; i++)
		if(phd->hd_mchto == inxtoch(i)) break;
	if(i >= MAXINX){
		error("bad phd->hd_mchto");
		return(-1);
		}
	if(phd->hd_mchto != local)return(-3);	/* being forwarded through us */
	phd->hd_mchfrom = mgetc();
	phd->hd_vmajor = mgetc();
	phd->hd_vminor = mgetc();
	i = 0;
	i += mgets(phd->hd_snto,NS);
	i += mgets(phd->hd_spasswd,20);
	i += mgets(phd->hd_sinfile,FNS);
	i += mgets(phd->hd_soutfile,FNS);
	i += mgets(phd->hd_srespfile,FNS);
	i += mgets(phd->hd_snfrom,NS);

	/* addrfrom is the person who sent this to us,
	   addrto is the person who received the command, i.e.
	   addrto is on this machine */
	if(phd->hd_snfrom[0] == 0)strcpy(phd->hd_snfrom,"root");
	sprintf(phd->hd_addrfrom,  "%s:%s",longname(phd->hd_mchfrom),phd->hd_snfrom);
	sprintf(phd->hd_addrto,    "%s:%s",longname(phd->hd_mchto),phd->hd_snto);

	i += mgets(phd->hd_sttyname,20);
	if(phd->hd_sttyname[0] == 0)strcpy(phd->hd_sttyname,"/dev/ttyx");
	cflag = mgetc();
	if(!phd->hd_mchfrom || !phd->hd_code || !cflag || !phd->hd_vmajor || !phd->hd_vminor){
		error("mgetc fails");
		return(-1);
		}

	cflag -= 'a';
	phd->hd_fnonotify = (cflag & F_NONOTIFY);
	phd->hd_fquiet = (cflag & F_QUIET);

	phd->hd_vmajor -= 'a';
	phd->hd_vminor -= 'a';

	i += mgets(sbuf,BUFSIZ);
	phd->hd_lttytime = 0;
	sscanf(sbuf,"%lo",&phd->hd_lttytime);

	i += mgets(parmlist,PARMLIST);
#ifdef CRN
	cptr = parmlist;
	while( *cptr != '(' )
		cptr++;
	*cptr = '\0';
	strcpy( phd->hd_ijobno, parmlist );
	*cptr = '(';
#else CRN
	strcpy( phd->hd_ijobno, "XYZZ" );
#endif CRN
	/* keep variable parameter list in crn slot */
	parseparmlist(parmlist);

	i += mgets(sbuf,BUFSIZ);		/* time sent */
	sscanf(sbuf,"%ld",&phd->hd_ltimesent);
	phd->hd_ltimesent += TIMEBASE;
	i += mgetcmd(phd->hd_scmdact);
	i += mgetcmd(phd->hd_scmdvirt);
	if(i != 0){error("mgets fails"); return(-1);}
	if(phd->hd_scmdvirt[0] == 0)strcpy(phd->hd_scmdvirt,phd->hd_scmdact);
	return(0);
}
/* 
   check() -- verify login name and password
   phd    = login,passwd
   fverify  = 1 if password must check
   Returns 1 if password is ok, 0 if not.
*/
check(phd,fverify)	/* 1 if OK, 0 if not */
register struct header *phd;
int fverify;
{
	char *sencpasswd, *u, *nullstr = "";
	struct passwd *pwd;
#ifdef CRN
	struct gecos *gcos;
#endif CRN
	if(phd->hd_snto[0] == 0)return(!fverify);
	debug("check: phd->hd_snto = %s\n", phd->hd_snto );
	if(!goodacctname(phd->hd_snto))return(!fverify);
	pwd = getpwnam(phd->hd_snto);
	debug("got pwd=%d, pwd->pw_passwd = %s\n",pwd, pwd->pw_passwd);
	if(pwd == NULL)return(!fverify);
	if(*phd->hd_spasswd)sencpasswd = crypt(phd->hd_spasswd,pwd->pw_passwd);
	else sencpasswd = nullstr;
	debug("check: passwd(rcvd)=%s, passwd(file) = %s, passwd(encrypt)=%s\n", phd->hd_spasswd, pwd->pw_passwd, sencpasswd );

	status.muid = guid(pwd->pw_uid,pwd->pw_gid);
	status.mgid = pwd->pw_gid;
#ifdef CRN
	if( (gcos=pwgecos( pwd->pw_gecos )) == NULL )
		strcpy( status.jobno, MAGICCRN );
	else 
		strcpy( status.jobno, gcos->gc_crn );
#else CRN
	strcpy( status.jobno, "XYZZ");
#endif CRN
	strcpy(status.dir,pwd->pw_dir);
	strcpy(status.loginshell,pwd->pw_shell);
	u = status.loginshell;
	if(u[0] == 0 || strcmp("/bin/sbash",u) == 0)strcpy(u,Bsh);

	getpwdf(pwd);
	/* ignore network passwd */
	/* acct is not a pair, acct is not "network", passwd is incorrect,
	and verification is requested => passwd not ok */
	if(!facctpaircheck(phd) && strcmp(phd->hd_snto,"network") != 0
	&& strcmp(pwd->pw_passwd,sencpasswd) != 0 && fverify)
		return(0);
	return(1);	/* otherwise passwd ok */
	}
mread(b,n)
  register int n; {
	if(length <= 0)return(0);
	if(length < n)n = length;
	n = nread(b,n);
	if(n != BROKENREAD)length -= n;
	return(n);
	}
char mgetc(){			/* returns 0 if fail */
	register char c;
	register int n;
	char buf[3];
	if((n=nread(buf,3)) == BROKENREAD)return(0);
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
		if((n=nread(&c,1)) == BROKENREAD){
			*s = 0;
			error("mgets %s",s);
			return(1);
			}
		if(n == 0)break;
		if(c == '\\'){
			if((n=nread(&c,1)) == BROKENREAD){
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
	if(nread(&c,1) == BROKENREAD){
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
		if((n=nread(&c,1)) == BROKENREAD){
			s[i] = 0;
			error("mgetcmd %s",s);
			return(1);
			}
		if(n <= 0 || c == '\n')break;
		if(c == '\\'){
			if(nread(&c,1) == BROKENREAD){
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
/* gather 24-hour stats and  mail to STATADDR */
/* should also gather stats on # error msgs */
dumpit(currt)
  long currt; {
	register struct dumpstruc *p = &dump;
	register int ntot;
	long elapt;
	double cputime,utime,stime,bs,rawbs;
	char *sstartt;
	FILE *fdm;
	char froma[30];
	struct tms tbf;

	/* if STATADDR is a file, the mail program this call will
	   ultimately execute must be able to deal with it,
	   and the remote mail program must be able to write on the
	   file, i.e. mode 666 */
	sprintf(froma,"%s=>",longname(local));
	strcat(froma,longname(remote));
	fdm = mailopen(STATADDR,froma,1,0);
	if(fdm == NULL)return;

	/* calculate times */
	elapt = currt - dump.longtime;
	ntot = p->nnetcp + p->nnetmail + p->nsmail + p->nnetlpr
		+ p->nresp + p->nnet;
	sstartt = ctime(&dump.longtime) + 4;
	sstartt[strlen(sstartt) - 9] = 0;

	times(&tbf);
# ifndef NOFP
	utime = tbf.tms_utime + tbf.tms_cutime;
	stime = tbf.tms_stime + tbf.tms_cstime;
	cputime = utime + stime;
	if(elapt > 0)cputime = (cputime/elapt) * 100.0;
	else cputime = 0.0;
	utime = utime/60.0;
	stime = stime/60.0;
	cputime = cputime/60.0;
	bs = p->bytetot;
	if(p->elaptot > 0)bs = bs /p->elaptot;
	else bs = 0.0;
# endif NOFP

	/* print out the statistics */
	fprintf(fdm,"Subject: %s, %s, time %s\n",
		froma,sstartt, comptime(elapt));
	fprintf(fdm,"Command summary:\n");
	fprintf(fdm,"\t# sent %d\t# pass_thru %d\t# rcv %d:\t# netcp %d\n",
		p->nsend,p->npass,ntot,p->nnetcp);
	fprintf(fdm,"\t# netlpr %d\t# netmail %d\t# sendbmail %d\t# resp %d\n",
		p->nnetlpr,p->nnetmail,p->nsmail,p->nresp);
	fprintf(fdm,"Protocol summary:\n");
	fprintf(fdm,"\t# pk_sent %d\t# pk_rcv %d\t# b_sent %ld\t# b_rcv %ld\n",
		p->npacksent,p->npackrcv,p->nbytesent, p->nbytercv);
	fprintf(fdm,
		"\t# send_fails %d\t# retrans %d\t# abn %d\t\t# cksum_errs %d\n",
		p->nsendfail,p->nretrans, p->nabnormal,p->ncksum);
# ifndef NOFP
	fprintf(fdm,"Load:\tuser %4.1f\tsys %4.1f\tpct %5.2f\trate %6.1f\n",
		utime,stime,cputime,bs);
	rawbs = p->brawtot*100L;
	rawbs = rawbs / linechars();
	fprintf(fdm,"\trawbytes %ld\tuse %4.1f\n", p->brawtot,rawbs);
# endif NOFP
	mailclose(fdm);

	/* reset counters */
	p->nbytesent = p->nbytercv = p->elaptot = p->bytetot = 0L;
	p->nretrans = p->nloop = p->nabnormal = p->ncksum = 0;
	p->npacksent = p->npackrcv = p->nnetcp = p->nnetmail = 0;
	p->nsmail = p->nnetlpr = p->nnet = p->npass = 0;
	p->nsend = p->nsendfail = 0;
	dump.longtime = currt;
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
/*
	static char buf[20];
	char skey[30];
	makeuukey(skey,status.login,local);
	strcpy(s,nbsdecrypt(s,skey,buf));
*/
	while(*s){
		*s &= 0177;		/* strip quote bites */
		*s++ ^= 040;		/* invert upper-lower */
		}
	}
/*VARARGS0*/
mreopen(fsendtofmach,phd,sfn,a,b,c){
/* simply handles errors by giving error msg */
	if(freopen(a,b,c) == NULL)
		errormsg(fsendtofmach,phd,sfn,"%s: %s",a,sys_errlist[errno]);
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
/* set up a dummy environment for v7 /bin/sh */
setenv(home)
  char *home; {
	static char *env[3],benv[2][50];
	env[0] = benv[0];
	env[1] = benv[1];
#ifdef CCV7
	strcpy( env[0], "PATH=:.:/usr/cc/bin:/usr/ucb/bin" );
#else CCV7
	strcpy(env[0],"PATH=:/bin:/usr/bin");
#endif CCV7
	sprintf(env[1],"HOME=%s",home);
	env[2] = 0;
	environ = env;
	}
/* 
	errormsg(fsendtofmach,phd,sfn,"string",arg(s))
	
	Sends error message to user.
	If fsendtofmach=TRUE, send to phd->hd_mchfrom, otherwise
	send to phd->hd_mchto.
	Also, if error occured during return of a "response",
	send to local machine.

	Note that errormsg can be called by the netrcv subroutine
	after the setuid() call to the specific user, so the 
	user must be able to get off an error msg back to him,
	and to write in the two log files.
	Can't use -w,-x,-y,-z for the net cmd because must be root for those.

	If sfn != NULL, then unlink sfn before exiting.
*/
/*VARARGS0*/
errormsg(fsendtofmach,phd,sfn,s,a,b,c,d,e,f,g,h)
char fsendtofmach;				
struct header *phd;
char *sfn,*s;
{
	int rcode;
	char errstr[BUFSIZ], cmdstr[BUFSIZ], rcmd[BUFSIZ];
	char toadd[FNS], fromadd[FNS], mchto, mchfrom;
	char snto[FNS], snfrom[FNS];

	if(phd->hd_sttyname[0] == 0)strcpy(phd->hd_sttyname,"/dev/ttyx");
	/* will send to toadd, from fromadd */
	if(!fsendtofmach || strcmp(phd->hd_scmdvirt,"response") == 0){
		/* send to tomach mach, thus send to toaddr. */
		/* if this is an error during a response, send to local mach. */
		strcpy(toadd,  phd->hd_addrto);
		strcpy(fromadd,phd->hd_addrfrom);
	}
	else {		/* send to remote mach, thus send back to addrfrom*/
		strcpy(toadd,  phd->hd_addrfrom);
		strcpy(fromadd,phd->hd_addrto);
	}
	sprintf(errstr,"Error: ");
	sprintf(cmdstr,s,a,b,c,d,e,f,g,h);
	strcat(errstr,cmdstr);
	strcat(errstr,"\n");
	addtolog(remote,errstr);
	addtopublic(errstr);

	mchto =   MchSFromAddr(snto,toadd); 
	mchfrom = MchSFromAddr(snfrom,fromadd);

	sprintf(rcmd,
"%s %s %s %lo %c %s \"'%s'\" %ld -t %s -f %s -x %ld -y %s -c \"'%s'\" -e %ld",
	MWRITECMD, snto, phd->hd_sttyname, phd->hd_lttytime, 
	local, snfrom,phd->hd_scmdvirt, phd->hd_ltimesent-TIMEBASE,
	toadd, fromadd, phd->hd_lttytime, phd->hd_sttyname, phd->hd_scmdvirt,
	phd->hd_ltimesent-TIMEBASE);

	if(mchto == local)
		sprintf(cmdstr, "echo \"%s\" | %s", errstr,rcmd);
	else 
		sprintf(cmdstr,
		"echo \"%s\" | %s -m%c -b -c errormessage -l network - %s",
			errstr,netcmd,mchto,rcmd);
	rcode = system(cmdstr);
	debug( "errormsg: cmdstr = %s\n", cmdstr );
	debug( "errormsg: rcode = %d\n", rcode );
	if(sfn != NULL)unlink(sfn);
	exit(EX_USAGE);
	}
handlekill(){	/* SIGTERM signal */
	long t;
	/*
	t = gettime();
	dumpit(t);
	*/
# ifdef NETLDISC
	/* turn off net line discipline if possible */
	netd.dp_linedis = 0;
	ioctl(netd.dp_linefd,TIOCSETD,&netd.dp_linedis);
	close(netd.dp_linefd);
	printf("Network line discipline turned off.\n");
# endif NETLDISC
	exit(EX_OK);	/* kill myself */
	}

/* check a request to see if it is an acct pair */
/* returns 1 if it is, 0 if not */
static facctpaircheck(phd)
register struct header *phd;
{
	return(0);
}


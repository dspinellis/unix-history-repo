/* Copyright (c) 1979 Regents of the University of California */
# include "defs.h"
/* must be setuid root */
/*
	net - -c cmd -f -i file -l name -mmach -n -o file -p passwd
		-r file -s file -w -x -y -z command
		
	-	take from standard input
	-c cmd	think of this as a "cmd" *
	-f	force prompting of user name and password
	-i file	remote stdin *
	-l name remote login name
	-m Mach	remote machine
	-n	do not mail or write back anything, not even err msgs
	-o file	remote stdout & stderr *
	-p pass remote password
	-r file	local response file
	-s file	local stdin file *
	
	(super users only, always skip login/passwd check:)
	-w	this is a write/mail response cmd *
	-x	this is being forwarded through us to another machine *
	-y	skip login/password check *
	-z	this is a response file being returned *

	* = not documented in net(NEW)
	
*/
static char dfname[]=		DFNAME;

main(argc, argv)
  char **argv; {
	register int i;
	int out();
	char resp[FNS], infile[FNS], outfile[FNS], localin[FNS];
	char buf[BFS*2];
	char sin, code, zopt, wopt, yopt, xopt;
	char *s;
	long cnt = 0l;
	char cflag = 'a';
	FILE *file, *temp, *rfile;

	struct utmp utmpstr;
	argv[argc] = 0;
	debugflg = DBV;
	sin = 0;
	cmd[0] = resp[0] = outfile[0] = infile[0] = 0;
	realcmd[0] = localin[0] = 0;
	wopt = xopt = yopt = zopt = 0;
	ttystr[0] = 0;
	if(isatty(0)) strcat(ttystr,ttyname(0));
	else if(isatty(2)) strcat(ttystr,ttyname(2));
	remote = 0;
	signal(SIGHUP,out);
	signal(SIGQUIT,out);
	signal(SIGINT,out);
	signal(SIGTERM,out);
	while(argc > 1 && argv[1][0] == '-'){
		argc--; argv++;
		switch(argv[0][1]){
		case 0:   sin++; break;
		case 'c': harg(realcmd,&argc,&argv); break;
		case 'f': status.force++; break;
		case 'i': harg(infile,&argc,&argv); break;
		case 'l': harg(status.login,&argc,&argv); break;
		case 'm': harg(buf,&argc,&argv); remote = lookup(buf); break;
		case 'n': status.nowrite++; status.nonotify++; break;
		case 'o': harg(outfile,&argc,&argv); break;
		case 'p':
			  harg(status.mpasswd,&argc,&argv);
			  if(status.mpasswd[0] == 0)
			  	strcpy(status.mpasswd,"\n\n");
			  break;
		case 'r': harg(buf,&argc,&argv); addir(resp,buf); break;
		case 's': harg(localin,&argc,&argv); break;
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
		strcat(cmd,argv[0]);
		strcat(cmd," ");
		}
	code = 'q';
	if(zopt || wopt || yopt || xopt){
		/* check z or w or y or x option permission */
# ifndef TESTING
		if(getuid() != 0){
			error("Not super-user");
			exit(1);
			}
# endif
		code = zopt ? 's' : 'w';
		code = yopt ? 'y' : code;
		if(status.mpasswd[0] == 0)	/* no passwd required */
			strcpy(status.mpasswd,"\n");
		}
/*
	code	option	reason
	q		normal request
	w	-w	message to be written back
	 	-x	being forwarded through us
	y	-y	simply skips login check (used by netlpr)
	s	-z	normal response
*/
/*
	debug("d:%s:%s:",status.defcmd,cmd);
*/
	commandfile(); 
	debug("d:%s:",cmd);
	if(remote == 0)remote = getremote(local);
# ifndef TESTING
	if(remote == local){
		fprintf(stderr,"Request sent to local machine - doesn't make sense\n");
		/* exit(1); */
		}
# endif
	strcat(status.defcmd," ");
	if(strlen(cmd) == 0)strcpy(cmd,status.defcmd);
	cmd[strlen(cmd)-1] = 0;
	mktemp(dfname);
	/* determine through machine */
	i = gothru(local,remote);
	if(i == 0){
		s = longname(remote);
		if(s != 0)fprintf(stderr,"No path to %s machine.\n",s);
		else fprintf(stderr,"Unknown machine\n");
		exit(1);
		}
	dfname[strlen(dfname)-11] = i;		/* set directory */
	dfname[strlen(dfname)-7] = i;		/* set file (unused) */
	if(resp[0]){
		if(strcmp(resp,"/dev/tty") == 0){
			error("Can't have /dev/tty as response file");
			exit(1);
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
				exit(1);
				}
			if((rfile=fopen(resp,"w")) == NULL){
				perror(resp);
				exit(1);
				}
			chmod(resp,0600);
			fclose(rfile);
			chown(resp,getuid(),getgid());
			}
		else if(access(resp,2) == -1){
			perror(resp);
			exit(1);
			}
		}
	file = fopen(dfname,"w");
	if(file == NULL){
		perror(dfname);
		exit(1);
		}
	chmod(dfname,0600);
	chown(dfname,getuid(),getgid());
	if(xopt)goto stickit;
	if(code == 'q'){
		passwdent();
		promptlogin();
		}
	if(status.mpasswd[0] == '\n')
		status.mpasswd[0] = 0;
# ifdef OLDPROT
	if(machtype[local-'a'] == M_CC && machtype[remote-'a'] == M_CC
		&& status.mpasswd[0] != 0){
		s = crypt(status.mpasswd);
		strcpy(status.mpasswd,s);
		}
# endif
	if(status.mpasswd[0] == 0 && code == 'q' &&
		strcmp(status.login,"network") != 0){
		fprintf(stderr,"Zero-length password not allowed\n");
		unlink(dfname);
		exit(1);
		}
	if(code == 'q' && (streql(status.login,"root") == 0 ||
		streql(status.login,"ruut") == 0)){
		fprintf(stderr,"Can't login as root through the network\n");
		unlink(dfname);
		exit(1);
		}
# ifdef SPACCT
	/* handle special accounts */
	strcpy(status.mpasswd,handlesp(status.login,status.mpasswd,
		status.localname,status.muid,status.mgid));
# endif
	enmask(status.mpasswd);
	ltime = 0;
	if(ttystr[0] && status.nowrite == 0){
		temp = fopen("/etc/utmp","r");
		if(temp == NULL){
			perror("/etc/utmp");
			unlink(dfname);
			exit(1);
			}
		while(fread(&utmpstr,1,sizeof utmpstr,temp) == sizeof utmpstr)
# ifdef OLDTTY
			if(utmpstr.ut_tty == ttystr[8]){
# else
			if(strcmp(utmpstr.ut_line,ttystr+5) == 0){
# endif
				ltime = utmpstr.ut_time;
				break;
				}
		}
/*
	debug("p:%s:\n",status.mpasswd);
*/
	if(status.nonotify)cflag += F_NONOTIFY;
/*
	protocol:
	code, remote mach, local mach, version stamp (2), remote login name,
	password, -i, -o, -r files, local login name, terminal, flag,
	utmp tty time, cc jobno, unused string,
	command '\n' real command '\n'
	any data
	
	changes:
	1) remove header
	2) remove unused string
	3) use ascii length instead of 4 bytes
	4) encrypt the login name, command, and part of data as well
*/

	fprintf(file,
	"%c :%c :%c :%c :%c :%s :%s :%s :%s :%s :%s :%s :%c :%lo :%d :%ld :",
		code,remote,local,VMAJOR+'a',VMINOR+'a',status.login,
		status.mpasswd,infile,outfile,resp,
		status.localname,ttystr,cflag,ltime,
		status.jobno,gettime()-TIMEBASE);
	fputs(cmd,file);
	putc('\n',file);
	fputs(realcmd,file);
	putc('\n',file);
stickit:
	if(sin)
		while((i = fread(buf,1,BUFSIZ,stdin)) > 0){
			if(fwrite(buf,1,i,file) != i){
				perror("net queue file");
				unlink(dfname);
				exit(1);
				}
			if((cnt += i) > MAXFILE)goto toobig;
			if(feof(stdin))break;
			}
	else if(localin[0]){
		if(access(localin,4) == -1){
			perror(localin);
			unlink(dfname);
			exit(1);
			}
		temp = fopen(localin,"r");
		if(temp == NULL){
			perror(localin);
			unlink(dfname);
			exit(1);
			}
		while((i = fread(buf,1,BUFSIZ,temp)) > 0){
			if((cnt += i) > MAXFILE)goto toobig;
			if(fwrite(buf,1,i,file) != i){
				perror("net queue file");
				unlink(dfname);
				exit(1);
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
	chown(dfname,getuid(),getgid());
	exit(0);
toobig:
	fprintf(stderr,"No more than %ld bytes can be sent\n",MAXFILE);
	out();		/* no return */
	}
out(){
	register int i;
	struct sgttyb stt;
	signal(SIGHUP,SIG_IGN); signal(SIGINT,SIG_IGN);
	signal(SIGQUIT,SIG_IGN); signal(SIGTERM,SIG_IGN);
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
# ifdef OLDPROT
	while(*s){
		*s &= 0177;		/* strip quote bites */
		*s++ ^= 040;		/* invert upper-lower */
		}
# else
	static char buf[20];
	strcpy(s,nbsencrypt(s,THEKEY,buf));
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

static char sccsid[] = "@(#)mwrite.c	4.1	(Berkeley)	%G%";

/* sccs id variable */
static char *mwrite_sid = "@(#)mwrite.c	1.2";
/*

	Mwrite provides the "response" function in the network.
	It sends its standard input to "toaddress", either by opening
	his terminal and writing on it, or by mailing it to him.
	It is executed by a remote machine from netdameon.c, either for a 
	response to be sent back or an errormsg to be sent back.
	It excutes mmail locally if it needs to.

Archaic Usage:
	mwrite toaddress ttystr lutmptime fmach fuser [command ltimeSent]

New Usage:
	mwrite [-t toaddress] [-f fromaddress] [-x lutmptime]
		[-c command] [-y ttystr] [-e ltimeSent] [-r rc]
	
	fmach is a single letter.
	ttystr is the full name, e.g. /dev/tty0
	ltimeSent is number of secs since TIMEBASE in decimal.
	lutmptime is the login time from utmp in OCTAL in the old protocol
	and in decimal in the new protocol.
	rc is the decimal return code (exit code>>8) of the command.
	command and ltimeSent are optional.

	There is duplication in this argument list.
	See the note in mmail.c about this stuff.

	Mwrite must be setuid bin or setuid root, to get in group 0,
	on the CC machines, in order to write on the user's terminal.

	Exit Codes:
		Returns 0 if the writing on the terminal works.
		Returns the return code of the mmail program if this is mailed.
*/
# include "defs.h"
jmp_buf env;
main(argc,argv)
  char **argv; {
	long lutmptime, otime, ltimeSent, ltimeCur, ltimeElap;
	int alarmint();
	FILE *file;
	int i;
	struct utmp *putmp;
	char buf[BUFSIZ],*s;
	char fromaddress[BUFSIZ], toaddress[BUFSIZ];
	char ttynamestr[BUFSIZ], cmdstr[BUFSIZ], *stimeCur, stimeSent[20];
	char src[10], stemp[BUFSIZ];
	struct stat statbuf;

	debugflg = DBV;
	argv[argc] = 0;
	otime = 0;
	src[0] = 0;
	errno = 0;

	/* NO LONGER NEEDED */
	strcpy(toaddress,argv[1]);
	strcpy(ttynamestr,argv[2]);
	sscanf(argv[3],"%lo",&lutmptime);
	sprintf(fromaddress,"%s:%s",longname(argv[4][0]),argv[5]);
	if(argc > 6)strcpy(cmdstr,argv[6]);
	else cmdstr[0] = 0;
	if(argc > 7)strcpy(stimeSent,argv[7]);
	else stimeSent[0] = 0;

	/* parse arguments */
	for(i = 1; i < argc; i++){
		if(argv[i][0] == '-')
		switch(argv[i][1]){
		case 't':
			strcpy(toaddress,argv[++i]);
			break;
		case 'y':
			strcpy(ttynamestr,argv[++i]);
			break;
		case 'x':
			lutmptime = atol(argv[++i]);
			break;
		case 'f':
			strcpy(fromaddress,argv[++i]);
			break;
		case 'c':
			strcpy(cmdstr,argv[++i]);
			break;
		case 'e':
			strcpy(stimeSent,argv[++i]);
			break;
		case 'r':
			strcpy(src,argv[++i]);
			break;
		}
		/* it is important that this code ignore unknown flags
		   so that options can be added w/o recompiling */
	}

	ltimeSent=atol(stimeSent)+TIMEBASE;

	setjmp(env);
	alarm(0);
	signal(SIGALRM,alarmint);
	if(errno != 100 && ttynamestr[0] && ttynamestr[8] != 'x'){
		alarm(100);
		putmp = getutmp(ttynamestr);
		if(putmp != NULL) otime = putmp->ut_time;
		/*
		debug("lutmptime %lo otime %lo",lutmptime,otime);
		*/
		if(otime != 0 && otime == lutmptime) {
			file = fopen(ttynamestr,"w");
			if(file != NULL && fstat(fileno(file),&statbuf) !=  -1
				&& (statbuf.st_mode&02)){
				alarm(0);
				if(src[0] != 0)sprintf(stemp,", R: %s",src);
				else stemp[0] = 0;
				ltimeCur = gettime();
				stimeCur = ctime(&ltimeCur);
				stimeCur += 11;
				stimeCur[strlen(stimeCur)-9] = 0;
				fprintf(file,
					"\r\nMessage from %s at %s ...\r\n",
					fromaddress, stimeCur);
				if(cmdstr[0] != 0){
					s = ctime(&ltimeSent);
					s[strlen(s)-6] = 0;
					ltimeElap = ltimeCur - ltimeSent;
					fprintf(file,
					"(command: %s%s, sent %s, took %s)\r\n",
					cmdstr,stemp,s,comptime(ltimeElap));
				}
				while(fgets(buf,BUFSIZ,stdin) != NULL){
					fputs(buf,file);
					fputc('\r',file);
				}
				fprintf(file,"------\r\n");
				exit(EX_OK);
				}
			}
		}
	
	/* well, couldn't write to him, so we'll mail to him on this mach */
	/* mail to "toaddress", saying its from "fromaddress", about a command
	   "cmdstr", which was sent at time "stimeSent" */

	alarm(0);
	sprintf(stimeSent,"%ld",ltimeSent);
	if(cmdstr[0] != 0){
		if(src[0] != 0)
			mexecl(MMAILCMD,"mmail", "-r",src, "-c",cmdstr,
				"-e",stimeSent,"-f",fromaddress,
				"-t",toaddress,"-z",0);
		else
			mexecl(MMAILCMD,"mmail", "-c",cmdstr, "-e",stimeSent,
				"-f",fromaddress, "-t",toaddress,"-z",0);
	}
	else
		mexecl(MMAILCMD,"mmail", "-f",fromaddress, "-t",toaddress,
			"-z", 0);
	perror(MMAILCMD);
	exit(EX_UNAVAILABLE);
}
alarmint(){
	errno = 100;
	signal(SIGALRM,SIG_IGN);		/* alarm off */
	longjmp(env,0);			/* ugh */
	}

# include "defs.h"
/*
   Mmail is a berkeley network internal command.
   It is executed locally by the mwrite command,
   and from a remote machine by the sendberkmail command.
   Its purpose is to send mail to a user on this
   machine using the system mail program.

Archaic Usage:

   mmail [-commandsent -timesent] fromuser frommach touser

Correct Usage:
   mmail [-c commandsent] [-e timesent] [-f fromaddress] [-t toaddress] 
	[-h hopcnt] [-r rc] [-z]

   The mwrite command uses all the options.
   The sendberkmail command does not use the commandsend, timesent and rc 
   options.
   Timesent is time in seconds since 1901 in decimal, as returned by time().
   Frommach is a multi-character name, not a single letter.
   Rc is the return code (exit code>>8) of the command.

Assumptions about the system mail command:
1. We assume there is an optional argument "-r" which can be added to mail.
   Mail argument format (two choices):

   (ROPTION)
		mail -f fromaddress toaddress
	which becomes mail from "fromaddress" instead of "network".

   (ROPTION2)
		mail -r frommach fromuser toaddress
	which becomes mail from "fromach:fromuser" instead of "network".

   ROPTION is to be preferred.

2. We assume that mail accepts the "-h hopcnt" flag, and passes it thru
   unchanged to the sendberkmail program.  The hopcnt is incremented everytime
   it passes thru mmail, so inifinite mail forwarding is detected.
   Since both the from and to addresses cycle, it there is infinite looping
   we simply mail to root to that effect and throw away the mail.


   If this argument scheme looks flakey it is because I screwed up
   in the argument design.  With the network now up to 10 machines,
   I can't add another parameter to the internal commands of the network
   like mmail and mwrite.  If I had used labeled parms instead of
   positional parms, I would be able to add more options/info
   without having to recompile all code...

   exit codes:
	normally returns the exit code from the mail program
	returns 101 if unable to exec the mail program
	returns 102 if infinite mail forwarding loop is detected.

   NUID is the value returned from getuid for "network" account.
*/
main(argc,argv)
  char **argv; {
	int pip[2], n, ret, i, uid, roption = 0, hopcnt = 0, pid;
	char *sargv[20], *cmdstr=NULL, buf[BUFSIZ], *timestr, fromaddress[BUFSIZ];
	char toaddress[BUFSIZ], src[20], snFrom[BUFSIZ], snto[BUFSIZ], mchFrom, mchto,
		*sn, stemp[BUFSIZ], fisresponse = 0;
	long timesent = TIMEBASE, el;

	debugflg = DBV;
	src[0] = 0;

	/* parse old format positional parms */
	if(argv[1][0] == '-'){
		cmdstr = argv[1] + 1;
		timesent = atol(argv[2] + 1);
		sprintf(fromaddress,"%s:%s",argv[4],argv[3]);
		strcpy(toaddress,argv[5]);
	}
	else {
		sprintf(fromaddress,"%s:%s",argv[2],argv[1]);
		strcpy(toaddress,argv[3]);
	}
	argv[argc] = 0;

	/* parse labeled parameters */
	/*  prob because of -cmd in arg1 and arg2 */
	for(i = 1; i < argc; i++){
		if(argv[i][0] == '-' && argv[i][2] == 0)
		switch(argv[i][1]){
		case 'f': 
			strcpy(fromaddress,argv[++i]);
			break;
		case 'c': 
			cmdstr = argv[++i];
			break;
		case 'e':
			timesent = atol(argv[++i]);
			break;
		case 't':
			strcpy(toaddress,argv[++i]);
			break;
		case 'h':
			hopcnt = atoi(argv[++i]);
			break;
		case 'r':
			strcpy(src,argv[++i]);
			break;
		case 'z':
			fisresponse++;
			break;
		/* it is important there be no error if an unknown
		   flag is encountered */
		}
	}
	mchFrom = MchSFromAddr(snFrom,fromaddress);

	/* compute time send */
	timestr = ctime(&timesent);
	timestr[strlen(timestr) - 6] = 0;
	el = gettime() - timesent;

# ifdef ROPTION
	/* only use roption if super-user or "network" */
/*
	uid = getuid();
	uid = uidmask(uid);
	if(uid == SUPERUSER || uid == NUID)roption++;
*/
	/* or if mail is to the same user who is sending it */
/*
	sn = SnCurrent();
	if(sn != NULL){
		stemp[0] = 0;
		mchfrom = MchSFromAddr(stemp,fromaddress);
		if(strcmp(stemp,????) == 0)roption++;
	}
	uid = geteuid();
	uid = uidmask(uid);
	if(uid == SUPERUSER || uid == NUID)roption++;
*/
	roption++;
# endif

# ifdef ROPTION2
	/* only use roption if super-user or "network" */
	uid = getuid();
	uid = uidmask(uid);
	if(uid == SUPERUSER || uid == NUID)roption++;
	/* or if mail is to the same user who is sending it */
	/* disabled since mail must accept this  and it does not now */
	/* if(strcmp(SnCurrent(),argv[3]) == 0)roption++; */
	uid = geteuid();
	uid = uidmask(uid);
	if(uid == SUPERUSER || uid == NUID)roption++;
# endif

	/* check the hopcnt */
	hopcnt++;
	if(hopcnt > MAXHOPS)hopcnterr(toaddress, hopcnt);

	/* analyze the dest, if local, strip off mach name, otherwise ok */
	mchto = MchSFromAddr(snto,toaddress);
	if(mchto == local)strcpy(toaddress,snto);

	/* add the hopcnt here */
	if(roption){
		sargv[1] = "-r";
# ifdef ROPTION2
		sargv[2] = longname(mchFrom);
		sargv[3] = snFrom;
		sargv[4] = toaddress;
		sargv[5] = 0;
# endif
# ifdef ROPTION
		sargv[2] = fromaddress;
		sargv[3] = toaddress;
		sargv[4] = 0;
# endif
		}
	else {
		sargv[1] = toaddress;
		sargv[2] = 0;
		}
	sargv[0] = "mail";
	pipe(pip);
	while((pid = fork()) == -1)sleep(2);
	if(pid == 0){
		close(pip[1]);
		close(0);
		dup(pip[0]);
		/* by convention, SYSMAIL1 may forward this mail
		   and response messages shouldn't be forwarded */
		if(!fisresponse)
			execv(SYSMAIL1, sargv);
		execv(SYSMAIL2, sargv);
		execv(SYSMAIL3, sargv);
		execv(SYSMAIL4, sargv);
		exit(101);
		/* mexecv is not used because it gives a diagnostic */
		}
	close(pip[0]);
	close(1);
	dup(pip[1]);
	if(!roption)
		printf("(from %s)\n",fromaddress);
	if(cmdstr != NULL){
		if(src[0] != 0)sprintf(stemp,", R: %s", src);
		else stemp[0] = 0;
		printf("(command: %s%s, sent %s, took %s)\n",
			cmdstr,stemp,timestr,comptime(el));
	}
	fflush(stdout);
	while((n = read(0,buf,BUFSIZ)) > 0)
		write(pip[1],buf,n);
	close(pip[1]);
	close(1);
	wait(&ret);
	ret >>= 8;
	if(ret != 0)
		fprintf(stderr,
		"Non-zero return code (%d) from the mail program.\n",ret);
	exit(ret);
	}
/*
	hopcnterr()

	there appears to be infinite mail forwarding -
	as detected by the hop count.  Mail to root and give up.
	Both the from and to addresses are cycling, so mail 
	can't be sent there.
*/
hopcnterr(toaddress,hopcnt)
	char *toaddress;
	int hopcnt;
{
	char cmdstr[BUFSIZ];
	int rcode;
	sprintf(cmdstr,"echo infinite mail loop for %s hops %d | mail root",
		toaddress,hopcnt);
	rcode = system(cmdstr);
	exit(102);
	/*UNREACHED*/
}

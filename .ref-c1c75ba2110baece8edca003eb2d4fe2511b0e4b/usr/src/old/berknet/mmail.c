static char sccsid[] = "@(#)mmail.c	4.1	(Berkeley)	%G%";

# include "defs.h"
/* sccs id variable */
static char *mmail_sid = "@(#)mmail.c	1.2";

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

	mail -r fromaddress toaddress

   which becomes mail from "fromaddress" instead of "network".

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

*/
main(argc,argv)
  char **argv; {
	int n, ret, i, hopcnt = 0, pid;
	char *sargv[20], *cmdstr=NULL, buf[BUFSIZ], *timestr, 
		fromaddress[BUFSIZ];
	char toaddress[BUFSIZ], src[20], snFrom[BUFSIZ], snto[BUFSIZ],
		mchFrom, mchto, stemp[BUFSIZ], fisresponse = 0;
	long timesent = TIMEBASE, el;
	FILE *fdm;

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

	/* check the hopcnt */
	hopcnt++;
	if(hopcnt > MAXHOPS)hopcnterr(toaddress, hopcnt);

	/* analyze the dest, if local, strip off mach name, otherwise ok */
	mchto = MchSFromAddr(snto,toaddress);
	if(mchto == local)strcpy(toaddress,snto);

	/* it is important to realize that mmail is executed
	   either as root, network, or the USER!
	   So the -r option must be accepted (and possibly ignored)
	   by the mail program if the user is a reandom user. 
	*/
	/* now we fork off a mail command. if fisresponse, then
	we are "cautious" and don't use mail forwarders */

	fdm = mailopen(toaddress, fromaddress, fisresponse, hopcnt);
	if(cmdstr != NULL){
		if(src[0] != 0)sprintf(stemp,", R: %s", src);
		else stemp[0] = 0;
		fprintf(fdm,"Subject: \"%s\"%s, sent %s, took %s\n",
			cmdstr,stemp,timestr,comptime(el));
	}
	while((n = fread(buf,1,BUFSIZ,stdin)) > 0)
		fwrite(buf,1,n,fdm);
	ret = mailclose(fdm);
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
	exit(EX_OSERR);
	/*UNREACHED*/
}

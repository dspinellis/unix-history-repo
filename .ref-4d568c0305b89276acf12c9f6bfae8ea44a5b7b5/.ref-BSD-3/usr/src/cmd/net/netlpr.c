/*

netlpr [-m mach] [-l login] [-p password] [-f] [-n] [-q] [-c ?pr] [file1 ... filen]

	send a set of files to the lineprinter on the mach machine.
	if between CC machines, no account is needed, so no login
	or passwd is prompted.
	Other flags are simply passed thru.
	Flags:
		-m mach		remote machine
		-l login	remote login name, ignored on CC
		-p passwd	remote password,     "     "   "
		-f		force prompting of user name
		-n		don't send anything back
		-q		quiet option
		-c printer	use the "printer" command instead of "lpr"
*/
/* also, netlpr should check the remote lpr's other user execute
   bit to see if it is down
*/
/* must run setuid root on CC machines only */

# include "defs.h"

main(argc,argv)
  char **argv; {
	/* parms are flags to the lpr command on the rem mach */
	int rcode,uid,pid;
	char parms[BUFSIZ], fnoacct, *sn, suid[20], sLprCommand[20];
	static char tomachstr[BUFSIZ], realcmd[BUFSIZ], tempbuf[BUFSIZ], sCmdAct[BUFSIZ];
	argv[argc] = 0;
	parms[0] = 0;
	debugflg = DBV;

	strcpy(sLprCommand,"lpr");

	argv++; argc--;
	while(argc > 0 && argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'b': status.nonotify++; break;
		case 'c': harg(sLprCommand,&argc,&argv); break;
		case 'f': status.force++; break;
		case 'l': harg(status.login,&argc,&argv); break;
		case 'm': harg(tempbuf,&argc,&argv); 
			  remote = lookup(tempbuf);
			  break;
		case 'n': status.nowrite++; break;
		case 'p': harg(status.mpasswd,&argc,&argv); break;
		case 'q': status.quiet++; break;
		default:  strcat(parms,argv[0]); strcat(parms," "); break;
		}
		argc--, argv++;
		}
	/* read the .netrc file, to get a value for "remote" */
	commandfile();
	if(remote == 0)remote = getremote(local);
	/* fnoacct is true if no password is needed to netlpr */
	fnoacct = machtype[remote-'a']==M_CC&&machtype[local-'a']==M_CC;

	if(fnoacct){ /* no acct needed. */
		/* look in passwd file for jobno */
		uid = getuid();
		passwdent();
		sprintf(suid,"%d",uid);
	} else {
		/* get status.localname */
		sn = SnCurrent();
		if(sn == NULL){
			fprintf(stderr,"Unknown userid\n");
			exit(1);
		}
		strcpy(status.localname,sn);
		/* prompt for login and passwd */
		promptlogin(remote);
	}

	/* check to see that the lpr command is one of the approved set */
	if(strcmp(sLprCommand,"lpr") != 0
	&& strcmp(sLprCommand,"epr") != 0
	&& strcmp(sLprCommand,"bpr") != 0){
		fprintf(stderr,
			"May not execute the '%s' command from netlpr.\n",
			sLprCommand);
		exit(1);
	}

	do {
		if(fnoacct)
			sprintf(sCmdAct,"%s -c%04d,%s %s",
			sLprCommand,status.jobno,status.localname,parms);
		else 	sprintf(sCmdAct,"%s %s",sLprCommand,parms);

		sprintf(tomachstr,"-m%c",remote);
		sprintf(realcmd,"netlpr %s%s",parms,(argc > 0 ? argv[0] : ""));

		while((pid = fork()) == -1)sleep(2);
		if(pid == 0){
			if(argc > 0){
				if(access(argv[0],04) == -1){
					perror(argv[0]);
					exit(1);
					}
# ifdef CC
				if(fnoacct){
					setuid(0);
					mexecl(netcmd,"net",tomachstr,"-y",
					"-l",status.localname,"-u",suid,
					"-s",argv[0],"-c",realcmd,sCmdAct,0);
					}
				else
# endif
				{
					kexecl(netcmd,"net",tomachstr,
					"-s",argv[0],"-c",realcmd,sCmdAct,0);
					}
				}
			else {
# ifdef CC
				if(fnoacct){
					setuid(0);
					mexecl(netcmd,"net",tomachstr,"-y","-",
					"-l",status.localname,"-u",suid,
					"-c",realcmd,sCmdAct,0);
					}
				else
# endif
				{
					kexecl(netcmd,"net",tomachstr, "-",
					"-c",realcmd,sCmdAct,0);
					}
				}
			fprintf(stderr,"Network is down\n");
			exit(1);
			}
		wait(&rcode);
		argc--, argv++;
		} while(argc > 0);
	exit(rcode >> 8);
	}

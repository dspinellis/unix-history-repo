static char sccsid[] = "@(#)netlpr.c	4.1	(Berkeley)	9/12/82";

/* sccs id variable */
static char *netlpr_sid = "@(#)netlpr.c	1.4";
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

/* global variables */
struct userinfo status;

main(argc,argv)
  char **argv; {
	/* parms are flags to the lpr command on the rem mach */
	int rcode,uid,pid, Iflag;
	char parms[BUFSIZ], fnoacct, *sn, suid[20], sLprCommand[20];
	static char tomachstr[BUFSIZ], realcmd[BUFSIZ], tempbuf[BUFSIZ],
		sCmdAct[BUFSIZ];
	argv[argc] = 0;
	parms[0] = 0;
	debugflg = DBV;

	strcpy(sLprCommand,"lpr");

	argv++; argc--;
	Iflag = 0;
	while(argc > 0 && argv[0][0] == '-'){
		if( Iflag ) {
			strcat( parms, argv[0] );
			strcat( parms, " " );
			argc--; argv++;
			continue;
		}
		switch(argv[0][1]){
		case 'c': harg(sLprCommand); break;
		case 'f': status.force++; break;
		case 'l': harg(status.login); break;
		case 'm': harg(tempbuf); 
			  remote = lookup(tempbuf);
			  if(remote == 0){
				fprintf(stderr,"Unknown machine %s\n",tempbuf);
				exit(EX_NOHOST);
			  } 
			  break;
		case 'n': status.nowrite++; break;
		case 'p': harg(status.mpasswd); break;
		case 'q': status.quiet++; break;
		case 'I': Iflag++; break;
		default:  strcat(parms,argv[0]); strcat(parms," "); break;
		}
		argc--, argv++;
		}
	/* read the .netrc file, to get a value for "remote" */
	commandfile();
	if(remote == 0)remote = getremote(local);
	/* fnoacct is true if no password is needed to netlpr */
	fnoacct = machtype[chtoinx(remote)]==M_CC
	          && machtype[chtoinx(local)]==M_CC;

	if(fnoacct){ /* no acct needed. */
		/* look in passwd file for jobno */
		uid = getuid();
		passwdent();
		sprintf(suid,"%d",uid);
	} else {
		/* get status.localname */
		sn = SnFromUid(getuid());
		if(sn == NULL){
			fprintf(stderr,"Unknown userid\n");
			exit(EX_OSFILE);
		}
		strcpy(status.localname,sn);
		/* prompt for login and passwd */
		envloginpasswd(remote,status.login,status.mpasswd);
		promptlogin(remote);
	}

	/* check to see that the lpr command is one of the approved set */
	if(strcmp(sLprCommand,"lpr") != 0
	&& strcmp(sLprCommand,"vpr") != 0
	&& strcmp(sLprCommand,"epr") != 0
	&& strcmp(sLprCommand,"bpr") != 0){
		fprintf(stderr,
			"May not execute the '%s' command from netlpr.\n",
			sLprCommand);
		exit(EX_USAGE);
	}

	do {
		if(fnoacct)
			sprintf(sCmdAct,"%s -c%s,%s %s",
			sLprCommand,status.jobno,status.localname,parms);
		else 	sprintf(sCmdAct,"%s %s",sLprCommand,parms);

		sprintf(tomachstr,"-m%c",remote);
		sprintf(realcmd,"netlpr %s%s",parms,(argc > 0 ? argv[0] : ""));

		while((pid = fork()) == -1)sleep(2);
		if(pid == 0){
			if(argc > 0){
				if(access(argv[0],04) == -1){
					perror(argv[0]);
					exit(EX_USAGE);
					}
# ifdef NOREMACCT
				if(fnoacct){
					setuid(0);
					mexecl(netcmd,"net",tomachstr,"-y",
					"-l",status.localname,"-u",suid,
					"-s",argv[0],"-c",realcmd,sCmdAct,0);
					perror(netcmd);
					}
				else
# endif
				{
					kexecl(netcmd,"net",tomachstr,
					"-s",argv[0],"-c",realcmd,sCmdAct,0);
					perror(netcmd);
					}
				}
			else {
# ifdef NOREMACCT
				if(fnoacct){
					setuid(0);
					mexecl(netcmd,"net",tomachstr,"-y","-b","-",
					"-l","root","-u",suid,
					"-c",realcmd,sCmdAct,0);
					perror(netcmd);
					}
				else
# endif
				{
					kexecl(netcmd,"net",tomachstr, "-",
					"-c",realcmd,sCmdAct,0);
					perror(netcmd);
					}
				}
			fprintf(stderr,"Network is down\n");
			exit(EX_UNAVAILABLE);
			}
		wait(&rcode);
		argc--, argv++;
		} while(argc > 0);
	exit(rcode >> 8);
	}

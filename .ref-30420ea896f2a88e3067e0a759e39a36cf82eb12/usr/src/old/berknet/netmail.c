static char sccsid[] = "@(#)netmail.c	4.1	(Berkeley)	%G%";

/* sccs id variable */
static char *netmail_sid = "@(#)netmail.c	1.2";
/*


 netmail [-c] [-l ...] [-p ...] [-f] [-n] [-q] ([mach] | [mach:username])

   Read mail on remote machine "mach"
   Sends a command to the remote machine to "mail" the mail
   to this machine.

   If the -c option is specified, this command is a mail check command,
   and in this mode it logs into the remote machine as "network"
   and determines if mach:username has mail.
   It writes/mails a message to that effect.
   This variant is intended to be used in .login files, silently
   checking if you have mail on another machine.

   Must duplicate effort that will be redone by the net command-
   the calls to commandfile and promptlogin are necessary
   to get a value for the login name to send to the prmail
   command on the other machine.
   May read the passwd file:
	1. Commandfile calls getenv(HOME) to get the home directory.
	   If not easily reached,....
	2. SnCurrent() calls getlogin(). If no entry in utmp file,
	   will read passwd file.
 */
# include "defs.h"

/* global variables */
struct userinfo status;

main(argc,argv)
  char **argv; {
	char *s;
	char machparm[BUFSIZ], fromaddress[BUFSIZ], fMailCheck = 0;
	char rcmd[BUFSIZ], fquiet = 0;
	debugflg = DBV;
	strcpy(rcmd,"netmail");
	argc--; argv++;
	while(argc > 0 && argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'b':	status.nonotify++; appss(rcmd,argv[0]); break;
		case 'c':	fMailCheck++;      appss(rcmd,argv[0]); break;
		case 'f':	status.force++;    appss(rcmd,argv[0]); break;
		case 'l':	harg(status.login); break;
		case 'n':	status.nowrite++;  appss(rcmd,argv[0]); break;
		case 'p': 	harg(status.mpasswd); break;
		case 'q':	fquiet = 1; 	   appss(rcmd,argv[0]); break;
		default:
			fprintf(stderr,
	"Usage: netmail [-l login] [-p password] [-c] [-f] [-n] [-q] [mach]\n");
			exit(EX_USAGE);
		}
		argc--, argv++;
		}
	if(argc > 0){
		if(FMemberSCh(argv[0],':'))
			remote = MchSFromAddr(status.login,argv[0]);
		else
			remote = lookup(argv[0]);
		if(remote == 0){
			fprintf(stderr,"Unknown machine %s\n",argv[0]);
			exit(EX_NOHOST);
		}
		appss(rcmd,argv[0]);
	}

	/* read the .netrc file to get a value for remote */
	/* will get status.login, passwd, and force for fetch variant */
	commandfile();
	if(remote == 0)remote = getremote(local);
	sprintf(machparm,"-m%c",remote);
		

	if(remote == local){
		fprintf(stderr,
		"Use the mail command to read your mail on this machine.\n");
		exit(EX_USAGE);
	}

/* read pw file, get local addr to send to prmail, store in status.localname */
	s = SnFromUid(getuid());
	if(s == NULL){
		fprintf(stderr,"Unknown local user\n");
		exit(EX_OSFILE);
	}
	strcpy(status.localname,s);
	sprintf(fromaddress,"%s:%s",longname(local),s);

	/* mail check variant */
	if(fMailCheck){
		if(status.login[0] == 0){
			fprintf(stderr,
			"Must supply a remote user name for mail check.\n");
			exit(EX_USAGE);
		}
		/* send mail check over, no passwd needed */
		if(fquiet)
			mexecl(netcmd,"net","-q",machparm,"-l","network",
			"-c",rcmd,
			PRMAIL,"-c","-l",status.login,"-f",fromaddress,0);
		else
			mexecl(netcmd,"net","-q",machparm,"-l","network",
			"-c",rcmd,
			PRMAIL,"-c","-l",status.login,"-f",fromaddress,"-k",0);
		perror(netcmd);
		fprintf(stderr,"Network is down\n");
		exit(EX_UNAVAILABLE);
	}

	/* mail forward variant */

	/* 
	   get name to send as parameter to prmail.
	   required for multiple login names with the same uid's
	   stored in status.login
	*/
	envloginpasswd(remote,status.login,status.mpasswd); /* look in env */
	promptlogin(remote);	/* prompt for name, passwd explicitely */

	if(fquiet)
		kexecl(netcmd,"net","-q",machparm,"-c",rcmd,PRMAIL,"-l",
			status.login,"-f",fromaddress,"-r",0);
	else
		kexecl(netcmd,"net","-q",machparm,"-c",rcmd,PRMAIL,"-l",
			status.login,"-f",fromaddress,"-r","-k",0);
	perror(netcmd);
	fprintf(stderr,"Network is down\n");
	exit(EX_UNAVAILABLE);
	}
/*
	append string sfrom to end of string sto, preceded by blank */
appss(sto,sfrom)
	register char *sto, *sfrom;
{
	strcat(sto," ");
	strcat(sto,sfrom);
}

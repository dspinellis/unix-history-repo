/*

 netmail [-l ...] [-p ...] [-f] [-n] [-r] [-c] ([mach] | [mach:username])

   Read mail on remote machine "mach"
   Sends a command to the remote machine to "mail" the mail
   to this machine.
   The -r option says remove any mail you find on the
   remote machine after sending it back to this machine.
   Uses the -q option of net, so is a quiet command.

   If the -c option is specified, this command is a mail check command,
   and in this mode it logs into the remote machine as "network"
   and determines if mach:username has mail.
   If so, it writes/mails a message to that effect.
   If not, it should be silent.
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

main(argc,argv)
  char **argv; {
	char *s;
	char machparm[BUFSIZ], fromaddress[BUFSIZ], fMailCheck = 0;
	char removemail=0, removestr[10], rcmd[BUFSIZ];
	debugflg = DBV;
	strcpy(rcmd,"netmail");
	argc--; argv++;
	while(argc > 0 && argv[0][0] == '-'){
		switch(argv[0][1]){
		case 'b':	status.nonotify++; appss(rcmd,argv[0]); break;
		case 'c':	fMailCheck++;      appss(rcmd,argv[0]); break;
		case 'f':	status.force++;    appss(rcmd,argv[0]); break;
		case 'l':	harg(status.login,&argc,&argv); break;
		case 'n':	status.nowrite++;  appss(rcmd,argv[0]); break;
		case 'p': 	harg(status.mpasswd,&argc,&argv); break;
		case 'q':	/* ignore */ break;
		case 'r':	removemail++;      appss(rcmd,argv[0]); break;
		default:
			fprintf(stderr,
	"Usage: netmail [-l login] [-p password] [-c] [-f] [-n] [-r] [mach]\n");
			exit(1);
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
			exit(1);
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
		exit(1);
	}

	/* get local address to send to prmail, store in status.localname */
	s = SnCurrent();
	if(s == NULL){
		fprintf(stderr,"Unknown local user");
		exit(1);
	}
	strcpy(status.localname,s);
	sprintf(fromaddress,"%s:%s",longname(local),s);

	/* mail check variant */
	if(fMailCheck){
		if(status.login[0] == 0){
			fprintf(stderr,
			"Must supply a remote user name for mail check.\n");
			exit(1);
		}
		/* send mail check over, no passwd needed */
		mexecl(netcmd,"net","-q",machparm,"-l","network","-c",rcmd,
		PRMAIL,"-c","-l",status.login,"-f",fromaddress,0);
		fprintf(stderr,"Network is down\n");
		exit(1);
	}

	/* mail forward variant */

	/* 
	   get name to send as parameter to prmail.
	   required for multiple login names with the same uid's
	   stored in status.login
	*/
	promptlogin(remote);	/* prompt for name, passwd explicitely */

	if(removemail)strcpy(removestr,"-r");
	else	      strcpy(removestr,"-z");
	kexecl(netcmd,"net","-q",machparm,"-c",rcmd,PRMAIL,"-l",
		status.login,"-f",fromaddress,removestr,0);
	fprintf(stderr,"Network is down\n");
	exit(1);
	}
/*
	append string sfrom to end of string sto, preceded by blank */
appss(sto,sfrom)
	register char *sto, *sfrom;
{
	strcat(sto," ");
	strcat(sto,sfrom);
}

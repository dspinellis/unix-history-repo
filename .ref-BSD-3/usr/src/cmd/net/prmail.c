/*
	prmail [-l username] [-f fromaddress] [-c] [-r]

	Print/forward mail on this machine to another machine.
	If no options specified, the current user's mail is printed
	on stdout and not removed.
	
Preferred usage (Has two modes):
	Mail check-
		A message is printed that there is mail.
		This is intended for people who put netmail -c addr
		in their .login files, and don't want to
		be prompted for their password.
		Mail check is indicated by a -c option.
	Mail forward-
		The mail is "mailed" to the fetcher.
		If the -r option is given, the mail is also
		appended to the mbox file and then removed.
		Forward is indicated by the lack of a -c option.

	Options:
		-l username	read username's mail
		-f fromaddress	forward mail to this address
		-r 		remove mail after forwarding
		-c		is a mail check, don't forward mail.
	
	Exit Codes:
		0 	on normal conditions
		101	if can't exec mail program
		102	if can't get uid/name
		103	if can't open mail file, once it is stat'ed.

*/

# include "defs.h"

/* 
  mail seems to reside in one of three places:
	1. the user's home directory/.mail
	2. /usr/mail/username
	3. /usr/spool/mail/username
  the conditional compilation flags for these three forms are:
	1. OLDMAIL
	2. USRMAIL
	3. is the default
*/
# ifdef USRMAIL
# define MAILDIR "/usr/mail"
# else
# define MAILDIR "/usr/spool/mail"
# endif

main(argc,argv)
	char **argv;
{			
	long ltimeMail;
	struct stat statbuf;
	FILE *f;
	char fn[BUFSIZ],buf[BUFSIZ],outbuf[BUFSIZ],*s,username[20],
		fromaddress[BUFSIZ],toaddress[BUFSIZ],rmcmd[BUFSIZ],removemail=0,
		fMailCheck=0, *stimeMail;
	int i,ret,pid;
	fromaddress[0] = 0;
	username[0] = 0;
	setbuf(stdout,outbuf);
	while(argc > 1){
		argc--, argv++;
		if(argv[0][0] == '-'){
			switch(argv[0][1]){
			case 'c': fMailCheck++; break;
			case 'f': harg(fromaddress,&argc,&argv); break;
			case 'l': harg(username,&argc,&argv); break;
			case 'r': removemail++; break;
			/* it is important to ignore unknown flags for
			   compatibilty reasons */
			}
		}
	}

	/* get the name of the user who's mail we're reading */
	if(username[0] == 0){
		s = SnCurrent();
		if(s == NULL){
			fprintf(stderr,"Unknown user\n");
			exit(102);
			}
		strcpy(username,s);
	}

# ifdef OLDMAIL
	/* handle mail directory in user's directory */
	sprintf(fn,"%s/.mail",getenv("HOME"));
# else
	sprintf(fn,"%s/%s",MAILDIR,username);
# endif
	sprintf(toaddress,"%s:forward", longname(local));
	if(fromaddress[0] != 0){

		/* don't send  anything back if nothing to send */
		if(stat(fn,&statbuf) < 0 || getsize(&statbuf) == 0L) exit(0);

		/* if a mail check, print message and exit */
		if(fMailCheck){
			ltimeMail = statbuf.st_mtime;
			stimeMail = ctime(&ltimeMail);
			stimeMail[strlen(stimeMail) - 6] = 0;
			printf(
	"\"%s\" has mail on the %s machine.   \nLast updated on %s.   \n",
				username,longname(local),stimeMail);
			printf("File %s:%s, Length %ld characters.   \n",
				longname(local),fn,getsize(&statbuf));
			exit(0);
		}

		/* read the mail and mail it to the account asking for it */
		f = freopen(fn,"r",stdin);
		if(f == NULL){
			perror(fn);
			exit(103);
		}
		while((pid = fork()) == -1);
		if(pid == 0){
			/* send to the person who sent this to us */ 
			/* system mail must know about remote mailing */
			mexecl(SYSMAIL1,"mail", fromaddress, 0);
			mexecl(SYSMAIL2,"mail", fromaddress, 0);
			mexecl(SYSMAIL3,"mail", fromaddress, 0);
			exit(101);
		}
		/* parent */
		fclose(stdin);
		wait(&ret);
		ret >>= 8;
		if(removemail){
			if(ret == 0){
				ret = RcAppendMail(fn,username);
				if(ret == 0){
					sprintf(rmcmd,"rm -f %s",fn);
					ret = system(rmcmd);
					ret >>= 8;
				}
			}
			if(ret != 0)fprintf(stderr,"Mail not removed\n");
		}
		exit(ret);
		}

	/* this [archaic] section if forwarding address not specified */
	if(stat(fn,&statbuf) < 0 || getsize(&statbuf) == 0L){
		printf("No mail.\n");
		exit(0);
		}
	f = fopen(fn,"r");
	if(f == NULL){
		perror(fn);
		exit(1);
		}
	while((i = fread(buf,1,BUFSIZ,f)) > 0)
		fwrite(buf,1,i,stdout);
	fclose(f);
	exit(0);
	}
/*
	RcAppendMail(fnFrom) returns a return code

	Copy mail from fnFrom to the end of the mbox file in the user's
	home directory.
	Returns 1 if error, 0 if ok.
	Can't use getenv() because if there's no entry in utmp
	for machines with multiple names per uid, the getenv() will
	return the homedir of the first name/uid pair it finds.
*/
RcAppendMail(fnFrom,sn)
	char *fnFrom;
	char *sn;
{
	FILE *fdFrom, *fdTo;
	char *shdir, fnTo[BUFSIZ], sBuf[BUFSIZ];
	int nchar;

# ifdef MULTNAMS
	struct passwd *pwd;

	pwd = getpwnam(sn);
	if(pwd == NULL)return(1);
	shdir = pwd->pw_dir;
# else
	shdir = getenv("HOME");
	if(shdir == NULL)return(1);
# endif
	sprintf(fnTo,"%s/mbox",shdir);
	fdTo = fopen(fnTo,"a");
	if(fdTo == NULL){
		perror(fnTo);
		return(1);
	}

	fdFrom = fopen(fnFrom,"r");
	if(fdFrom == NULL){
		perror(fdFrom);
		return(1);
	}

	while((nchar = fread(sBuf,1,BUFSIZ,fdFrom)) > 0){
		if(fwrite(sBuf,1,nchar,fdTo) != nchar){
			perror(fnTo);
			return(1);
		}
	}
	fclose(fdFrom);
	fclose(fdTo);
	return(0);
}

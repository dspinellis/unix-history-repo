static char sccsid[] = "@(#)prmail.c	4.1	(Berkeley)	%G%";

/*
	prmail -f fromaddress [-l username] [-c] [-k]

	Print/forward mail on this machine to another machine.
	
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
		-c		is a mail check, don't forward mail.
		-k		print "No Mail" for all to see
	
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

char _sobuf[BUFSIZ];
main(argc,argv)
	char **argv;
{			
	long ltimeMail;
	struct stat statbuf;
	struct passwd *pwd;
	FILE *f;
	char fn[BUFSIZ],*s,username[20],
		fromaddress[BUFSIZ],toaddress[BUFSIZ],
		fMailCheck=0, *stimeMail,fquiet = 1;
	int ret,pid;
	fromaddress[0] = 0;
	username[0] = 0;
	setbuf(stdout,_sobuf);
	while(argc > 1){
		argc--, argv++;
		if(argv[0][0] == '-'){
			switch(argv[0][1]){
			case 'c': fMailCheck++; break;
			case 'f': harg(fromaddress); break;
			case 'k': fquiet = 0; break;
			case 'l': harg(username); break;
			/* it is important to ignore unknown flags for
			   compatibilty reasons */
			}
		}
	}

	/* get the name of the user who's mail we're reading */
	if(username[0] == 0){
		s = SnFromUid(getuid());
		if(s == NULL){
			fprintf(stderr,"Unknown user\n");
			exit(EX_OSFILE);
			}
		strcpy(username,s);
	}

# ifdef OLDMAIL
	/* handle mail directory in user's directory */
	/* can't do getenv because may be logging in as "network" */
	pwd = getpwnam(username);
	if(pwd == NULL){
		fprintf(stderr,"no passwd file\n");
		exit(EX_OSFILE);
	}
	sprintf(fn,"%s/.mail",pwd->pw_dir);
# else
	sprintf(fn,"%s/%s",MAILDIR,username);
# endif
	sprintf(toaddress,"%s:%s", longname(local),username);
	if(fromaddress[0] == 0){
		fprintf(stderr,"Need a From Address\n");
		exit(EX_USAGE);
	}

	/* don't send  anything back if nothing to send */
	if(stat(fn,&statbuf) < 0 || getsize(&statbuf) == 0L) {
		if(!fquiet)
			printf("No mail for %s on the %s machine.\n",
				username,longname(local));
		exit(EX_OK);
	}

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
		exit(EX_OK);
	}

	/* read the mail and mail it to the account asking for it */
	/* send mail to "fromaddress", as from "toaddress" */
	ret = mailmail(fn,fromaddress);
	if(ret == 0){
		ret = RcAppendMail(fn,username);
		if(ret == 0){
# ifndef OLDMAIL
			ret = unlink(fn);
			if(ret < 0)
# endif
				ret = creat(fn,0644);
			if(ret >= 0)close(ret);
		}
	}
	if(ret < 0)fprintf(stderr,"Mail not removed\n");
	exit(ret);
}
/* mail contents of file fn to user "toaddress" */
/* read file and mail each message separately */
/* returns return code of executing the mail prorgam */
mailmail(fn,toaddress)
char *fn, *toaddress;
{
	FILE *fdfile, *fdcmd;
	FILE *mailopen();
	char line[BUFSIZ];
	int ret;
	int more;

	fdfile = fopen(fn,"r");
	if(fdfile == NULL){
		perror(fn);
		exit(EX_DATAERR);
	}
	more = 1;
	line[0] = 0;
	while(more){
		fdcmd = mailopen(toaddress,NULL,1,0);
		if(fdcmd == NULL){
			perror("mail command");
			exit(EX_UNAVAILABLE);
		}
		/* read line with from on it */
		if(line[0] == 0)fgets(line,BUFSIZ,fdfile);
		/* insert a > before the first from line */
		fprintf(fdcmd,">%s",line);
		more = 0;
		while(fgets(line,BUFSIZ,fdfile) != NULL){
			if(strncmp(line,"From ",5) == 0){
				more++;
				break;
			}
			fputs(line,fdcmd);
		}
		ret = mailclose(fdcmd);
		ret >>= 8;
		if(ret != 0){
			fprintf(stderr,
			"Non-zero return code (%d) from the mail program\n",
				ret);
			break;
		}
	}
	fclose(fdfile);
	return(ret);
}

/*
	RcAppendMail(fnFrom) returns a return code

	Copy mail from fnFrom to the end of the mbox file in the user's
	home directory.
	Returns -1 if error, 0 if ok.
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
	if(pwd == NULL)return(-1);
	shdir = pwd->pw_dir;
# else
	shdir = getenv("HOME");
	if(shdir == NULL)return(-1);
# endif
	sprintf(fnTo,"%s/mbox",shdir);
	fdTo = fopen(fnTo,"a");
	if(fdTo == NULL){
		perror(fnTo);
		return(-1);
	}

	fdFrom = fopen(fnFrom,"r");
	if(fdFrom == NULL){
		perror(fdFrom);
		return(-1);
	}

	while((nchar = fread(sBuf,1,BUFSIZ,fdFrom)) > 0){
		if(fwrite(sBuf,1,nchar,fdTo) != nchar){
			perror(fnTo);
			return(-1);
		}
	}
	fclose(fdFrom);
	fclose(fdTo);
	return(0);
}

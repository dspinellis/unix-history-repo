static char sccsid[] = "@(#)v6mail.c	4.4	(Berkeley)	1/9/83";

/*
 * Version 6 Cory mail--
 *	a clean and simple mail program
 *	machine and version independent
 *		Eric Schmidt
 *	must run as setuid root to chown the destination mailbox
 *	if NOTROOT defined, doesn't need to run as root
 *	
 *	DON'T CHANGE THIS CODE
 *		bitch to "csvax:schmidt" instead
 */

/*
 * mail command usage
 *	mail [-yn]
 *		prints your mail
 *	mail people
 *		sends standard input to people
 *
 *	mail -r fromaddr people
 *		sends mail from the network
 *
 *	mail -d people
 *		don't call sendmail, send mail directly
 *	mail msgs
 *		send to "msgs"
 *	mail filename
 *		mail to filename instead of user (must be at least one /)
 *	mail -D
 *		delete the invokers mailbox (more efficient than 
 *			mail -n >/dev/null)
 */

/*
 *	bugs:
 *		Ingres 11/70 multiple names/uid?
 *	additions:
 *		Save? type 'x' - doesn't unlink the mail file
 */
/*
 * BIFF is an immediate notification flag using the IPC stuff
 */
# include "local.h"
# include <stdio.h>
# include "mach.h"

# ifdef RAND

/* for all machines at RAND */
# define MAILMODE 0644

# endif RAND

# ifdef NOSC

/* for all machines at NOSC */
# define MAILMODE 0644

# endif NOSC

# ifdef BERKELEY
/* for Berkeley */
/* for each machine */
/* lump the CC machines into one */
# ifdef CCV7
# define MAILMODE 0600
# define MSGSCMD "/usr/ucb/bin/msgs"
# endif

# ifdef CCV6
# define MSGSCMD "/usr/bin/eecs/msgs"
# define MAILMODE 0600
# endif

# ifdef ING70
# define MAILMODE 0666
# define MSGSCMD "/usr/bin/msgs"
# define NOTROOT
# endif

# ifdef INGVAX
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/msgs"
# endif

/*
# ifdef VIRUS
# define MAILMODE 0644
# define MSGSCMD "/usr/bin/msgs"
# endif
*/

# ifdef UCBVAX
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/msgs"
# define BIFF
# endif

# ifdef IMAGE
# define MAILMODE 0644
# define MSGSCMD "/usr/bin/msgs"
# endif

# ifdef KIM
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/msgs"
# endif

# ifdef ESVAX
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/msgs"
# endif

# ifdef Q
# define MAILMODE 0600
# define MSGSCMD "/usr/bin/eecs/msgs"
# endif

# ifdef ARPAVAX
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/msgs"
# define BIFF
# endif

# ifdef SRC
# define MAILMODE 0600
# define MSGSCMD "/usr/bin/msgs"
# endif

# ifdef MATHSTAT
# define MAILMODE 0600
# define MSGSCMD "/usr/bin/msgs"
# endif

# ifdef CSVAX
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/msgs"
# define BIFF
# endif

# ifdef ONYX
# define MAILMODE 0644
# define MSGSCMD "/usr/ucb/bin/msgs"
# endif

# ifdef CORY
# define MAILMODE 0600
# define MSGSCMD "/usr/bin/eecs/msgs"
# endif

# ifdef EECS40
# define MAILMODE 0644
# define MSGSCMD "/usr/bin/msgs"
# endif
/* end of berkeley defsn */

# endif
/* end of per-machine ifdefs */

# ifdef USRMAIL
# define MAILDIR "/usr/mail"
# else
# define MAILDIR "/usr/spool/mail"
# endif

char	lettmp[]  = "/tmp/MaXXXXX";	/* keep letter before sending it */
char	preptmp[] = "/tmp/mbXXXXX";	/* if prepending msg, use this file */
int	chew;				/* if true, strip extra from lines */
int	dflag;				/* if true, don't call sendmail */
char	shopcnt[30] = "0";		/* hop count parameter for rmt mail */
int	errs;				/* no of errs in sending */
char	deleteonly;			/* if true, just delete mailbox */
char 	remname[50];			/* if non-empty, from line extra */

char _sobuf[BUFSIZ];
main(argc, argv)
char **argv;
{
	register int myuid;
	int delexit();
	char namebuf[128], *sn = NULL, logindir[60];
	struct passwd *pwd;

	setbuf(stdout,_sobuf);
	mktemp(lettmp);
	mktemp(preptmp);
	unlink(lettmp);
	unlink(preptmp);
	myuid = getuid();
	logindir[0] = 0;
	sn = getlogin();
	if(sn == NULL || *sn == 0 || *sn == ' '){
		pwd = getpwuid(myuid);		/* will read passwd file */
		if(pwd != NULL){
			sn = pwd->pw_name;
			strcpy(logindir,pwd->pw_dir);
		}
		if(sn == NULL){
			fprintf(stderr,"Who are you?\n");
			delexit(EX_OSFILE);
		}
	}
	strcpy(namebuf,sn);
	if (argc < 2)
		goto hitit;
	for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++)
	switch(argv[0][1]) {
		case 'y':
		case 'n':
			argc++, argv--;
hitit:
			printmail(argc, argv, namebuf,logindir);
			delexit(EX_OK);

		case 'r':	/* one-arg -r--   -r addr */
			if (argc < 2)
				continue;
			/* ignore -r if not network or root */
			if (strcmp("network", namebuf) == 0 || myuid == 0 ||
			    strcmp("uucp", namebuf) == 0 || index(argv[1], '!') != NULL) {
				strcpy(namebuf,argv[1]);
				chew++;		/* eat From lines */
			}
			else strcpy(remname, argv[1]);
			argc--, argv++;
			continue;
		case 'h':	/* hop count - used by network */
			if(argc < 2) continue;
			strcpy(shopcnt,argv[1]);
			argc--, argv++;
			continue;
		case 'd':	/* really deliver this message */
			dflag++;
			continue;
		case 'D':	/* only delete the invokers mailbox */
			deleteonly++;
			goto hitit;		/* delete mail box, thats all */
	}
	/* if we are already ignoring signals, catch sigint */
	if(signal(SIGINT,SIG_IGN) != SIG_IGN)
		signal(SIGINT, delexit);
	argc++, argv--;
	bulkmail(argc, argv, namebuf);
	delexit(EX_OK);
}

printmail(argc, argv, name, logindir)
char **argv;
char *name, *logindir;
{
	register int c;
	FILE *fdin;
	char sfnmail[60], mbox[120];
	struct stat statbuf;

# ifdef OLDMAIL
	if(logindir[0] == 0){
		pwd = getpwuid(getuid());
		if(pwd == NULL){
			fprintf(stderr,"Can't get directory\n");
			exit(EX_OSFILE);
		}
		strcpy(logindir, pwd->pw_dir);
	}
	sprintf(sfnmail,"%s/.mail",logindir);
# else
	sprintf(sfnmail,"%s/%s",MAILDIR,name);
# endif
	if(deleteonly){
		remove(sfnmail);
		return;
	}
	if (stat(sfnmail, &statbuf)>=0 && statbuf.st_nlink==1 &&
		getsize(&statbuf) > 0L && (fdin = fopen(sfnmail, "r")) != NULL){
		getput(fdin, stdout);
		fclose(fdin);
		fflush(stdout);
		c = 'y';
		if (argc<2) {
			if(isatty(0)){
				printf("Save(y-n) ?");
				fflush(stdout);
				c = getchar();
			}
		} else
			c = argv[1][1];
		if (!any(c, "xyn"))
			delexit(EX_OK);
		if (c == 'y') {
			sprintf(mbox,"%s/mbox",logindir);
			if (accesss(mbox)) {
				printf("Saved mail in 'mbox'\n");
				if(insert(sfnmail, mbox, getuid(),getgid()))
					remove(sfnmail);
			}
			else printf("In wrong directory\n");
		}
		else if(c != 'x') remove(sfnmail);
	} else
		printf("No mail.\n");
}

bulkmail(argc, argv, from)
char **argv, *from;
{
	extern int errno;
	char linebuf[BUFSIZ];
	char *getdate();
	FILE *fdout;

# ifdef SENDMAIL
	/*
	**  Ship off to sendmail if appropriate (and possible)
	*/

	if (!dflag)
	{
		argv[0] = "-sendmail";
		argv[argc] = 0;
		execv("/usr/lib/sendmail", argv);
		/* oops...  better just deliver it. */
		fprintf(stderr, "Not using sendmail\n");
		errno = 0;
		argv[argc] = (char *)-1;
	}
# endif

	fdout = fopen(lettmp, "w");
	if (fdout == NULL) {
		perror(lettmp);
		delexit(EX_OSFILE);
	}
	/*
	 * If delivering mail from the network via mail -r,
	 * Strip the leading line and throw it away, as long
	 * as it begins with "From ..." (and preserve the date if poss.)
	 */
	if (chew) {
		fgets(linebuf,BUFSIZ,stdin);
		if(strncmp(linebuf,"From ",5) != 0){
			fline(fdout,NULL,from);
			fprintf(fdout,"%s", linebuf);
		}
		else fline(fdout,getdate(linebuf),from);
	}
	else fline(fdout,NULL,from);
	if(remname[0]) fprintf(fdout,"(from %s)\n",remname);

	/* on the image machine, promt with subj */
	if(getput(stdin,fdout) == 0)
		delexit(EX_OSERR);
	putc('\n',fdout);
	fclose(fdout);
	while (--argc > 0)
		sendto(*++argv,from);
	delexit(errs);
}
/* print from line, with date date, if date = NULL, compute it */
fline(fdout,date,from)
FILE *fdout;
char *date;
char *from;
{
	int tbuf[2];

	if(date == NULL){
		time(tbuf);
		date = ctime(tbuf);
	}
	fprintf(fdout,"From %s  %s", from, date);
}
/* look over linebuf and return ptr to date, NULL if error */
char *getdate(linebuf)
char *linebuf;
{
	register char *s;
	s = linebuf;
	while(*s){
		if(strncmp(s," Sun ",5) == 0
		|| strncmp(s," Mon ",5) == 0
		|| strncmp(s," Tue ",5) == 0
		|| strncmp(s," Wed ",5) == 0
		|| strncmp(s," Thu ",5) == 0
		|| strncmp(s," Fri ",5) == 0
		|| strncmp(s," Sat ",5) == 0)
			return(++s);
		s++;
	}
	return(NULL);
}

sendto(person, fromaddr)
char *person;
char *fromaddr;
{
	static int saved = 0;
	register int hisuid, hisgid;
	char sfnmail[60], logindir[60];
	struct passwd *pwd;

	stripmach(&person);
	if(person[0] == ':')person++;
	/* sendmail provides these services */
	if(any(':',person)
# ifdef MSGSCMD
		|| strcmp(person,"msgs") == 0
# endif
		){
		int pid;
		int pidchild;

		while((pid = fork()) == -1)sleep(2);
		if (pid < 0) {
			perror("fork");
			goto assback;
		}
		if (pid == 0) {
			fclose(stdin);
			freopen(lettmp,"r",stdin);
			setuid(getuid());	/* insure no security hole*/
			if (strcmp(person,"msgs") != 0) {
				/* 
				sendberkmail will add the machine, e.g.
				CSVAX:schmidt, if the -f flag is not set
				*/
				execl("/usr/net/bin/sendberkmail",
				"sendberkmail", "-t",person,"-h",shopcnt,
				chew ? "-f" : 0,fromaddr,0);
				perror("/usr/net/bin/sendberkmail");
			}
# ifdef MSGSCMD
			else {
				execl(MSGSCMD, "msgs", "-s", 0);
				perror(MSGSCMD);
			}
# endif
			exit(EX_UNAVAILABLE);
		}
		for (;;) {
			register int rcode = wait(&pidchild);
			if (rcode == -1)
				goto assback;
			if (rcode == pid)
				break;
		}
		if ((pidchild & 0377) != 0 || (pidchild >> 8) != 0)
			goto assback;
		return;
	}

	if(!any('/',person)){
	/* if name has no / in it, we assume it is a user's name */
# ifdef HPASSWD
		hisuid = uidfromsn(person);
# else
		pwd = getpwnam(person);
		if(pwd != NULL){
			hisuid = guid(pwd->pw_uid,pwd->pw_gid);
			hisgid = pwd->pw_gid;
			strcpy(logindir,pwd->pw_dir);
		}
		else hisuid = -1;
# endif
		if(hisuid == -1){
	assback:
			fflush(stdout);
			fprintf(stderr,"Can't send to %s.\n", person);
			errs++;
			if (isatty(0) && saved==0) {
				saved++;
				if (accesss("dead.letter")) {
					printf("Letter saved in 'dead.letter'\n");
					insert(lettmp, "dead.letter",
						getuid(),getgid());
				} else
					printf("In wrong directory\n");
			}
			return;
		}
# ifdef OLDMAIL
		sprintf(sfnmail,"%s/.mail",logindir);
# else
		sprintf(sfnmail,"%s/%s",MAILDIR,person);
# endif
		lock(sfnmail);
		insert(lettmp, sfnmail, hisuid, hisgid);
		unlock();
		}
	else {		/* it has / in it, "person" is a file */
		if(accesss(person)){
			lock(person);
			insert(lettmp, person, -1, -1);
			unlock();
		}
		else
			fprintf(stderr,"Can't access %s\n",person);
	}
}

/* return 1 if success, 0 otherwise */
insert(from, to, uid, gid)
char *from, *to;
{
# ifdef V6
	return(prepend(from,to,uid, gid));
# else
	return(append(from,to,uid, gid));
# endif
}

#ifdef BIFF
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif

/* return 1 if success, 0 otherwise */
append(from,to,uid, gid)
char *from, *to;
{
	register FILE *fdin, *fdout;
	int ret;
	struct stat statbuf;
#ifdef BIFF
	char *cp, buf[100], *rindex();
	int s;
	struct hostent *hp;
	struct sockaddr_in sin;
	struct servent *sp;
#endif
	if (stat(to, &statbuf) >= 0 && (statbuf.st_mode&S_IFDIR) != 0) {
		fprintf(stderr, "Exotic destination %s\n", to);
		errs++;
		return(0);
	}
	if ((fdout = fopen(to, "a")) == NULL) {
		perror(to);
		errs++;
		return(0);
	}
# ifndef NOTROOT
	if(uid != -1)mchown(to, uid, gid);
# endif
	if(uid != -1)chmod(to, MAILMODE);
	if ((fdin = fopen(from, "r")) == NULL) {
		perror(from);
		return(0);
	}
#ifdef BIFF
	{ s = socket(AF_INET, SOCK_STREAM, 0, 0);
	  cp = rindex(to, '/');
	  if (cp) 
		sprintf(buf, "%s@%d\n", cp+1, ftell(fdout));
	}
#endif
	ret = getput(fdin,fdout);
	fclose(fdin);
	fclose(fdout);
#ifdef BIFF
	if (cp && s >= 0) {
		hp = gethostbyname("localhost");
		sp = getservent("biff", "udp");
		if (hp && sp) {
			bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
			sin.sin_family = hp->h_addrtype;
			sin.sin_port = sp->s_port;
			(void) sendto(s, buf, strlen(buf) + 1, 0,
				&sin, sizeof (sin));
		}
		close(s);
	}
#endif
	return(ret);
}

/* return 1 if success, 0 otherwise */
prepend(from, to, uid, gid)
char *from, *to;
{
	register int (*sig)();
	struct stat statbuf;
	FILE *fdout, *fdin;
	int ret;

	if (stat(to, &statbuf) >= 0 && (statbuf.st_mode&S_IFDIR) != 0) {
		fprintf(stderr, "Exotic destination %s\n", to);
		goto badexit;
	}
	unlink(preptmp);
	if ((fdout = fopen(preptmp, "w")) == NULL) {
		perror("mail");
		goto badexit;
	}
	chmod(preptmp, MAILMODE);
	if ((fdin = fopen(from, "r")) == NULL) {
		perror("mail");
		goto badexit;
	}
	if(getput(fdin,fdout) == 0){
		perror("file i/o");
		goto badexit;
	}
	fclose(fdin);
	fdin = fopen(to, "r");
	/* ignore error since may not exist */
	if(fdin != NULL && getput(fdin,fdout) == 0){
		perror("file i/o");
		goto badexit;
	}
	if(fdin != NULL)fclose(fdin);
	fclose(fdout);
	sig = signal(SIGINT, SIG_IGN);
	remove(to);
	if ((fdout = fopen(to, "w")) == NULL) {
		perror(to);
		unlink(preptmp);
		signal(SIGINT, sig);
		goto badexit;
	}
# ifdef NOTROOT
	if(uid != -1)chmod(to,0666);
# else
	if(uid != -1)mchown(to, uid, gid);
# endif
	if(stat(to, &statbuf) < 0 || statbuf.st_nlink != 1) {
		fclose(fdout);
		signal(SIGINT, sig);
		goto badexit;
	}
	if ((fdin = fopen(preptmp, "r"))  == NULL) {
		perror("mail");
		signal(SIGINT, sig);
		goto badexit;
	}
	ret = getput(fdin,fdout);
	fclose(fdout);
	fclose(fdin);
	signal(SIGINT, sig);
	return(ret);
badexit:
	unlink(preptmp);
	errs++;
	return(0);
}

delexit(ex)
{
	unlink(lettmp);
	unlink(preptmp);
	exit(ex);
}

/* return 1 if ok, 0 otherwise */
getput(fdin, fdout)
register FILE *fdin, *fdout;
{
	extern int errno;
	register int c;

	while((c = getc(fdin)) != EOF) {
		errno = 0;
		putc(c,fdout);
		if(errno) {
			perror("mail");
			return(0);
		}
	}
	return(1);
}

accesss(s1)
register char *s1;
{
	struct stat statbuf;
		if(stat(s1,&statbuf)<0 || access(s1,2) == 0)
		return(1);
	return(0);
}

any(c, str)
	register char *str, c;
{
	register char *f;

	f = str;
	while (*f)
		if (c == *f++)
			return(1);
	return(0);
}
char	locktmp[30];				/* Usable lock temporary */
char	curlock[50];				/* Last used name of lock */
int	locked;					/* To note that we locked it */

/*
 * Lock the specified mail file by setting the file mailfile.lock.
 * We must, of course, be careful to unlink the lock file by a call
 * to unlock before we stop.  The algorithm used here is to see if
 * the lock exists, and if it does, to check its modify time.  If it
 * is older than 30 seconds, we assume error and set our own file.
 * Otherwise, we wait for 5 seconds and try again.
 */

lock(file)
char *file;
{
	register int f;
	struct stat statbuf;
	long curtime;
/* 
   if using OLDMAIL, and NOTROOT, cann't lock since can't necessarily
   write on user's login directory
*/
# ifdef OLDMAIL
	return;
# endif

	if (file == NULL) {
		printf("Locked = %d\n", locked);
		return(0);
	}
	if (locked)
		return(0);
	sprintf(curlock,"%s%s",file,".lock");
	sprintf(locktmp,"%s/tmXXXXXX",MAILDIR);
	mktemp(locktmp);
	unlink(locktmp);
	for (;;) {
		f = lock1(locktmp, curlock);
		if (f == 0) {
			locked = 1;
			return(0);
		}
		if (stat(curlock, &statbuf) < 0)
			return(0);
		time(&curtime);
		if (curtime < statbuf.st_mtime + 30) {
			sleep(5);
			continue;
		}
		unlink(curlock);
	}
}

/*
 * Remove the mail lock, and note that we no longer
 * have it locked.
 */

unlock()
{

	if (locked)
		unlink(curlock);
	locked = 0;
}

/*
 * Attempt to set the lock by creating the temporary file,
 * then doing a link/unlink.  If it fails, return -1 else 0
 */

lock1(tempfile, name)
	char tempfile[], name[];
{
	int fno;

	fno = creat(tempfile, 0400);
	if (fno < 0)
		return(-1);
	close(fno);
	if (link(tempfile, name) < 0) {
		unlink(tempfile);
		return(-1);
	}
	unlink(tempfile);
	return(0);
}

/*
	stripfx(prefix string, pointer to string)

	takes a ptr to string and compares it to prefix string.
	may be called multiple times
	returns ":username"
*/
stripfx(pfx, name)
	register char *pfx;
	register char **name;
{
	register char *cp = *name;

	while (*pfx && (*cp == *pfx || *cp == toupper(*pfx)))
		cp++, pfx++;
	if (*cp != ':' || *pfx != 0)
		return;
	*name = cp;
}
stripmach(pperson)
register char **pperson;
{
# ifdef RAND
/* for machines at RAND */
# ifdef GRAPHICS
	stripfx("g",pperson);
	stripfx("graphics",pperson);
# endif
# ifdef TP
	stripfx("t",pperson);
	stripfx("tp",pperson);
# endif
# ifdef VAX
	stripfx("v",pperson);
	stripfx("vax",pperson);
# endif
/* end of defns for Rand */
# endif

# ifdef NOSC
/* for machines at NOSC */
# ifdef ATTS
	stripfx("a",pperson);
	stripfx("atts",pperson);
# endif
# ifdef CCMM
	stripfx("c",pperson);
	stripfx("ccmm",pperson);
# endif
# ifdef MSSF
	stripfx("m",pperson);
	stripfx("mssf",pperson);
# endif
/* end of defns for NOSC */
# endif

# ifdef BERKELEY

/* for Berkeley */
# ifdef A
	stripfx("a",pperson);
# endif
# ifdef B
	stripfx("b",pperson);
# endif
# ifdef C
	stripfx("c",pperson);
# endif
# ifdef D
	stripfx("d",pperson);
# endif
# ifdef E
	stripfx("e",pperson);
# endif
# ifdef ING70
	stripfx("i",pperson);
	stripfx("ing70",pperson);
	stripfx("ingres",pperson);
# endif
# ifdef INGVAX
	stripfx("j",pperson);
	stripfx("ingvax",pperson);
# endif
# ifdef VIRUS
	stripfx("k",pperson);
	stripfx("virus",pperson);
# endif
# ifdef IMAGE
	stripfx("m",pperson);
	stripfx("image",pperson);
# endif
# ifdef KIM
	stripfx("n",pperson);
	stripfx("kim",pperson);
# endif
# ifdef ESVAX
	stripfx("o",pperson);
	stripfx("esvax",pperson);
# endif
# ifdef Q
	stripfx("q",pperson);
# endif
# ifdef ARPAVAX
	stripfx("r",pperson);
	stripfx("arpavax",pperson);
# endif
# ifdef SRC
	stripfx("s",pperson);
	stripfx("src",pperson);
# endif
# ifdef MATHSTAT
	stripfx("t",pperson);
	stripfx("mathstat",pperson);
# endif
# ifdef CSVAX
	stripfx("v",pperson);
	stripfx("vax",pperson);
	stripfx("csvax",pperson);
# endif
# ifdef CORY
	stripfx("y",pperson);
	stripfx("cory",pperson);
# endif
# ifdef EECS40
	stripfx("z",pperson);
	stripfx("eecs40",pperson);
# endif
/* end of berkeley defns */
# endif
}
/* 
   this removes the mail file sfn by either truncating it, as required
   on OLDMAIL systems, or unlinking it. If the unlink fails, we truncate it.
*/
remove(sfn)
char *sfn;
{
	int i;
# ifdef OLDMAIL
	i = creat(sfn,0666);
	if(i >= 0)close(i);
# else
	if(unlink(sfn) < 0){
		i = creat(sfn,MAILMODE);
		if(i >= 0)close(i);
	}
# endif
}

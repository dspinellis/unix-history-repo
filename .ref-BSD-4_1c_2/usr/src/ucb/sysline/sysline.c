static char rcsid[]
  = "$Header: /usr/src/ucb/sysline/RCS/sysline.c,v 1.2 83/02/08 16:20:50 jkf Exp $";

/*
 * sysline - system status display on 25th line of terminal
 * j.k.foderaro
 *
 * Prints a variety of information on the special status line of terminals
 * that have a status display capability.  Cursor motions, status commands,
 * etc. are gleamed from /etc/termcap.
 * By default, all information is printed, and flags are given on the command
 * line to disable the printing of information.  The information and
 * disabling flags are:
 *
 *  flag	what
 *  -----	----
 *		time of day
 *		load average and change in load average in the last 5 mins
 *		number of user logged on
 *   -p		# of processes the users owns which are runnable and the
 *		  number which are suspended.  Processes whose parent is 1
 *		  are not counted.
 *   -l		users who've logged on and off.
 *   -m		summarize new mail which has arrived
 * 
 *  <other flags>
 *   -r		use non reverse video
 *   -c		turn off 25th line for 5 seconds before redisplaying.
 *   -b		beep once one the half hour, twice on the hour
 *   +N		refresh display every N seconds.
 *   -i		print pid first thing
 *   -e		do simple print designed for an emacs buffer line
 *   -h		print hostname between time and load average
 *   -D		print day/date before time of day
 *   -d		debug mode - print status line data in human readable format
 *   -q		quiet mode - don't output diagnostic messages
 *   -s		print Short (left-justified) line if escapes not allowed
 *   -j		Print left Justified line regardless
 *
 */

/* turn this on always */
#define WHO

/* turn this on if you are on running 4.1a or greater (i.e. a system
   with the gethostname() function */
#define HOSTNAME

/* turn this on if you are running on vmunix */
#define VMUNIX

/* turn this on if you are running on 4.1c or greater */
# define NEW_BOOTTIME 

#include <sys/param.h>
#include <signal.h>
#include <stdio.h>
#include <utmp.h>
#include <ctype.h>
#include <unctrl.h>
#include <time.h>
#include <sys/stat.h>
#ifdef VMUNIX
#include <nlist.h>
#include <sys/vtimes.h>
#include <sys/proc.h>
#endif
#ifdef pdp11
#include <a.out.h>
#include <sys/proc.h>
#define strcmpn strncmp
#endif
#ifdef TERMINFO
#include <curses.h>
#include <term.h>
#endif

#ifdef VMUNIX
#define MAXUSERS 100
#else
#define MAXUSERS 40
#endif
#define DEFDELAY 60	/* update status once per minute */
#define MAILDIR "/usr/spool/mail"

/* if MaxLoad is defined, then if the load average exceeded MaxLoad
 * then the process table will not be scanned and the log in/out data
 * will not be checked.   The purpose of this is to reduced the load
 * on the system when it is loaded.
 */
#define MaxLoad 6.0


struct nlist nl[] =
#ifdef NEW_BOOTTIME
  { { "_boottime" },	/* After 4.1a the label changed to "boottime" */
#else
  { { "_bootime" },	/* Under 4.1a and earlier it is "bootime" */
#endif
#define	NL_BOOT 0
    { "_proc" },
#define NL_PROC 1
    { "_avenrun" },
#define NL_AVEN 2
#ifdef VMUNIX
    { "_nproc" },
#define NL_NPROC 3
#endif
    { 0 }};

struct proc *pr;
int nproc;
int procadr;

double avenrun[3];	/* used for storing load averages */

int kmem;		/* file pointers for memory */
int ut;
int users, nentries;

#ifdef WHO
char whofilename[100];
#endif

#ifdef HOSTNAME
char hostname[32];
#endif

char lockfilename[100];	/* if exists, will prevent us from running */

/* flags which determine which info is printed */
int mailcheck = 1;	/* m - do biff like checking of mail 	*/
int proccheck = 1;	/* p - give information on processes	*/
int logcheck  = 1; 	/* l - tell who logs in and out		*/
int hostprint = 0;	/* h - print out hostname		*/
int dateprint = 0;	/* h - print out day/date		*/
int quiet     = 0;	/* q - hush diagnostic messages		*/

/* flags which determine how things are printed */
int clr_bet_ref = 0;	/* c - clear line between refeshes 	*/
int reverse     = 1;	/* r - use reverse video 		*/
int shortline   = 0;	/* s - short (left-justified) if escapes not allowed */
int leftline    = 0;	/* j - left-justified even if escapes allowed */
int sawmail;	/* remember mail was seen to print bells	*/

/* flags which have terminal do random things	*/
int beep      = 0;	/* b - beep every half hour and twice every hour */
int synch     = 1;	/* synchronize with clock		*/
int printid   = 0;	/* print pid of this process at startup */

/*
 * used to turn off reverse video every REVOFF times
 * in an attempt to not wear out the phospher.
 */
#define REVOFF 5
int revtime = 1;

/* select output device (status display or straight output (emacs window)) */
int emacs = 0;		/* assume status display */
int dbug = 0;

/* used by mail checker */
off_t mailsize = 0;
off_t linebeg = 0;		/* place where we last left off reading */

/* globals */
int mailprocessed;
char *username;
struct stat stbuf, mstbuf;	/* mstbuf for mail check only */
char *ourtty,*ttyname();	/* keep track of what tty we're on */
char *getenv();
char *tparm(), *tgoto();
unsigned delay = DEFDELAY;
int chars;
short uid;
double loadavg = 0.0;		/* current load average */
int fullprocess;
int users = 0;

/* strings which control status line display */
#ifdef	TERMINFO

char	*rev_out, *rev_end, *arrows;

#else	/* TERMCAP */

char	to_status_line[30];
char	from_status_line[20];
char	dis_status_line[20];
char	rev_out[20], rev_end[20];
char	*arrows, *bell = "\007";
int	eslok;	/* escapes on status line okay (reverse, cursor addressing) */
int	columns;
#endif

/* 
 * In order to determine how many people are logged on and who has
 * logged in or out, we read in the /etc/utmp file. We also keep track of 
 * the previous utmp file.
 */
struct utmp uts[2][MAXUSERS];

outc(c)
char c;
{
	if (dbug)
		printf("%s", unctrl(c));
	else putchar(c);
}

erroutc(c)
char c;
{
	if (dbug)
		fprintf(stderr,"%s", unctrl(c));
	else fputc(c,stderr);
}

main(argc,argv)
char **argv;
{
    register new,old,tmp;
    int clearbotl();
    char *cp;
    extern char _sobuf[];


    setbuf(stdout, _sobuf);
    signal(SIGINT,SIG_IGN);
    signal(SIGQUIT,SIG_IGN);
    signal(SIGALRM,SIG_IGN);
#ifdef VMUNIX
    signal(SIGTTOU,SIG_IGN);
#endif
    /*
     * When we logoff, init will do a "vhangup()" on this
     * tty which turns off I/O access and sends a SIGHUP
     * signal.  We catch this and thereby clear the status
     * display.  Note that a bug in 4.1bsd caused the SIGHUP
     * signal to be sent to the wrong process, so you had to
     * `kill -HUP' yourself in your .logout file.
     */
    signal(SIGHUP,clearbotl);

    argv++;
    while(--argc > 0) {
	switch(argv[0][0]) {
	    case '-': for(cp = &argv[0][1]; *cp ; cp++)
		      { 
			switch(*cp) {
			case 'r' : reverse = 0;	/* turn off reverse video */
				   break;
			case 'c':  clr_bet_ref = 1;
				   break;
			case 'h':  hostprint = 1;
				   break;
 			case 'D':  dateprint = 1;
 				   break;
			case 'm':  mailcheck = 0;
				   break;
			case 'p':  proccheck = 0;
				   break;
			case 'l':  logcheck = 0;
				   break;
			case 'b':  beep = 1;	
				   break;
			case 'i':  printid = 1;
				   break;
			case 'e':  emacs = 1;
				   break;
			case 'd':  dbug = 1;
				   signal(SIGINT, SIG_DFL);
				   signal(SIGQUIT, SIG_DFL);
				   break;
			case 'q':  quiet = 1;
				   break;
			case 's':  shortline = 1;
				   break;
 			case 'j':  leftline = 1;
 				   break;
		 	default:   fprintf(stderr,"sysline: bad flag: %c\n",*cp);
			}
		      }
		      break;
	    case '+': delay = atoi(&argv[0][1]);
	    	      if((delay <= 10) || (delay > 500)) delay = DEFDELAY;
		      synch = 0;	/* no more sync */
		      break;
	    default:  fprintf(stderr,"sysline: illegal argument %s\n",argv[0]);
	}
	argv++;
    }
    if(emacs) {
	columns = 80;
    } else {
    	/* if not to emacs window, initialize terminal dependent info */
	initterm();
    }
    
    /* immediately fork and let the parent die if not emacs mode */
    if(!emacs && !dbug && fork()) exit(0); 
    uid = getuid();

    ourtty = ttyname(2);	/* remember what tty we are on */
    if(printid) { printf("%d\n",getpid()); fflush(stdout);}
    close(1);
    dup2(2, 1);

    strcpy(whofilename,getenv("HOME"));
    strcat(whofilename,"/.who");

    strcpy(lockfilename,getenv("HOME"));
    strcat(lockfilename,"/.syslinelock");
    
#ifdef HOSTNAME
    if( hostprint ) gethostname(hostname,sizeof(hostname));
#endif

    if((ut = open("/etc/utmp",0)) < 0)
    {
	fprintf(stderr,"Can't open utmp");
	exit(1);
    }

    if((kmem = open("/dev/kmem",0)) < 0)
    {
	fprintf(stderr,"Can't open kmem");
	exit(1);
    }

    /* read in namelist in order to get location of symbols */
    readnamelist();

    if(proccheck) initprocread();

    if(mailcheck) 
    {
	chdir(MAILDIR);
	username = getenv("USER");
	if(stat(username,&mstbuf) != -1)
	{
		mailsize = mstbuf.st_size;
	}
	else mailsize = 0;
    }

    old = 0;
    new = 1;

    while(emacs || isloggedin())
    {
	if(access(lockfilename,0))
	{
	    mailprocessed = 0;
	    prtinfo(old,new);
	    sleep(delay);
	    if(clr_bet_ref)
	    {
		tputs(dis_status_line, 1, outc);
		fflush(stdout);
		sleep(5);
	    }
	    revtime = (1 + revtime) % REVOFF;

	    /*
	     * if we have processed mail, then dont switch utmp pointers
	     * since we havent printed the people whove logged in and out
	     */
	    if(!mailprocessed || !fullprocess)
	    {
		tmp = old;
		old = new;
		new = tmp;
	    }
	} else sleep(60);
    }
    clearbotl();
    /* NOTREACHED */
}

isloggedin()
{
   /*
    * you can tell if a person has logged out if the owner of
    * the tty has changed
    */
    struct stat statbuf;
    if(fstat(2,&statbuf) == 0)
    {
	if(statbuf.st_uid == uid) return(1);
    }
    return(0);	/* not logged in */
}


readnamelist()
{
	time_t bootime, clock, nintv, time();

#ifdef pdp11
	nlist("/unix",nl);
#else
	nlist("/vmunix",nl);
#endif
	if(nl[0].n_value == 0) {
	    if (!quiet)
		fprintf(stderr, "No namelist\n");
	    return;
	}
	lseek(kmem, (long)nl[NL_BOOT].n_value, 0);
	read(kmem, &bootime, sizeof(bootime));
	(void) time(&clock);
	nintv = clock - bootime;
	if (nintv <= 0L || nintv > 60L*60L*24L*365L) {
	   if (!quiet)
		fprintf(stderr, "Time makes no sense... namelist must be wrong\n");
	   nl[NL_PROC].n_value = nl[NL_AVEN].n_value = 0;
	}
}

readutmp(n)
{
	lseek(ut,0L,0);
	nentries = read(ut,&uts[n][0],MAXUSERS*sizeof(struct utmp)) 
	           / sizeof(struct utmp);
}

/* 
 * read in the process table locations and sizes, and allocate space
 * for storing the process table.  This is done only once.
 */
initprocread()
{
	if (nl[NL_PROC].n_value == 0) return;
#ifdef VMUNIX
	lseek(kmem,(long)nl[NL_PROC].n_value,0);
	read(kmem,&procadr,sizeof(procadr));
	lseek(kmem,(long)nl[NL_NPROC].n_value,0);
	read(kmem,&nproc,sizeof(nproc));
#endif
#ifdef pdp11
	procadr = nl[NL_PROC].n_value;
	nproc = NPROC;	/* from param.h */
#endif
	pr = (struct proc *) calloc(nproc,sizeof(struct proc));
}

/*
 * read in the process table.  This assumes that initprocread has alread been
 * called to set up storage.
 */
readproctab()
{
	if (nl[NL_PROC].n_value == 0) return(0);
	/* printf("There are %d entries beginning at %x\n",nproc,procadr); */
	lseek(kmem,(long)procadr,0);
	read(kmem,pr,nproc * sizeof(struct proc));
	return(1);
}

/*
 * codes to say what has happened to a particular entry in utmp
 * NOCH means no change, ON means new person logged on,
 * OFF means person logged off.
 */
#define NOCH 0;
#define ON 0x1
#define OFF 0x2

prtinfo(old,new)
int old,new;
{
	int procrun,procstop;
	int on,off;
	int status[MAXUSERS];
	register int i;
	double diff;

	stringinit();

#ifdef WHO
	/* check for file named .who in the home directory */
	whocheck();
#endif

	timeprint();

	/*
	 * if mail is seen, don't print rest of info, just the mail
	 * reverse new and old so that next time we run, we won't lose log
	 * in and out information
	 */
	if(mailcheck && (sawmail = mailseen()) )
	{
	    mailprocessed = 1;
	    goto bottom;
	}

#ifdef HOSTNAME
	/*
	 * print hostname info if requested
	 */
	if(hostprint)
	{
	    stringspace();
	    stringcat(hostname,strlen(hostname),1);
	}
#endif

	/* 
	 * print load average and difference between current load average
	 * and the load average 5 minutes ago
	 */
	if (nl[NL_AVEN].n_value != 0) {
		stringspace();
#ifdef VMUNIX
		lseek(kmem,(long)nl[NL_AVEN].n_value,0);
		read(kmem,avenrun,sizeof(avenrun));
#endif
#ifdef pdp11
		loadav(avenrun);
#endif
		stringprt("%.1f ",avenrun[0]);
		if((diff = avenrun[0] - avenrun[1]) < 0.0)
		     stringprt("%.1f",diff);
		else stringprt("+%.1f",diff);
		loadavg = avenrun[0];	/* remember load average */
	}

	/*
	 * print log on and off information
	 */
	stringspace();

	fullprocess = 1;
	
#ifdef MaxLoad	
	if(loadavg > MaxLoad) fullprocess = 0;	/* too loaded to run */
#endif
	/* read utmp file (logged in data) only if we are doing a full
	   process or if this is the first time and we are calculating
	   the number of users
	 */
	if(fullprocess || (users == 0)) readutmp(new);


	/* 
	 * make a pass through utmp, checking if person has logged off
	 * or on.  Results are stored in status[]
	 */
	on = off = 0;
	/* only do this if it hasn't been done yet (users == 0) or
	 * if the load average is low enough to permit it
	 */
	if(fullprocess || (users == 0 ))
	{
	    users = 0;
	    for(i=0 ; i < nentries ; i++)
	    {
		if(strcmpn(uts[old][i].ut_name,uts[new][i].ut_name,8) != 0)
		{
		    if(uts[old][i].ut_name[0] == '\0')
		    {
			status[i] = ON;
			on++;
		    }
		    else if (uts[new][i].ut_name[0] == '\0')
		    {
			status[i] = OFF;
			off++;
		    }
		    else {
			status[i] = ON | OFF;
			on++;
			off++;
		    }
		}
		else status[i] = NOCH;

		if(uts[new][i].ut_name[0]) users++;
	    }
	}

	/* at this point we know how many users there are */
	stringprt("%du",users);

	/* if there is any unread mail, put out a star */
	/* fprintf(stderr,"mailsz:%d,mtime:%d,atime:%d\n",
		mailsize,mstbuf.st_mtime,mstbuf.st_atime); */
	if((mailsize > 0) && (mstbuf.st_mtime >= mstbuf.st_atime))
		stringcat("*",1,1);

	/* if the load is too high, then we indicate that with a - sign */
	if(!fullprocess && (proccheck || logcheck)) stringcat("-",1,1);
	
	/* if we are to check on number of process running, do so now */
	if(fullprocess && proccheck && readproctab())
	{ 
	    /* 
	     * we are only interested in processes which have the same
	     * uid as us, and whose parent process id is not 1.
	     */
	    procrun = procstop = 0;
	    for(i=0; i < nproc ; i++)
	    {
		if((pr[i].p_stat == 0) || (pr[i].p_pgrp == 0)) continue; 
		if((pr[i].p_uid == uid) && (pr[i].p_ppid != 1)) 
		{
		    /* printf("found pid %d, stat=%o\n", pr[i].p_pid, pr[i].p_stat); */
		    switch (pr[i].p_stat) {

		    case SSTOP:
			    procstop++;
			    break;

		    case SSLEEP:
			    /*
			     * sleep can mean waiting for a signal or just
			     * in a disk or page wait queue ready to run.
			     * We can tell if it is the later by the pri being
			     * negative
			     */
			    if (pr[i].p_pri < PZERO) procrun++;
			    break;

		    case SWAIT:
		    case SRUN:
		    case SIDL:
			    procrun++;

		    }
		}
	    }

	    if((procrun > 0) || (procstop > 0))
	    {
		stringspace();
		if((procrun > 0) && (procstop > 0)) 
		{
		   stringprt("%dr",procrun);
		   stringprt(" %ds",procstop);
		}
		else if(procrun > 0) stringprt("%dr",procrun);
		     else stringprt("%ds",procstop);
	    }
	}   

	/* 
	 * if anyone has logged on or off, and we are interested in it,
	 * print it out
	 */
	if(logcheck && on)
	{
	    stringspace();
	    stringprt("on:",on);
	    for(i = 0 ; i < nentries ; i++)
	    {
		if(status[i] & ON)
		{ 
		    stringprt(" %.8s",uts[new][i].ut_name);
		    ttyprint(uts[new][i].ut_line);
		}
	    }
	}

	/* 
	 * check for people logging off if we are intereste
	 */
	if(logcheck && off)
	{
	    stringspace();
	    stringprt("off:",off);
	    for(i = 0 ; i < nentries ; i++)
	    {
		if(status[i] & OFF)
		{ 
		    stringprt(" %.8s",uts[old][i].ut_name);
		    ttyprint(uts[old][i].ut_line);
		}
	    }
	}
bottom:
	/* dump out what we know */
	stringdump();
}

timeprint()
{
	long curtime;
	struct tm *tp, *localtime();
	static int beepable=0;

	/* always print time */
	time(&curtime);
	tp = localtime(&curtime);

	if (dateprint)
		stringprt("%.11s", ctime(&curtime));
	stringprt("%d:",(tp->tm_hour > 12 ? tp->tm_hour - 12 
				     : (tp->tm_hour == 0 ? 12 : tp->tm_hour)));
	stringprt("%02d",tp->tm_min);

	if(synch) delay = 60 - tp->tm_sec;	/* sync with clock */

	/* beepable is used to insure that we get at most one set of beeps
	    every half hour */
	if(beep && beepable && (tp->tm_min == 30)) 
	{   
	    tputs(bell, 1, outc); fflush(stdout);
	    beepable = 0;
	}
	else if(beep && beepable && (tp->tm_min == 00)) 
	{
	    tputs(bell, 1, outc); fflush(stdout);
	    sleep(2);
	    tputs(bell, 1, outc); fflush(stdout);
	    beepable = 0;
	}
	else if(beep && ((tp->tm_min != 00) || (tp->tm_min != 30)))
	    beepable = 1;

}

/*
 * whocheck -- check for file named .who and print it on the who line first
 */
whocheck()
{
    register wf,i,chss;
    char buff[81];

    if((wf = open(whofilename,0)) >= 0)
    {
	chss = read(wf,buff,80);
	if(chss == 0) 
	{
	    close(wf);
	    return;
	}
	buff[chss] = '\0';
	/* 
	 * remove all line feeds, and replace by spaces if they are within
	 * the message, else replace them by nulls.
	 */
	for(i = chss; i >= 0; i--)
	{
	    if(buff[i] == '\n') 
	    {
		if(buff[i+1]) buff[i] = ' ';
		else buff[i] = '\0';
	    }
	}
	stringprt("%s",buff);
	stringspace();
	close(wf);
    }
}

/* 
 * ttyprint -- given the name of a tty, print in the string buffer its
 * short name surrounded by parenthesis.
 * ttyxx is printed as (xx)
 * console is printed as (cty)
 */
ttyprint(name)
char *name;
{
    char buff[10];

    if(strcmpn(name,"tty",3)==0)
    {
	sprintf(buff,"(%s)",name+3);
    }
    else if(strcmp(name,"console")== 0)
    {
	sprintf(buff,"(cty)");
    }
    else sprintf(buff,"(%s)",name);

    stringcat(buff,strlen(buff),0);
}

/* 
 * mail checking function 
 * returns 0 if no mail seen
 */
mailseen()
{
    FILE *mfd;
    int retval = 0;
    int chs,initchs;
    register char *rp,*cp;
    char lbuf[100], sendbuf[100];
    int toprint,seenspace = 0;

    if(stat(username,&mstbuf) != -1)
    {
	if((mstbuf.st_size > mailsize) && ((mfd=fopen(username,"r")) != NULL))
	{
	    /* fprintf(stderr,"Mail gotten was %db, now %db\n",
				 mailsize,stbuf.st_size); */
	    fseek(mfd,mailsize,0);
	    while((initchs = readline(mfd, lbuf, sizeof(lbuf))) != EOF)
	    {
		if(strcmpn(lbuf,"From",4) == 0)
		{
		    cp = lbuf+5;	/* start after the From */
		    while(*cp && (*++cp != ' ')); /* skip to blank */
		    *cp = '\0';		/* terminate name */
		    stringspace();
		    /*  if(!emacs) stringcat(bell,0,0);   BELL MOVED */
		    sprintf(sendbuf,"Mail from %s ",lbuf+5);
		    stringcat(sendbuf,strlen(sendbuf),0);
		    /* print message preceeded by little arrow */
		    /* skip over the headers and look for blank line */
		    while(((chs = readline(mfd, lbuf, sizeof(lbuf))) != EOF) && (chs != 0))
		        if(strcmpn(lbuf,"Subject",7)==0)
		        {
			    sprintf(sendbuf,"on %s",lbuf+9);
			    stringcat(sendbuf,strlen(sendbuf),1);
			}
		    if(!emacs) stringcat(arrows,2,0);
		    else stringcat(" : ",3,0);

		    if(chs != EOF)
		    {
			cp = sendbuf;
			toprint = columns - chars; /* space left on line */
			lbuf[0] = '\0';
			while((chs = readline(mfd, lbuf, sizeof(lbuf))) != EOF)
			{
			    if(toprint > 0)
			    {
				*cp++ = ' ';	/* space before lines */
				toprint--;
			    }
			    rp = lbuf;
			    if(strcmpn(lbuf,"From",4) == 0) break;
			    while(*rp && (toprint > 0))
			    {
				if(isspace(*rp))
				{ 
				    if(!seenspace)
				    {
					*cp++ = ' ';
					seenspace = 1;
					toprint--;
				    }
				}
				else {
				    *cp++ = *rp;
				    seenspace = 0;
				    toprint--;
				}
				rp++;
			    }
			}
			*cp = '\0';
			stringcat(sendbuf,strlen(sendbuf),1);
			/*  if(!emacs) stringcat(bell,0,0);   BELL MOVED */
			retval = 1;
		    }
		break;
		}
	    }
	    if(initchs == EOF) 
	    {
		stringprt("Mail has just arrived",chs);
	    }
	/*
	 * want to update write time  so a star will
	 * appear after the number of users until the
	 * user reads his mail 
	 */
	mailsize = linebeg;	
	touch(username,mfd);
	fclose(mfd);
	}
	else mailsize = mstbuf.st_size;
    }
    else mailsize = 0;
    return(retval);
}
				
/* 
 * readline -- read a line from fp and store it in buf.
 * return the number of characters read.
 */
readline(fp, buf, maxch)
FILE *fp;
char *buf;
{
    register char *cp, ch;
    int size = maxch;
    long ftell();

    linebeg = ftell(fp);		/* remember loc where line begins */
    cp = buf;
    while(((ch=getc(fp)) != EOF) && (ch != '\n') && (size-- > 0)) *cp++ = ch;
    *cp = '\0';
    if((size == maxch) && (ch == EOF)) return (EOF);
    else return(maxch - size);
}
    

/* string hacking functions */

int eol; 	/* non zero when we have hit the end of line */
char *sp;
char strarr[120];

stringinit()
{
    sp = strarr;
    chars = 0;
    eol = 0;
}

stringprt(format,value)
char *format;
{
    char tempbuf[150];
    int bufsiz;

    sprintf(tempbuf,format,value);
    bufsiz = strlen(tempbuf);
    stringcat(tempbuf,bufsiz,0);
}

stringdump()
{
    char bigbuf[200];
    register char *cp;
    register int i;
    char blanks[80];

    *sp = '\0';
    bigbuf[0] = 0;
    if(!emacs) {
	if (sawmail) strcat(bigbuf, bell);
	if (eslok) {
	    if(!leftline)
	    	cp = tparm(to_status_line, columns - chars);
	    else 
	    	cp = tparm(to_status_line, 0);
	    strcat(bigbuf, cp);
	} else {
	    strcat(bigbuf, to_status_line);
	    if (!shortline & !leftline) {
		for (i=0; i < (columns-chars); i++)
		    blanks[i] = ' ';
		blanks[columns-chars] = '\0';
		strcat(bigbuf, blanks);
	    }
	}
	if(reverse && !(revtime == 0)) strcat(bigbuf, rev_out);
    }
    strcat(bigbuf,strarr);
    if (!emacs) {
	if (reverse) strcat(bigbuf, rev_end);
	strcat(bigbuf, from_status_line);
	if (sawmail) {
	    strcat(bigbuf, bell);
	    strcat(bigbuf, bell);
	}
	tputs(bigbuf, 1, outc);
	if (dbug) putchar('\n');
	fflush(stdout);
    } else
	write(2,bigbuf,strlen(bigbuf));
}

stringspace()
{
    if(!emacs && reverse && !(revtime == 0)) {
#ifdef TERMINFO
	stringcat(rev_end, magic_cookie_glitch<=0?0:magic_cookie_glitch, 0);
	stringcat(" ",1,0);
	stringcat(rev_out, magic_cookie_glitch<=0?0:magic_cookie_glitch, 0);
#else
	stringcat(rev_end,0,0);
	stringcat(" ",1,0);
	stringcat(rev_out,0,0);
#endif
    } else stringcat(" ",1,0);
}

/* 
 * stringcat :: concatenate the characters in string str to the list we are 
 * 	        building to send out.
 *  
 * the three args are
 *    str - the string to print. may contain funny (terminal control) chars.
 *    chrs - the number of printable characters in the string
 *    trunc - a flag which is non zero if we should truncate strings which
 *   	      don't fit.  If this is 0 then if a string doesn't completely
 *	      fit it wont' be printed, this prevents us from getting 1/2
 *	      way through an escape sequence.
 */
stringcat(str,chrs,trunc)
char *str;
{

    if((chrs == 0) || (!eol && chars + chrs <= columns)
    		   || (!eol && trunc && (chars < columns)))
    {
	while(*sp++ = *str++)
	    if(trunc)
	    {   
		if(++chars >= columns)	/* check for trunc */
		{
		    eol = 1;
		    return;
		}
	    }
	sp--;
	if(!trunc) chars += chrs;
    }
    else eol = 1;
}

/*
 * touch :: update the modify time of a file.
 */
touch(name,filedes)
char *name;		/* name of file */
FILE *filedes;		/* already open for read file descriptor */
{
	register fd;
	char buf[1];

	lseek(fileno(filedes),0L,0);
	read(fileno(filedes),buf,1);	/* get first byte */
	if((fd = open(name,2)) >= 0) 	/* open in append mode */
	{
	    lseek(fd,0L,0);		/* go to beginning */
	    write(fd,buf,1);		/* and rewrite first byte */
	    close(fd);
	}
}


/* 
 * clearbotl :: clear bottom line.  
 * called when process quits or is killed.
 * it clears the bottom line of the terminal.
 */
clearbotl()
{
	register int fd;
	int exit();

	signal(SIGALRM,exit);
	alarm(30);	/* if can't open in 30 secs, just die */
	if( !emacs && (fd = open(ourtty,1)) >=0)
	{
	    write(fd,dis_status_line,strlen(dis_status_line));
	    close(fd);
	}
	exit(0);
}

#ifdef TERMINFO
initterm()
{
	static char standbuf[40];

	setupterm(0, 1, 0);
	if (!has_status_line) {
		/* not an appropriate terminal */
		if (!quiet)
		   fprintf(stderr, "sysline: no status capability for %s\n",
			getenv("TERM"));
		exit(1);
	}
	if (status_line_esc_ok) {
		if (set_attributes) {
			/* reverse video mode */
			strcpy(standbuf, tparm(set_attributes,0,0,1,0,0,0,0,0,0));
			rev_out = standbuf;
			rev_end = exit_attribute_mode;
		} else if (enter_standout_mode && exit_standout_mode) {
			rev_out = enter_standout_mode;
			rev_end = exit_standout_mode;
		} else {
			rev_out = rev_end = "";
		}
	} else
		rev_out = rev_end = "";
	columns--;	/* avoid cursor wraparound */
}

#else	/* TERMCAP */

initterm()
{
	char *term, *cp;
	char tbuf[1024], is2[40];
	extern char *UP;
	
	if ((term=getenv("TERM")) == NULL) {
		if (!quiet)
		   fprintf(stderr, "sysline: No TERM variable in enviroment\n");
		exit(1);
	}
	if (tgetent(tbuf, term) <= 0) {
		if (!quiet)
		   fprintf(stderr, "sysline: Unknown terminal type: %s\n", term);
		exit(1);
	}
	if (tgetflag("hs") <= 0) {
		if (!strcmpn(term, "h19", 3)) {
			/* for upward compatability with h19sys */
			strcpy(to_status_line, "\033j\033x5\033x1\033Y8%+ \033o");
			strcpy(from_status_line, "\033k\033y5");
			strcpy(dis_status_line, "\033y1");
			strcpy(rev_out, "\033p");
			strcpy(rev_end, "\033q");
			arrows = "\033Fhh\033G";
			columns = 80;
			UP = "\b";
			return;
		}
		if (!quiet)
		   fprintf(stderr, "sysline: No status capability for %s\n", term);
		exit(1);
	}
	cp = is2;
	if (tgetstr("i2", &cp) != NULL) {
		/* someday tset will do this */
		tputs(is2, 1, erroutc);
		fflush(stdout);
	}

	/* the "-1" below is to avoid cursor wraparound problems */
	columns = tgetnum("co") - 1;
	cp = to_status_line;
	tgetstr("ts", &cp);
	cp = from_status_line;
	tgetstr("fs", &cp);
	cp = dis_status_line;
	tgetstr("ds", &cp);
	eslok = tgetflag("es");
	if (eslok) {
		cp = rev_out;
		tgetstr("so", &cp);
		cp = rev_end;
		tgetstr("se", &cp);
	} else {
		reverse = 0;	/* turn off reverse video */
	};
	UP = "\b";
	if (!strcmpn(term, "h19", 3))
		arrows = "\033Fhh\033G";	/* "two tiny graphic arrows" */
	else
		arrows = "->";
}

char *
tparm(cap, parm)
char *cap;
int parm;
{
	return tgoto(cap, 0, parm);
}
#endif

#ifdef pdp11
loadav(ap)
double ap[];
{
	register int i;
	short s_avenrun[3];

	lseek(kmem, (long)nl[NL_AVEN].n_value, 0);
	read(kmem, s_avenrun, sizeof(s_avenrun));
	for (i=0; i < (sizeof(s_avenrun)/sizeof(s_avenrun[0])); i++)
		ap[i] = s_avenrun[i] / 256.0;
}
#endif

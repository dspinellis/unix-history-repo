/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)at.c	5.7 (Berkeley) %G%";
#endif not lint

/*
 *	Synopsis:	at [-s] [-c] [-m] time [filename]
 *						
 * 
 *
 *	Execute commands at a later date.
 *
 *
 *	Modifications by:	Steve Wall
 *				Computer Systems Research Group
 *				University of California @ Berkeley
 *
 */
#include <sys/param.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>

#define HOUR		100		/* 1 hour (using military time) */
#define HALFDAY		(12 * HOUR)	/* half a day (12 hours) */
#define FULLDAY		(24 * HOUR)	/* a full day (24 hours) */

#define WEEK		1		/* day requested is 'week' */
#define DAY		2		/* day requested is a weekday */
#define MONTH		3		/* day requested is a month */

#define BOURNE		"/bin/sh"	/* run commands with Bourne shell*/
#define CSHELL		"/bin/csh"	/* run commands with C shell */

#define NODATEFOUND	-1		/* no date was given on command line */

#define ATDIR		"/usr/spool/at"		/* spooling area */

#define LINSIZ		256		/* length of input buffer */

/*
 * A table to identify potential command line values for "time". 
 *
 * We need this so that we can do some decent error checking on the 
 * command line arguments. (This was inspired by the old "at", which 
 * accepted "at 900 jan 55" as valid input and other small bugs.
 */
struct datetypes {
	int type;
	char *name;
} dates_info[22] = {
	{ DAY,	 "sunday"    },
	{ DAY,	 "monday"    },
	{ DAY,	 "tuesday"   },
	{ DAY,	 "wednesday" },
	{ DAY,	 "thursday"  },
	{ DAY,	 "friday"    },
	{ DAY,	 "saturday"  },
	{ MONTH, "january"   },
	{ MONTH, "february"  },
	{ MONTH, "march"     },
	{ MONTH, "april"     },
	{ MONTH, "may"	     },
	{ MONTH, "june"	     },
	{ MONTH, "july"	     },
	{ MONTH, "august"    },
	{ MONTH, "september" },
	{ MONTH, "october"   },
	{ MONTH, "november"  },
	{ MONTH, "december"  },
	{ 0, ""},
};

/*
 * Months of the year.
 */
char *months[13] = {
	"jan", "feb", "mar", "apr", "may", "jun",
	"jul", "aug", "sep", "oct", "nov", "dec", 0,
};

/*
 * A table of the number of days in each month of the year.
 *
 *	yeartable[0] -- normal year
 *	yeartable[1] -- leap year
 */
static int yeartable[2][13] = {
	{ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
	{ 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
};

/*
 * Structure holding the relevant values needed to create a spoolfile.
 * "attime" will contain the info about when a job is to be run, and
 * "nowtime" will contain info about what time the "at" command is in-
 * voked.
 */
struct times {
	int year;			/* year that job is to be run */
	int yday;			/* day of year that job is to be run */
	int mon;			/* month of year that job is to be run*/
	int mday;			/* day of month that job is to be run */
	int wday;			/* day of week that job is to be run */
	int hour;			/* hour of day that job is to be run */
	int min;			/* min. of hour that job is to be run */
} attime, nowtime;

char	atfile[100];			/* name of spoolfile "yy.ddd.hhhh.??" */
char	*getenv();			/* get info on user's environment */
char	**environ;			/* user's environment */
FILE	*spoolfile;			/* spool file */
FILE	*inputfile;			/* input file ("stdin" or "filename") */
char	*getwd();			/* used to get current directory info */


main(argc, argv)
int argc;
char **argv;
{
	int c;				/* scratch variable */
	int usage();			/* print usage info and exit */
	int cleanup();			/* do cleanup on an interrupt signal */
	int dateindex = NODATEFOUND;	/* if a day is specified, what option
					   is it? (mon day, week, dayofweek) */
	char *shell = BOURNE;		/* what shell do we use to run job? */
	int shflag = 0;			/* override the current shell and run
					   job using the Bourne Shell */
	int cshflag = 0;		/* override the current shell and run 
					   job using the Cshell */
	int mailflag = 0;		/* send mail after a job has been run?*/
	int standardin = 0;		/* are we reading from stardard input */
	char *tmp;			/* scratch pointer */
	char line[LINSIZ];		/* a line from input file */
	char pwbuf[MAXPATHLEN];		/* the current working directory */
	char *jobfile = "stdin";	/* file containing job to be run */
	char *getname();		/* get the login name of a user */
	int pid;			/* For forking for security reasons */



	argv++; argc--;

	/*
	 * Interpret command line flags if they exist.
	 */
	while (argc > 0 && **argv == '-') {
		(*argv)++;
		while (**argv) switch (*(*argv)++) {

			case 'c' :	cshflag++; 
					shell = CSHELL;
					break;

			case 's' :	shflag++;
					shell = BOURNE;
					break;

			case 'm' :	mailflag++;
					break;

			default	 :	usage();

		}
		--argc, ++argv;
	}
	if (shflag && cshflag) {
		fprintf(stderr,"ambiguous shell request.\n");
		exit(1);
	}

	/*
	 * Get the time it is when "at" is invoked. We set both nowtime and 
	 * attime to this value so that as we interpret the time the job is to
	 * be run we can compare the two values to determine such things as
	 * whether of not the job should be run the same day the "at" command
	 * is given, whether a job is to be run next year, etc.
	 */
	getnowtime(&nowtime, &attime);

#ifdef DEBUG
	printit();
#endif

	if (argc <= 0)
		usage();

	/*
	 * Interpret argv[1] and create the time of day that the job is to
	 * be run. This is the same function that was used in the old "at"
	 */
	maketime(&attime, *argv);
	--argc; ++argv;

#ifdef DEBUG
	printf("\n\nAFTER MAKETIME\n");
	printit();
#endif

	/*
	 * If argv[(2)] exists, this is a request to run a job on a certain
	 * day of year or a certain day of week.
	 *
	 * We send  argv to the function "getdateindex" which returns the 
	 * index value of the requested day in the table "dates_info" 
	 * (see line 50 for table). If 'getdateindex" returns a NODATEFOUND, 
	 * then the requested day format was not found in the table (usually 
	 * this means that the argument is a "filename"). If the requested 
	 * day is found, we continue to process command line arguments.
	 */
	if (argc > 0) {
		if ((dateindex = getdateindex(*argv)) != NODATEFOUND) {

			++argv; --argc;

			/*
			 * Determine the day of year that the job will be run
			 * depending on the value of argv.
			 */
			makedayofyear(dateindex, &argv, &argc);
		}
	}

	/*
	 * If we get to this point and "dateindex" is set to NODATEFOUND,
	 * then we are dealing with a request with only a "time" specified
	 * (i.e. at 400p) and perhaps 'week' specified (i.e. at 400p week).
	 * If 'week' is specified, we just set excecution for 7 days in the
	 * future. Otherwise, we need to check to see if the requested time 
	 * has already passed for the current day. If it has, then we add 
	 * one to the day of year that the job will be executed.
	 */
	if (dateindex == NODATEFOUND) {
		int daysinyear;
		if ((argc > 0) && (strcmp(*argv,"week") == 0)) {
			attime.yday += 7;
			++argv; --argc;
		} else if (istomorrow())
			++attime.yday;

		daysinyear = isleap(attime.year) ? 366 : 365;
		if (attime.yday >= daysinyear) {
			attime.yday -= daysinyear;
			++attime.year;
		}
	}

	/*
	 * If no more arguments exist, then we are reading
	 * from standard input. Thus, we set the standard
	 * input flag (++standardin).
	 */
	if (argc <= 0)
		++standardin;


#ifdef DEBUG
	printf("\n\nAFTER ADDDAYS\n");
	printit();
#endif

	/*
	 * Start off assuming we're going to read from standard input,
	 * but if a filename has been given to read from, we will open it
	 * later.
	 */
	inputfile = stdin;

	/*
	 * Create the filename for the spoolfile.
	 */
	makeatfile(atfile,attime.year,attime.yday,attime.hour,attime.min);

	/*
	 * Open the spoolfile for writing.
	 */
	if ((spoolfile = fopen(atfile, "w")) == NULL){
		perror(atfile);
		exit(1);
	}

	/*
	 * Make the file not world readable.
	 */
	fchmod(fileno(spoolfile), 0400);

	/*
	 * The protection mechanism works like this:
	 * We are running ruid=user, euid=spool owner.  So far we have been
	 * messing around in the spool directory, so we needed to run
	 * as the owner of the spool directory.
	 * We now need to switch to the user's effective uid
	 * to simplify permission checking.  However, we fork first,
	 * so that we can clean up if interrupted.
	 */
	signal(SIGINT, SIG_IGN);
	pid = fork();
	if (pid == -1) {
		perror("fork");
		exit(1);
	}
	if (pid) {
		int wpid, status;

		/*
		 * We are the parent. If the kid has problems,
		 * cleanup the spool directory.
		 */
		wpid = wait(&status);
		if (wpid != pid || status) {
			cleanup();
			exit(1);
		}
		/*
		 * The kid should have alread flushed the buffers.
		 */
		_exit(0);
	}

	/*
	 * Exit on interrupt.
	 */
	signal(SIGINT, SIG_DFL);

	/*
	 * We are the kid, give up special permissions.
	 */
	setuid(getuid());

	/*
	 * Open the input file with the user's permissions.
	 */
	if (!standardin) {
		jobfile = *argv;
		if ((inputfile = fopen(jobfile, "r")) == NULL) {
			perror(jobfile);
			exit(1);
		}
	}

	/*
	 * Determine what shell we should use to run the job. If the user
	 * didn't explicitly request that his/her current shell be over-
	 * ridden (shflag of cshflag) then we use the current shell.
	 */
	if ((!shflag) && (!cshflag) && (getenv("SHELL") != NULL))
		shell = "$SHELL";

	/*
	 * Put some standard information at the top of the spoolfile.
	 * This info is used by the other "at"-oriented programs (atq,
	 * atrm, atrun).
	 */
	fprintf(spoolfile, "# owner: %.127s\n",getname(getuid()));
	fprintf(spoolfile, "# jobname: %.127s\n",jobfile);
	fprintf(spoolfile, "# shell: sh\n");
	fprintf(spoolfile, "# notify by mail: %s\n",(mailflag) ? "yes" : "no");
	fprintf(spoolfile, "\n");

	/*
	 * Set the modes for any files created by the job being run.
	 */
	c = umask(0);
	umask(c);
	fprintf(spoolfile, "umask %.1o\n", c);

	/*
	 * Get the current working directory so we know what directory to 
	 * run the job from.
	 */
	if (getwd(pwbuf) == NULL) {
		fprintf(stderr, "at: can't get working directory\n");
		exit(1);
	}
	fprintf(spoolfile, "cd %s\n", pwbuf);

	/*
	 * Copy the user's environment to the spoolfile.
	 */
	if (environ) {
		copyenvironment(&spoolfile);
	}

	/*
	 * Put in a line to run the proper shell using the rest of
	 * the file as input.  Note that 'exec'ing the shell will
	 * cause sh() to leave a /tmp/sh### file around.  This line
	 * depends on the shells allowing EOF to end tagged input.  The
	 * quotes also guarantee a quoting of the lines before EOF.
	 */
	fprintf(spoolfile, "%s << 'QAZWSXEDCRFVTGBYHNUJMIKOLP'\n", shell);

	/*
	 * Now that we have all the files set up, we can start reading in
	 * the job.
	 */
	while (fgets(line, LINSIZ, inputfile) != NULL)
		fputs(line, spoolfile);

	/*
	 * Close all files and change the mode of the spoolfile.
	 */
	fclose(inputfile);
	fclose(spoolfile);

	exit(0);

}

/*
 * Copy the user's environment to the spoolfile in the syntax of the
 * Bourne shell.  After the environment is set up, the proper shell
 * will be invoked.
 */
copyenvironment(spoolfile)
FILE **spoolfile;
{
	char *tmp;			/* scratch pointer */
	char **environptr = environ;	/* pointer to an environment setting */

	while(*environptr) {
		tmp = *environptr;

		/*
		 * We don't want the termcap or terminal entry so skip them.
		 */
		if ((strncmp(tmp,"TERM=",5) == 0) ||
		    (strncmp(tmp,"TERMCAP=",8) == 0)) {
			++environptr;
			continue;
		}

		/*
		 * Set up the proper syntax.
		 */
		while (*tmp != '=')
			fputc(*tmp++,*spoolfile);
		fputc('=', *spoolfile);
		fputc('\'' , *spoolfile);
		++tmp;

		/*
		 * Now copy the entry.
		 */
		while (*tmp) {
			if (*tmp == '\'')
				fputs("'\\''", *spoolfile);
			else if (*tmp == '\n')
				fputs("\\",*spoolfile);
			else
				fputc(*tmp, *spoolfile);
			++tmp;
		}
		fputc('\'' , *spoolfile);

		/*
		 * We need to "export" environment settings.
		 */
		fprintf(*spoolfile, "\nexport ");
		tmp = *environptr;
		while (*tmp != '=')
			fputc(*tmp++,*spoolfile);
		fputc('\n',*spoolfile);
		++environptr;
	}
	return;
}

/*
 * Create the filename for the spoolfile. The format is "yy.ddd.mmmm.??"
 * where "yy" is the year the job will be run, "ddd" the day of year, 
 * "mmmm" the hour and minute, and "??" a scratch value used to dis-
 * tinguish between two files that are to be run at the same time.
 */
makeatfile(atfile,year,dayofyear,hour,minute)
int year;
int hour;
int minute;
int dayofyear;
char *atfile;
{
	int i;				/* scratch variable */

	for (i=0; ; i += 53) {
		sprintf(atfile, "%s/%02d.%03d.%02d%02d.%02d", ATDIR, year,
			dayofyear, hour, minute, (getpid() + i) % 100);

		/*
		 * Make sure that the file name that we've created is unique.
		 */
		if (access(atfile, F_OK) == -1)
			return;
	}
}

/*
 * Has the requested time already passed for the currrent day? If so, we
 * will run the job "tomorrow".
 */
istomorrow()
{
	if (attime.hour < nowtime.hour)
		return(1);
	if ((attime.hour == nowtime.hour) && (attime.min < nowtime.min))
		return(1);

	return(0);
}

/*
 * Debugging wreckage.
 */
printit()
{
	printf("YEAR\tnowtime: %d\tattime: %d\n",nowtime.year,attime.year);
	printf("YDAY\tnowtime: %d\tattime: %d\n",nowtime.yday,attime.yday);
	printf("MON\tnowtime: %d\tattime: %d\n",nowtime.mon,attime.mon);
	printf("MONDAY\tnowtime: %d\tattime: %d\n",nowtime.mday,attime.mday);
	printf("WDAY\tnowtime: %d\tattime: %d\n",nowtime.wday,attime.wday);
	printf("HOUR\tnowtime: %d\tattime: %d\n",nowtime.hour,attime.hour);
	printf("MIN\tnowtime: %d\tattime: %d\n",nowtime.min,attime.min);
}

/*
 * Calculate the day of year that the job will be executed.
 * The av,ac arguments are ptrs to argv,argc; updated as necessary.
 */
makedayofyear(dateindex, av, ac)
int dateindex;
char ***av;
int *ac;
{
	char **argv = *av;	/* imitate argc,argv and update args at end */
	int argc = *ac;
	char *ptr;				/* scratch pointer */
	struct datetypes *daterequested;	/* pointer to information about
						   the type of date option
						   we're dealing with */

	daterequested = &dates_info[dateindex];

	/*
	 * If we're dealing with a day of week, determine the number of days
	 * in the future the next day of this type will fall on. Add this
	 * value to "attime.yday".
	 */
	if (daterequested->type == DAY) {
		if (attime.wday < dateindex) 
			attime.yday += dateindex - attime.wday;
		else if(attime.wday > dateindex) 
			attime.yday += (7 - attime.wday) + dateindex;
		else attime.yday += 7;
	}

	/*
	 * If we're dealing with a month and day of month, determine the
	 * day of year that this date will fall on.
	 */
	if (daterequested->type == MONTH) {

		/*
		 * If a day of month isn't specified, print a message
		 * and exit.
		 */
		if (argc <= 0) {
			fprintf(stderr,"day of month not specified.\n");
			exit(1);
		}

		/*
		 * Scan the day of month value and make sure that it
		 * has no characters in it. If characters are found or
		 * the day requested is zero, print a message and exit.
		 */
		ptr = *argv;
		while (isdigit(*ptr))
			++ptr;
		if ((*ptr != '\0') || (atoi(*argv) == 0)) {
			fprintf(stderr,"\"%s\": illegal day of month\n",*argv);
			exit(1);
		}

		/*
		 * Set the month of year and day of month values. Since
		 * the first 7 values in our dateinfo table do not deal
		 * with month names, we subtract 7 from the month of year
		 * value.
		 */
		attime.mon = (dateindex - 7);
		attime.mday = (atoi(*argv) - 1);

		/*
		 * Test the day of month value to make sure that the
		 * value is legal.
		 */
		if ((attime.mday + 1) > 
		    yeartable[isleap(attime.year)][attime.mon + 1]) {
			fprintf(stderr,"\"%s\": illegal day of month\n",*argv);
			exit(1);
		}

		/*
		 * Finally, we determine the day of year.
		 */
		attime.yday = (countdays());
		++argv; --argc;
	}

	/*
	 * If 'week' is specified, add 7 to the day of year.
	 */
	if ((argc > 0) && (strcmp(*argv,"week") == 0)) {
		attime.yday += 7;
		++argv; --argc;
	}

	/*
	 * Now that all that is done, see if the requested execution time
	 * has already passed for this year, and if it has, set execution
	 * for next year.
	 */
	if (isnextyear())
		++attime.year;
	
	/*
	 * Finally, reflect the updated argc,argv to the caller
	 */
	*av = argv;
	*ac = argc;
}

/*
 * Should the job be run next year? We check for the following situations:
 *
 *	1) the requested time has already passed for the current year. 
 *	2) the day of year is greater than the number of days in the year. 
 *
 * If either of these tests succeed, we increment "attime.year" by 1. 
 * If #2 is true, we also subtract the number of days in the current year
 * from "attime.yday". #2 can only occur if someone specifies a job to
 * be run "tomorrow" on Dec. 31 or if they specify a job to be run a
 * 'week' later and the date is at least Dec. 24. (I think so anyway)
 */
isnextyear()
{	register daysinyear;
	if (attime.yday < nowtime.yday)
		return(1);

	if ((attime.yday == nowtime.yday) && (attime.hour < nowtime.hour))
		return(1);

	daysinyear = isleap(attime.year) ? 366 : 365;
	if (attime.yday >= daysinyear) {
		attime.yday -= daysinyear;
		return(1);
	}
	if (attime.yday > (isleap(attime.year) ? 366 : 365)) {
		attime.yday -= (isleap(attime.year) ? 366 : 365);
		return(1);
	}

	return(0);
}

/*
 * Determine the day of year given a month and day of month value.
 */
countdays()
{
	int leap;			/* are we dealing with a leap year? */
	int dayofyear;			/* the day of year after conversion */
	int monthofyear;		/* the month of year that we are
					   dealing with */

	/*
	 * Are we dealing with a leap year?
	 */
	leap = isleap(attime.year);

	monthofyear = attime.mon;
	dayofyear = attime.mday;

	/*
	 * Determine the day of year.
	 */
	while (monthofyear > 0)
		dayofyear += yeartable[leap][monthofyear--];

	return(dayofyear);
}

/*
 * Is a year a leap year?
 */
isleap(year)
int year;

{
	return((year%4 == 0 && year%100 != 0) || year%100 == 0);
}

getdateindex(date)
char *date;
{
	int i = 0;
	struct datetypes *ptr;

	ptr = dates_info;

	for (ptr = dates_info; ptr->type != 0; ptr++, i++) {
		if (isprefix(date, ptr->name))
			return(i);
	}
	return(NODATEFOUND);
}

isprefix(prefix, fullname)
char *prefix, *fullname;
{
	char ch;
	char *ptr;
	char *ptr1;

	ptr = prefix;
	ptr1 = fullname;

	while (*ptr) {
		ch = *ptr;
		if (isupper(ch))
			ch = tolower(ch);

		if (ch != *ptr1++)
			return(0);

		++ptr;
	}
	return(1);
}

getnowtime(nowtime, attime)
struct times *nowtime;
struct times *attime;
{
	struct tm *now;
	struct timeval time;
	struct timezone zone;

	if (gettimeofday(&time,&zone) < 0) {
		perror("gettimeofday");
		exit(1);
	}
	now = localtime(&time.tv_sec);

	attime->year = nowtime->year = now->tm_year;
	attime->yday = nowtime->yday = now->tm_yday;
	attime->mon = nowtime->mon = now->tm_mon;
	attime->mday = nowtime->mday = now->tm_mday;
	attime->wday = nowtime->wday = now->tm_wday;
	attime->hour = nowtime->hour = now->tm_hour;
	attime->min = nowtime->min = now->tm_min;
}

/*
 * This is the same routine used in the old "at", so I won't bother
 * commenting it. It'll give you an idea of what the code looked
 * like when I got it.
 */
maketime(attime,ptr)
char *ptr;
struct times *attime;
{
	int val;
	char *p;

	p = ptr;
	val = 0;
	while(isdigit(*p)) {
		val = val*10+(*p++ -'0');
	}
	if (p-ptr < 3)
		val *= HOUR;

	for (;;) {
		switch(*p) {

		case ':':
			++p;
			if (isdigit(*p)) {
				if (isdigit(p[1])) {
					val +=(10* *p + p[1] - 11*'0');
					p += 2;
					continue;
				}
			}
			fprintf(stderr, "bad time format:\n");
			exit(1);

		case 'A':
		case 'a':
			if (val >= HALFDAY+HOUR)
				val = FULLDAY+1;  /* illegal */
			if (val >= HALFDAY && val <(HALFDAY+HOUR))
				val -= HALFDAY;
			break;

		case 'P':
		case 'p':
			if (val >= HALFDAY+HOUR)
				val = FULLDAY+1;  /* illegal */
			if (val < HALFDAY)
				val += HALFDAY;
			break;

		case 'n':
		case 'N':
			if ((val == 0) || (val == HALFDAY))
				val = HALFDAY;
			else
				val = FULLDAY+1;  /* illegal */
			break;

		case 'M':
		case 'm':
			if ((val == 0) || (val == HALFDAY))
				val = 0;
			else
				val = FULLDAY+1;  /* illegal */
			break;


		case '\0':
		case ' ':
			break;

		default:
			fprintf(stderr, "bad time format\n");
			exit(1);

		}
		break;
	}
	if (val < 0 || val >= FULLDAY) {
		fprintf(stderr, "time out of range\n");
		exit(1);
	}
	if (val%HOUR >= 60) {
		fprintf(stderr, "illegal minute field\n");
		exit(1);
	}
	attime->hour = val/HOUR;
	attime->min = val%HOUR;
}

/*
 * Get the full login name of a person using his/her user id.
 */
char *
getname(uid)
int uid;
{
	struct passwd *pwdinfo;			/* password info structure */
	char *logname, *getlogin();

	logname = getlogin();
	if (logname == NULL || (pwdinfo = getpwnam(logname)) == NULL ||
	    pwdinfo->pw_uid != uid)
		pwdinfo = getpwuid(uid);
	if (pwdinfo == 0) {
		fprintf(stderr, "no name for uid %d?\n", uid);
		exit(1);
	}
	return(pwdinfo->pw_name);
}

/*
 * Do general cleanup.
 */
cleanup()
{
	if (unlink(atfile) == -1)
		perror(atfile);
	exit(1);
}

/*
 * Print usage info and exit.
 */
usage()
{
	fprintf(stderr,"usage: at [-csm] time [date] [filename]\n");
	exit(1);
}


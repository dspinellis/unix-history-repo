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
static char sccsid[] = "@(#)atrun.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 *	Synopsis: atrun
 *
 *
 *	Run jobs created by at(1)
 *
 *
 *	Modifications by:	Steve Wall
 *				Computer Systems Research Group
 *				University of California @ Berkeley
 *
 */
# include <stdio.h>
# include <sys/types.h>
# include <sys/dir.h>
# include <sys/file.h>
# include <sys/time.h>
# include <sys/param.h>
# include <sys/quota.h>
# include <sys/stat.h>
# include <pwd.h>

# define ATDIR		"/usr/spool/at"		/* spooling area */
# define TMPDIR		"/tmp"			/* area for temporary files */
# define MAILER		"/bin/mail"		/* program to use for sending
						   mail */
# define NORMAL		0			/* job exited normally */
# define ABNORMAL	1			/* job exited abnormally */
# define PASTDIR	"/usr/spool/at/past"	/* area to run jobs from */
# define LASTFILE	"/usr/spool/at/lasttimedone"	/* update time file */


char nowtime[11];			/* time it is right now (yy.ddd.hhmm) */
char errfile[25];			/* file where we redirect errors to */


main(argc, argv)
char **argv;
{

	int i;				/* for loop index */
	int numjobs;			/* number of jobs to be run */
	int should_be_run();		/* should a job be run? */
	struct direct **jobqueue;	/* queue of jobs to be run */


	/*
	 * Move to the spooling area.
	 */
	chdir(ATDIR);

	/*
	 * Create a filename that represents the time it is now. This is used
	 * to determine if the execution time for a job has arrived.
	 */
	makenowtime(nowtime);

	/*
	 * Create a queue of the jobs that should be run.
	 */
	if ((numjobs = scandir(".",&jobqueue,should_be_run, 0)) < 0) {
		perror(ATDIR);
		exit(1);
	}

	/*
	 * If there are jobs to be run, run them.
	 */
	if (numjobs > 0) {
		for (i = 0; i < numjobs; ++i) {
			run(jobqueue[i]->d_name);
		}
	}

	/*
	 * Record the last update time.
	 */
	updatetime();

}

/*
 * Create a string with the syntax yy.ddd.hhmm that represents the
 * time it is right now. This string is used to determine whether a
 * job should be run.
 */
makenowtime(nowtime)
char *nowtime;
{
	struct tm *now;			/* broken down representation of the
					   time it is right now */
	struct timeval time;		/* number of seconds since 1/1/70 */
	struct timezone zone;		/* time zone we're in (NOT USED) */

	/*
	 * Get the time of day.
	 */
	if (gettimeofday(&time,&zone) < 0) {
		perror("gettimeofday");
		exit(1);
	}

	/*
	 * Get a broken down representation of the time it is right now.
	 */
	now = localtime(&time.tv_sec);

	/*
	 * Create a string to be used in determining whether or not a job
	 * should be run. The syntax is yy.ddd.hhmm .
	 */
	sprintf(nowtime,"%d.%03d.%02d%02d",now->tm_year,
					   now->tm_yday,
					   now->tm_hour,
					   now->tm_min);
	return;
}

/*
 * Run a job.
 */
run(spoolfile)
char *spoolfile;
{
	int i;				/* scratch variable */
	int pid;			/* process id of forked shell */
	int exitstatus;			/* exit status of the job */
	int notifybymail;		/* should we notify the owner of the
					   job after the job is run? */
	char shell[4];			/* shell to run the job under */
	char *getname();		/* get a uname from using a uid */
	char mailvar[4];		/* send mail variable ("yes" or "no") */
	char runfile[100];		/* file sent to forked shell for exec-
					   ution */
	char owner[16];			/* owner of job we're going to run */
	char jobname[100];		/* name of job we're going to run */
	char whichshell[100];		/* which shell should we fork off? */
	struct passwd *pwdbuf;		/* password info of the owner of job */
	struct stat errbuf;		/* stats on error file */
	struct stat jobbuf;		/* stats on job file */
	FILE *infile;			/* I/O stream to spoolfile */


	/*
	 * First we fork a child so that the main can run other jobs.
	 */
	if (pid = fork())
		return;

	/*
	 * Open the spoolfile.
	 */
	if ((infile = fopen(spoolfile,"r")) == NULL) {
		perror(spoolfile);
		exit(1);
	}

	/*
	 * Grab the 3-line header out of the spoolfile.
	 */
	fscanf(infile,"# owner: %s\n",owner);
	fscanf(infile,"# jobname: %s\n",jobname);
	fscanf(infile,"# shell: %s\n",shell);
	fscanf(infile,"# notify by mail: %s\n",mailvar);

	/*
	 * Check to see if we should send mail to the owner.
	 */
	notifybymail = (strcmp(mailvar, "yes") == 0);
	fclose(infile);

	/*
	 * Change the ownership of the spoolfile from "daemon" to the owner
	 * of the job.
	 */
	pwdbuf = getpwnam(owner);
	if (chown(spoolfile,pwdbuf->pw_uid,pwdbuf->pw_gid) == -1) {
		perror(spoolfile);
		exit(1);
	}

	/*
	 * Move the spoolfile to the directory where jobs are run from and
	 * then move into that directory.
	 */
	sprintf(runfile,"%s/%s",PASTDIR,spoolfile);
	rename(spoolfile, runfile);
	chdir(PASTDIR);

	/*
	 * Create a temporary file where we will redirect errors to.
	 * Just to make sure we've got a unique file, we'll run an "access"
	 * check on the file.
	 */
	for (i = 0; i <= 1000; i += 2) {
		sprintf(errfile,"%s/at.err%d",TMPDIR,(getpid() + i));

		if (access(errfile, F_OK))
			break;

		if (i == 1000) {
			fprintf(stderr, "couldn't create errorfile.\n");
			exit(1);
		}
	}

	/*
	 * Get the stats of the job being run.
	 */
	if (stat(runfile, &jobbuf) == -1) {
		perror(runfile);
		exit(1);
	}

	/*
	 * Fork another child that will run the job.
	 */
	if (pid = fork()) {

		/*
		 * If the child fails, save the job so that it gets
		 * rerun the next time "atrun" is executed and then exit.
		 */
		if (pid == -1) {
			chdir(ATDIR);
			rename(runfile, spoolfile);
			exit(1);
		}

		/*
		 * Wait for the child to terminate.
		 */
		wait((int *)0);

		/*
		 * Get the stats of the error file and determine the exit
		 * status of the child. We assume that if there is anything
		 * in the error file then the job ran into some errors.
		 */
		if (stat(errfile,&errbuf) != 0) {
			perror(errfile);
			exit(1);
		}
		exitstatus = ((errbuf.st_size == 0) ? NORMAL : ABNORMAL);

		/* If errors occured, then we send mail to the owner
		 * telling him/her that we ran into trouble.  
		 *
		 * (NOTE: this could easily be modified so that if any 
		 * errors occured while running a job, mail is sent regard-
		 * less of whether the -m flag was set or not.
		 *
		 * i.e. rather than:
		 *
		 *	"if (notifybymail)" use
		 * use:
		 *
		 *	"if ((exitstatus == ABNORMAL) || (notifybymail))"
		 *
		 * It's up to you if you want to implement this.
		 *
		 */ 
		if (notifybymail)
			sendmailto(getname(jobbuf.st_uid),jobname,exitstatus);

		/*
		 * Remove the errorfile and the jobfile.
		 */
		if (unlink(errfile) == -1)
			perror(errfile);
		if (unlink(runfile) == -1)
			perror(runfile);

		exit(0);
	}

	/*
	 * HERE'S WHERE WE SET UP AND FORK THE SHELL.
	 */

	/*
	 * Run the job as the owner of the jobfile
	 */
	quota(Q_SETUID,jobbuf.st_uid,0,0);
	setgid(jobbuf.st_gid);
	initgroups(getname(jobbuf.st_uid),jobbuf.st_gid);
	setuid(jobbuf.st_uid);

	/*
	 * Close all open files so that we can reopen a temporary file
	 * for stdout and sterr.
	 */
	for (i = getdtablesize(); --i >= 0;)
		close(i);

	/*
	 * Reposition stdin, stdout, and stderr.
	 *
	 *	stdin  = /dev/null
	 *	stout  = /dev/null
	 *	stderr = /tmp/at.err{pid}
	 *	
	 */
	open("/dev/null", 0);
	open("/dev/null", 1);
	open(errfile,O_CREAT|O_WRONLY,00644);

	/*
	 * Now we fork the shell.
	 *
	 * See if the shell is in /bin
	 */
	sprintf(whichshell,"/bin/%s",shell);
	execl(whichshell,shell,runfile, 0);

	/*
	 * If not in /bin, look for the shell in /usr/bin.
	 */
	sprintf(whichshell,"/usr/bin/%s",shell);
	execl(whichshell,shell,runfile, 0);

	/*
	 * If not in /bin, look for the shell in /usr/new.
	 */
	sprintf(whichshell,"/usr/new/%s",shell);
	execl(whichshell,shell,runfile, 0);

	/*
	 * If we don't succeed by now, we're really having troubles,
	 * so we'll send the owner some mail.
	 */
	fprintf(stderr, "%s: Can't execl shell\n",shell);
	exit(1);
}

/*
 * Send mail to the owner of the job. 
 */
sendmailto(user,jobname,exitstatus)
char *user;
char *jobname;
int exitstatus;
{
	char ch;			/* scratch variable */
	char mailtouser[100];		/* the process we use to send mail */
	FILE *mailptr;			/* I/O stream to the mail process */
	FILE *errptr;			/* I/O stream to file containing error
					   messages */
	FILE *popen();			/* initiate I/O to a process */


	/*
	 * Create the full name for the mail process.
	 */
	sprintf(mailtouser,"%s %s",MAILER, user);

	/*
	 * Open a stream to the mail process.
	 */
	if ((mailptr = popen(mailtouser,"w")) == NULL) {
		perror(MAILER);
		exit(1);
	}

	/*
	 * Send the letter. If the job exited normally, just send a
	 * quick letter notifying the owner that everthing went ok.
	 */
	if (exitstatus == NORMAL) {
		fprintf(mailptr,"Your job \"%s\" was run without ",jobname);
		fprintf(mailptr,"any errors.\n");
	}

	/*
	 * If the job exited abnormally, send a letter notifying the user
	 * that the job didn't run proberly. Also, send a copy of the errors 
	 * that occured to the user.
	 */
	else {
		if (exitstatus == ABNORMAL) {

			/*
			 * Write the intro to the letter.
			 */
			fprintf(mailptr,"\n\nThe job you submitted to at, ");
			fprintf(mailptr,"\"%s\", ",jobname);
			fprintf(mailptr,"exited abnormally.\nA list of the ");
			fprintf(mailptr," errors that occured follows:\n\n\n");

			/*
			 * Open the file containing a log of the errors that
			 * occured.
			 */
			if ((errptr = fopen(errfile,"r")) == NULL) {
				perror(errfile);
				exit(1);
			}

			/*
			 * Send the copy of the errors to the owner.
			 */
			fputc('\t',mailptr);
			while ((ch = fgetc(errptr)) != EOF) {
				fputc(ch,mailptr);
				if (ch == '\n')
					fputc('\t',mailptr);
			}
			fclose(errptr);
		}
	}

	/*
	 * Sign the letter.
	 */
	fprintf(mailptr,"\n\n-----------------\n");
	fprintf(mailptr,"The Atrun Program\n");

	/*
	 * Close the stream to the mail process.
	 */
	pclose(mailptr);
	return;
}

/*
 * Do we want to include a file in the job queue? (used by "scandir") 
 * We are looking for files whose "value" (its name) is less than or 
 * equal to the time it is right now (represented by "nowtime").
 * We'll only consider files with three dots in their name since these
 * are the only files that represent jobs to be run.
 */
should_be_run(direntry)
struct direct *direntry;
{
	int numdot = 0;			/* number of dots found in a filename */
	char *filename;			/* pointer for scanning a filename */


	filename = direntry->d_name;

	/*
	 * Count the number of dots found in the directory entry.
	 */
	while (*filename)
		numdot += (*(filename++) == '.');

	/*
	 * If the directory entry doesn't represent a job, just return a 0.
	 */
	if (numdot != 3)
		return(0);

	/*
	 * If a directory entry represents a job, determine if it's time to
	 * run it.
	 */
	return(strncmp(direntry->d_name, nowtime,11) <= 0);
}

/*
 * Record the last time that "atrun" was run.
 */
updatetime()
{

	struct timeval time;		/* number of seconds since 1/1/70 */
	struct timezone zone;		/* time zone we're in (NOT USED) */
	FILE *lastimefile;		/* file where recored is kept */

	/*
	 * Get the time of day.
	 */
	if (gettimeofday(&time,&zone) < 0) {
		perror("gettimeofday");
		exit(1);
	}

	/*
	 * Open the record file.
	 */
	if ((lastimefile = fopen(LASTFILE, "w")) == NULL) {
		fprintf(stderr, "can't update lastfile: ");
		perror(LASTFILE);
		exit(1);
	}

	/*
	 * Record the last update time (in seconds since 1/1/70).
	 */
	fprintf(lastimefile, "%d\n", (u_long) time.tv_sec);

	/*
	 * Close the record file.
	 */
	fclose(lastimefile);
}

/*
 * Get the full login name of a person using his/her user id.
 */
char *
getname(uid)
int uid;
{
	struct passwd *pwdinfo;			/* password info structure */
	

	if ((pwdinfo = getpwuid(uid)) == 0) {
		perror(uid);
		exit(1);
	}
	return(pwdinfo->pw_name);
}

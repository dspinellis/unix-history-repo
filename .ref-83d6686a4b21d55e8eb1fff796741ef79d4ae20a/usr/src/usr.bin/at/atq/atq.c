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
static char sccsid[] = "@(#)atq.c	5.6 (Berkeley) %G%";
#endif not lint

/*
 *
 *	Synopsis:  atq [ -c ] [ -n ] [ name ... ]
 *
 *
 *	Print the queue of files waiting to be executed. These files 
 *	were created by using the "at" command and are located in the 
 *	directory "/usr/spool/at".
 *
 *
 *	Author: Steve Wall
 *		Computer Systems Research Group
 *		University of California @ Berkeley
 *
 */

# include <stdio.h>
# include <sys/types.h>
# include <sys/file.h>
# include <sys/dir.h>
# include <sys/stat.h>
# include <sys/time.h>
# include <pwd.h>
# include <ctype.h>
# include "pathnames.h"

/*
 * Months of the year
 */
static char *mthnames[12] = {
	"Jan","Feb","Mar","Apr","May","Jun","Jul",
	"Aug","Sep","Oct","Nov","Dec",
};

char *nullentry = NULL;			/* avoid 'namelist' NULL ptr problems */
int numentries;				/* number of entries in spooling area */
int namewanted = 0;			/* only print jobs belonging to a 
					   certain person */
struct direct **queue;			/* the queue itself */


main(argc,argv)
int argc;
char **argv;
{

	int cflag = 0;			/* print in order of creation time */
	int nflag = 0;			/* just print the number of jobs in 
					   queue */
	int usage();			/* print usage info and exit */
	int creation();			/* sort jobs by date of creation */
	int alphasort();		/* sort jobs by date of execution */
	int filewanted();		/* should a file be included in queue?*/
	int printqueue();		/* print the queue */
	int countfiles();		/* count the number of files in queue
					   for a given person */
	char **namelist = &nullentry;	/* array of specific name(s) requested*/


	--argc, ++argv;

	/*
	 * Interpret command line flags if they exist.
	 */
	while (argc > 0 && **argv == '-') {
		(*argv)++;
		while (**argv) switch (*(*argv)++) {

			case 'c' :	cflag++; 
					break;

			case 'n' :	nflag++; 
					break;

			default	 :	usage();

		}
		--argc, ++argv;
	}

	/*
	 * If a certain name (or names) is requested, set a pointer to the
	 * beginning of the list.
	 */
	if (argc > 0) {
		++namewanted;
		namelist = argv;
	}

	/*
	 * Move to the spooling area and scan the directory, placing the
	 * files in the queue structure. The queue comes back sorted by
	 * execution time or creation time.
	 */
	if (chdir(_PATH_ATDIR) == -1) {
		perror(_PATH_ATDIR);
		exit(1);
	}
	if ((numentries = scandir(".",&queue,filewanted, (cflag) ? creation : 
				alphasort)) < 0) {
		perror(_PATH_ATDIR);
		exit(1);
	}

	/*
	 * Either print a message stating:
	 *
	 *	1) that the spooling area is empty.
	 *	2) the number of jobs in the spooling area.
	 *	3) the number of jobs in the spooling area belonging to 
	 *	   a certain person.
	 *	4) that the person requested doesn't have any files in the
	 *	   spooling area.
	 *
	 * or send the queue off to "printqueue" for printing.
	 *
	 * This whole process might seem a bit elaborate, but it's worthwhile
	 * to print some informative messages for the user.
	 *
	 */
	if ((numentries == 0) && (!nflag)) {
		printf("no files in queue.\n");
		exit(0);
	}
	if (nflag) {
		printf("%d\n",(namewanted) ? countfiles(namelist) : numentries);
		exit(0);
	}
	if ((namewanted) && (countfiles(namelist) == 0)) {
		printf("no files for %s.\n", (argc == 1) ?
					*argv : "specified users");
		exit(0);
	}
	printqueue(namelist);
	exit(0);
}

/*
 * Count the number of jobs in the spooling area owned by a certain person(s).
 */
countfiles(namelist)
char **namelist;
{
	int i;					/* for loop index */
	int entryfound;				/* found file owned by user(s)*/
	int numfiles = 0;			/* number of files owned by a
						   certain person(s) */
	char **ptr;				/* scratch pointer */


	/*
	 * For each file in the queue, see if the user(s) own the file. We
	 * have to use "entryfound" (rather than simply incrementing "numfiles")
	 * so that if a person's name appears twice on the command line we 
	 * don't double the number of files owned by him/her.
	 */
	for (i = 0; i < numentries ; i++) {
		ptr = namelist;
		entryfound = 0;

		while (*ptr) {
			if (isowner(*ptr,queue[i]->d_name))
				++entryfound;
			++ptr;
		}
		if (entryfound)
			++numfiles;
	}
	return(numfiles);
}

/*
 * Print the queue. If only jobs belonging to a certain person(s) are requested,
 * only print jobs that belong to that person(s).
 */
printqueue(namelist)
char **namelist;
{
	int i;					/* for loop index */
	int rank = 1;				/* rank of a job */
	int entryfound;				/* found file owned by user(s)*/
	int printrank();			/* print the rank of a job */
	int plastrun();				/* print the last time the 
						   spooling area was updated */
	int powner();				/* print the name of the owner
						   of the job */
	char **ptr;				/* scratch pointer */
	struct stat stbuf;			/* buffer for file stats */


	/*
	 * Print the time the spooling area was last modified and the header
	 * for the queue.
	 */
	plastrun();
	printf(" Rank	  Execution Date     Owner     Job #   Job Name\n");

	/*
	 * Print the queue. If a certain name(s) was requested, print only jobs
	 * belonging to that person(s), otherwise print the entire queue.
	 * Once again, we have to use "entryfound" (rather than simply 
	 * comparing each command line argument) so that if a person's name 
	 * appears twice we don't print each file owned by him/her twice.
	 *
	 *
	 * "printrank", "printdate", and "printjobname" all take existing 
	 * data and display it in a friendly manner.
	 *
	 */
	for (i = 0; i < numentries; i++) {
		if ((stat(queue[i]->d_name, &stbuf)) < 0) {
			continue;
		}
		if (namewanted) {
			ptr = namelist;
			entryfound = 0;

			while (*ptr) {
				if (isowner(*ptr,queue[i]->d_name))
					++entryfound;
				++ptr;
			}
			if (!entryfound)
				continue;
		}
		printrank(rank++);
		printdate(queue[i]->d_name);
		powner(queue[i]->d_name);
		printf("%5d",stbuf.st_ino);
		printjobname(queue[i]->d_name);
	}
	++ptr;
}

/*
 * See if "name" owns "job".
 */
isowner(name,job)
char *name;
char *job;
{
	char buf[128];			/* buffer for 1st line of spoolfile 
					   header */
	FILE *infile;			/* I/O stream to spoolfile */

	if ((infile = fopen(job,"r")) == NULL) {
		fprintf(stderr,"Couldn't open spoolfile ");
		perror(job);
		return(0);
	}

	if (fscanf(infile,"# owner: %127s%*[^\n]\n",buf) != 1) {
		fclose(infile);
		return(0);
	}

	fclose(infile);
	return((strcmp(name,buf) == 0) ? 1 : 0);
}

/*
 * Print the owner of the job. This is stored on the first line of the
 * spoolfile. If we run into trouble getting the name, we'll just print "???".
 */
powner(file)
char *file;
{
	char owner[10];				/* the owner */
	FILE *infile;				/* I/O stream to spoolfile */

	/*
	 * Open the job file and grab the first line.
	 */

	if ((infile = fopen(file,"r")) == NULL) {
		printf("%-10.9s","???");
		perror(file);
		return;
	}

	if (fscanf(infile,"# owner: %9s%*[^\n]\n",owner) != 1) {
		printf("%-10.9s","???");
		fclose(infile);
		return;
	}

	fclose(infile);
	printf("%-10.9s",owner);

}
	
/*
 * Print the time the spooling area was updated.
 */
plastrun()
{
	struct timeval now;			/* time it is right now */
	struct timezone zone;			/* NOT USED */
	struct tm *loc;				/* detail of time it is right */
	u_long lasttime;			/* last update time in seconds
						   since 1/1/70 */
	FILE *last;				/* file where last update hour
						   is stored */


	/*
	 * Open the file where the last update time is stored, and grab the
	 * last update hour. The update time is measured in seconds since
	 * 1/1/70.
	 */
	if ((last = fopen(_PATH_LASTFILE,"r")) == NULL) {
		perror(_PATH_LASTFILE);
		exit(1);
	}
	fscanf(last,"%lu",&lasttime);
	fclose(last);

	/*
	 * Get a broken down representation of the last update time.
	 */
	loc = localtime(&lasttime);

	/*
	 * Print the time that the spooling area was last updated.
	 */
	printf("\n LAST EXECUTION TIME: %s ",mthnames[loc->tm_mon]);
	printf("%d, 19%d ",loc->tm_mday,loc->tm_year);
	printf("at %d:%02d\n\n",loc->tm_hour,loc->tm_min);
}

/*
 * Print the rank of a job. (I've got to admit it, I stole it from "lpq")
 */
static 
printrank(n)
{
	static char *r[] = {
		"th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th"
	};

	if ((n/10) == 1)
		 printf("%3d%-5s", n,"th");
	else
		 printf("%3d%-5s", n, r[n%10]);
}

/*
 * Print the date that a job is to be executed. This takes some manipulation 
 * of the file name.
 */
printdate(filename)
char *filename;
{
	int yday  =  0;				/* day of year file will be 
						   executed */
	int min	  =  0;				/* min. file will be executed */
	int hour  =  0;				/* hour file will be executed */
	int day	  =  0;				/* day file will be executed */
	int month =  0;				/* month file will be executed*/
	int year  =  0;				/* year file will be executed */
	int get_mth_day();			/* convert a day of year to a
						   month and day of month */
	char date[19];				/* reformatted execution date */

	/*
	 * Pick off the necessary info from the file name and convert the day
	 * of year to a month and day of month.
	 */
	sscanf(filename,"%2d.%3d.%2d%2d",&year,&yday,&hour,&min);
	get_mth_day(year,yday,&month,&day);

	/*
	 * Format the execution date of a job.
	 */
	sprintf(date,"%3s %2d, 19%2d %02d:%02d",mthnames[month],
						    day, year,hour,min);

	/*
	 * Print the date the job will be executed.
	 */
	printf("%-21.18s",date);
}

/*
 * Given a day of the year, calculate the month and day of month.
 */
get_mth_day(year,dayofyear,month,day)
int year, dayofyear, *month, *day;

{

	int i = 1;				/* for loop index */
	int leap;				/* are we dealing with a leap
						   year? */
						/* Table of the number of days 
						   in each month of the year.

						     dofy_tab[1] -- regular year
						     dofy_tab[2] -- leap year 
									      */

	static int dofy_tab[2][13] = {
		{ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
		{ 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
	};

	/*
	 * Are we dealing with a leap year?
	 */
	leap = ((year%4 == 0 && year%100 != 0) || year%100 == 0);

	/*
	 * Calculate the month of the year and day of the month.
	 */
	while (dayofyear >= dofy_tab[leap][i]) {
		dayofyear -= dofy_tab[leap][i++];
		++(*month);
	}
	*day = (dayofyear + 1);
}
	
/*
 * Print a job name. If the old "at" has been used to create the spoolfile,
 * the three line header that the new version of "at" puts in the spoolfile.
 * Thus, we just print "???".
 */
printjobname(file)
char *file;
{
	char *ptr;				/* scratch pointer */
	char jobname[28];			/* the job name */
	FILE *filename;				/* job file in spooling area */

	/*
	 * Open the job file and grab the second line.
	 */
	printf("   ");

	if ((filename = fopen(file,"r")) == NULL) {
		printf("%.27s\n", "???");
		perror(file);
		return;
	}
	/*
	 * Skip over the first line.
	 */
	fscanf(filename,"%*[^\n]\n");

	/*
	 * Now get the job name.
	 */
	if (fscanf(filename,"# jobname: %27s%*[^\n]\n",jobname) != 1) {
		printf("%.27s\n", "???");
		fclose(filename);
		return;
	}
	fclose(filename);

	/*
	 * Put a pointer at the begining of the line and remove the basename
	 * from the job file.
	 */
	ptr = jobname;
	if ((ptr = (char *)rindex(jobname,'/')) != 0)
		++ptr;
	else 
		ptr = jobname;

	if (strlen(ptr) > 23)
		printf("%.23s ...\n",ptr);
	else
		printf("%.27s\n",ptr);
}

/*
 * Do we want to include a file in the queue? (used by "scandir") We are looking
 * for files with following syntax: yy.ddd.hhhh. so the test is made to see if 
 * the file name has three dots in it. This test will suffice since the only
 * other files in /usr/spool/at don't have any dots in their name.
 */
filewanted(direntry)
struct direct *direntry;
{
	int numdot = 0;
	char *filename;

	filename = direntry->d_name;
	while (*filename)
		numdot += (*(filename++) == '.');
	return(numdot == 3);
}

/*
 * Sort files by time of creation. (used by "scandir")
 */
creation(d1, d2)
struct direct **d1, **d2;
{
	struct stat stbuf1, stbuf2;

	if (stat((*d1)->d_name,&stbuf1) < 0)
		return(1);

	if (stat((*d2)->d_name,&stbuf2) < 0)
		return(1);

	return(stbuf1.st_ctime < stbuf2.st_ctime);
}
	
/*
 * Print usage info and exit.
 */
usage() 
{
	fprintf(stderr,"usage:	atq [-c] [-n] [name ...]\n");
	exit(1);
}

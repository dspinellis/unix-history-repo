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
static char sccsid[] = "@(#)atrm.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 *	synopsis: atrm [-f] [-i] [-] [[job #] [user] ...]
 *
 *
 *	Remove files from the directory /usr/spool/at. These files
 *	represent jobs to be run at a later date.
 *
 *	Author: Steve Wall
 *		Computer Systems Research Group
 *		University of California @ Berkeley
 *
 */

#include <stdio.h>
#include <pwd.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/file.h>
#include <sys/stat.h>

#define SUPERUSER	0			/* is user super-user? */
#define MAXENTRIES	1000			/* max # of entries allowed */
#define ATDIR		"/usr/spool/at"		/* spooling area */


int user;					/* person requesting removal */
int fflag = 0;					/* suppress announcements? */
int iflag = 0;					/* run interactively? */

main(argc,argv)
int argc;
char **argv;

{
	int i;				/* for loop index */
	int userid;			/* uid of owner of file */
	int isuname;			/* is a command line argv a user name?*/
	int numjobs;			/* # of jobs in spooling area */
	int usage();			/* print usage info and exit */
	int allflag = 0;		/* remove all jobs belonging to user? */
	int jobexists;			/* does a requested job exist? */
	int alphasort();		/* sort jobs by date of execution */
	int filewanted();		/* should a file be listed in queue? */
	char *getname();		/* get a user name from a uid */
	struct stat *statptr;		/* pointer to file stat structure */
	struct stat *stbuf[MAXENTRIES]; /* array of pointers to stat structs */
	struct direct **namelist;	/* names of jobs in spooling area */


	/*
	 * If job number, user name, or "-" is not specified, just print
	 * usage info and exit.
	 */
	if (argc < 2)
		usage();

	--argc; ++argv;

	/*
	 * Process command line flags.
	 */
	while (**argv == '-') {
		(*argv)++;
		switch (**argv) {

			case '\0':	++allflag;
					break;

			case 'f':	++fflag;
					break;
					
			case 'i':	++iflag;
					break;
					
			default:	usage();
		}
		++argv; --argc;
	}

	/*
	 * If all jobs are to be removed and extra command line arguments 
	 * are given, print usage info and exit.
	 */
	if (allflag && argc) 
		usage();

	/*
	 * If only certain jobs are to be removed and no job #'s or user
	 * names are specified, print usage info and exit.
	 */
	if (!allflag && !argc)
		usage();

	/*
	 * If interactive removal and quiet removal are requested, override
	 * quiet removal and run interactively.
	 */
	if (iflag && fflag)
		fflag = 0;

	/* 
	 * Move to spooling area and get user id of person requesting removal.
	 */
	if (chdir(ATDIR) == -1) {
		perror(ATDIR);
		exit(1);
	}
	user = getuid();

	/*
	 * Get a list of the files in the spooling area.
	 */
	if ((numjobs = scandir(".",&namelist,filewanted,alphasort)) < 0) {
		perror(ATDIR);
		exit(1);
	}

	/*
	 * Build an array of pointers to the file stats for all jobs in
	 * the spooling area.
	 */
	for (i = 0; i < numjobs; ++i) { 
		statptr = (struct stat *) malloc(sizeof(struct stat));
		if (statptr == NULL) {
			perror("malloc");
			exit(1);
		}
		if (stat(namelist[i]->d_name,statptr) < 0) {
			perror("stat");
			continue;
		}
		stbuf[i] = statptr;
	}

	/*
	 * If all jobs belonging to the user are to be removed, compare
	 * the user's id to the owner of the file. If they match, remove
	 * the file. If the user is the super-user, don't bother comparing
	 * the id's. After all files are removed, exit (status 0).
	 */
	if (allflag) {
		for (i = 0; i < numjobs; ++i) { 
			if (user == SUPERUSER || isowner(getname(user),
							namelist[i]->d_name)) 
				removentry(namelist[i]->d_name,
						(int)stbuf[i]->st_ino,
							user);
		}
		exit(0);
	}

	/*
	 * If only certain jobs are to be removed, interpret each command
	 * line argument. A check is done to see if it is a user's name or
	 * a job number (inode #). If it's a user's name, compare the argument
	 * to the files owner. If it's a job number, compare the argument to
	 * the inode number of the file. In either case, if a match occurs,
	 * try to remove the file. (The function "isusername" scans the
	 * argument to see if it is all digits which we will assume means 
	 * that it's a job number (a fairly safe assumption?). This is done
	 * because we have to determine whether we are dealing with a user
	 * name or a job number. By assuming that only arguments that are
	 * all digits is a job number, we allow users to have digits in
	 * their login name i.e. "johndoe2").
	 */

	while (argc--) {
		jobexists = 0;
		isuname = isusername(*argv);
		for (i = 0; i < numjobs; ++i) {

			/* 
			 * if argv is a username, compare his/her uid to
			 * the uid of the owner of the file......
			 */
			if (isuname) {
				if (!isowner(*argv,namelist[i]->d_name))
					continue;

			/*
			 * otherwise, we assume that the argv is a job # and
			 * thus compare argv to the inode (job #) of the file.
			 */
			} else {
				if (stbuf[i]->st_ino != atoi(*argv)) 
					continue;
			}
			++jobexists;
			removentry(namelist[i]->d_name, (int)stbuf[i]->st_ino,
					user);
		}

		/*
		 * If a requested argument doesn't exist, print a message.
		 */
		if (!jobexists && !fflag && !isuname) {
			printf("%6s: no such job number\n", *argv);
		}
		++argv;
	}
	exit(0);
}

/*
 * Print usage info and exit.
 */
usage()
{
	fprintf(stderr,"usage: atrm [-f] [-i] [-] [[job #] [user] ...]\n");
	exit(1);
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
	int numdot = 0;			/* number of dots in a filename */
	char *filename;			/* filename we are looking at */

	filename = direntry->d_name;
	while (*filename)
		numdot += (*(filename++) == '.');
	return(numdot == 3);
}

/*
 * Is a command line argument a username? As noted above we will assume 
 * that an argument that is all digits means that it's a job number, not
 * a user's name. We choose to determine whether an argument is a user name
 * in this manner because then it's ok for someone to have digits in their 
 * user name.
 */
isusername(string)
char *string;
{
	char *ptr;			/* pointer used for scanning string */

	ptr = string;
	while (isdigit(*ptr))
		++ptr;
	return((*ptr == '\0') ? 0 : 1);
}

/*
 * Remove an entry from the queue. The access of the file is checked for
 * write permission (since all jobs are mode 644). If access is granted,
 * unlink the file. If the fflag (suppress announcements) is not set,
 * print the job number that we are removing and the result of the access
 * check (either "permission denied" or "removed"). If we are running 
 * interactively (iflag), prompt the user before we unlink the file. If 
 * the super-user is removing jobs, inform him/her who owns each file before 
 * it is removed.
 */
removentry(filename,inode,user)
char *filename;
int inode;
int user;
{

	if (!fflag)
		printf("%6d: ",inode);

	if (!isowner(getname(user),filename) && user != SUPERUSER) {

		if (!fflag) {
			printf("permission denied\n");
		}

	} else {
		if (iflag) {
			if (user == SUPERUSER) {
				printf("\t(owned by ");
				powner(filename);
				printf(")");
			}
			printf("remove it? ");
			if (!yes())
				return;
		}
		if (unlink(filename) < 0) {
			if (!fflag) {
				fputs("FATAL ERROR (unlink fails): ",stdout);
				fflush(stdout);
				perror(filename);
			}
			return;
		}
		if (!fflag && !iflag)
			printf("removed\n");
	}
}

/*
 * See if "name" owns "job".
 */
isowner(name,job)
char *name;
char *job;
{
	char buf[30];			/* buffer for 1st line of spoolfile 
					   header */
	FILE *infile;			/* I/O stream to spoolfile */

	if ((infile = fopen(job,"r")) == NULL) {
		fprintf(stderr,"Couldn't open spoolfile");
		perror(job);
		return(0);
	}

	if (fscanf(infile,"# owner: %s\n",buf) != 1) {
		fclose(infile);
		return(0);
	}

	close(infile);
	return((strcmp(name,buf) == 0) ? 1 : 0);
}

/*
 * Print the owner of the job. This is stored on the first line of the
 * spoolfile. If we run into trouble getting the name, we'll just print "???".
 */
powner(file)
char *file;
{
	char owner[80];				/* the owner */
	FILE *infile;				/* I/O stream to spoolfile */

	/*
	 * Open the job file and grab the first line.
	 */

	if ((infile = fopen(file,"r")) == NULL) {
		printf("%s","???");
		perror(file);
		return;
	}

	if (fscanf(infile,"# owner: %s",owner) != 1) {
		printf("%s","???");
		perror(file);
		fclose(infile);
		return;
	}

	fclose(infile);
	printf("%s",owner);

}

/*
 * Get answer to interactive prompts, eating all characters beyond the first
 * one. If a 'y' is typed, return 1.
 */
yes()
{
	char ch;				/* dummy variable */
	char ch1;				/* dummy variable */

	ch = ch1 = getchar();
	while (ch1 != '\n' && ch1 != EOF)
		ch1 = getchar();
	return(ch == 'y');
}

/*
 * Get the uid of a person using his/her login name. Return -1 if no
 * such account name exists.
 */
getid(name)
char *name;
{

	struct passwd *pwdinfo;		/* password info structure */
	
	if ((pwdinfo = getpwnam(name)) == 0)
		return(-1);

	return(pwdinfo->pw_uid);
}

/*
 * Get the full login name of a person using his/her user id.
 */
char *
getname(uid)
int uid;
{
	struct passwd *pwdinfo;			/* password info structure */
	

	if ((pwdinfo = getpwuid(uid)) == 0)
		return("???");
	return(pwdinfo->pw_name);
}


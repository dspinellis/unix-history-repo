#ifndef lint
static char sccsid[] = "@(#)uuclean.c	5.7	2/24/88";
#endif

#include <signal.h>
#include "uucp.h"
#include <pwd.h>
#include <sys/stat.h>
#ifdef	NDIR
#include "ndir.h"
#else
#include <sys/dir.h>
#endif

/*
 *
 *	this program will search through the spool
 *	directory (Spool) and delete all files with a requested
 *	prefix which are older than (nomtime) seconds.
 *	If the -m option is set, the program will try to
 *	send mail to the usid of the file.
 *
 *	options:
 *		-m  -  send mail for deleted file
 *		-d  -  directory to clean
 *		-n  -  time to age files before delete (in hours)
 *		-p  -  prefix for search
 *		-x  -  turn on debug outputs
 *	exit status:
 *		0  -  normal return
 *		1  -  can not read directory
 */

#define NOMTIME 72	/* hours to age files before deletion */

int checkprefix = 0;
struct timeb Now;

main(argc, argv)
char *argv[];
{
	register DIR *dirp;
	register struct direct *dentp;
	time_t nomtime, ptime;
	struct stat stbuf;
	int mflg = 0;

	strcpy(Progname, "uuclean");
	uucpname(Myname);
	nomtime = NOMTIME * (time_t)3600;

	while (argc>1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'd':
			Spool = &argv[1][2];
			break;
		case 'm':
			mflg = 1;
			break;
		case 'n':
			nomtime = atoi(&argv[1][2]) * (time_t)3600;
			break;
		case 'p':
			checkprefix = 1;
			if (&argv[1][2] != '\0')
				stpre(&argv[1][2]);
			break;
		case 'x':
			chkdebug();
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			break;
		default:
			printf("unknown flag %s\n", argv[1]); break;
		}
		--argc;  argv++;
	}

	DEBUG(4, "DEBUG# %s\n", "START");
	if (chdir(Spool) < 0) {	/* NO subdirs in uuclean!  rti!trt */
		printf("%s directory inaccessible\n", Spool);
		exit(1);
	}

	if ((dirp = opendir(Spool)) == NULL) {
		printf("%s directory unreadable\n", Spool);
		exit(1);
	}

	time(&ptime);
	while (dentp = readdir(dirp)) {
		if (checkprefix && !chkpre(dentp->d_name))
			continue;

		if (stat(dentp->d_name, &stbuf) == -1) {
			DEBUG(4, "stat on %s failed\n", dentp->d_name);
			continue;
		}


		if ((stbuf.st_mode & S_IFMT) == S_IFDIR)
			continue;
		if ((ptime - stbuf.st_mtime) < nomtime)
			continue;
		if (dentp->d_name[0] == CMDPRE)
			notfyuser(dentp->d_name);
		DEBUG(4, "unlink file %s\n", dentp->d_name);
		unlink(dentp->d_name);
		if (mflg)
			sdmail(dentp->d_name, stbuf.st_uid);
	}

	closedir(dirp);
	exit(0);
}


#define MAXPRE 10
char Pre[MAXPRE][NAMESIZE];
int Npre = 0;
/***
 *	chkpre(file)	check for prefix
 *	char *file;
 *
 *	return codes:
 *		0  -  not prefix
 *		1  -  is prefix
 */

chkpre(file)
char *file;
{
	int i;

	for (i = 0; i < Npre; i++) {
		if (prefix(Pre[i], file))
			return(1);
		}
	return(0);
}

/***
 *	stpre(p)	store prefix
 *	char *p;
 *
 *	return codes:  none
 */

stpre(p)
char *p;
{
	if (Npre < MAXPRE - 2)
		strcpy(Pre[Npre++], p);
	return;
}

/***
 *	notfyuser(file)	- notfiy requestor of deleted requres
 *
 *	return code - none
 */

notfyuser(file)
char *file;
{
	FILE *fp;
	int numrq;
	char frqst[100], lrqst[100];
	char msg[BUFSIZ];
	char *args[10];

	if ((fp = fopen(file, "r")) == NULL)
		return;
	if (fgets(frqst, 100, fp) == NULL) {
		fclose(fp);
		return;
	}
	numrq = 1;
	while (fgets(lrqst, 100, fp))
		numrq++;
	fclose(fp);
	sprintf(msg,
	  "File %s delete. \nCould not contact remote. \n%d requests deleted.\n", file, numrq);
	if (numrq == 1) {
		strcat(msg, "REQUEST: ");
		strcat(msg, frqst);
	} else {
		strcat(msg, "FIRST REQUEST: ");
		strcat(msg, frqst);
		strcat(msg, "\nLAST REQUEST: ");
		strcat(msg, lrqst);
	}
	getargs(frqst, args, 10);
	mailst(args[3], msg, CNULL);
}


/***
 *	sdmail(file, uid)
 *
 *	sdmail  -  this routine will determine the owner
 *	of the file (file), create a message string and
 *	call "mailst" to send the cleanup message.
 *	This is only implemented for local system
 *	mail at this time.
 */

sdmail(file, uid)
char *file;
{
	static struct passwd *pwd;
	struct passwd *getpwuid();
	char mstr[40];

	sprintf(mstr, "uuclean deleted file %s\n", file);
	if (pwd != NULL && pwd->pw_uid == uid) {
		mailst(pwd->pw_name, mstr, CNULL);
		return;
	}

	setpwent();
	if ((pwd = getpwuid(uid)) != NULL)
		mailst(pwd->pw_name, mstr, CNULL);
}

cleanup(code)
int code;
{
	exit(code);
}

#ifndef lint
static char sccsid[] = "@(#)selsub.c	4.5	(Berkeley)	%G%";
#endif not lint

#include "stdio.h"
#include "sys/types.h"
#include "sys/file.h"
#include "sys/stat.h"
#include "lrnref.h"

char learnrc[100];

selsub(argc,argv)
char *argv[];
{
	char ans1[100];
	static char ans2[40];
	static char dirname[40];
	static char subname[40];
	FILE *fp;
	char *getenv();
	char *home;

	if (argc > 1 && argv[1][0] == '-') {
		direct = argv[1]+1;
		argc--;
		argv++;
	}
	if (chdir(direct) != 0) {
		perror(direct);
		fprintf(stderr, "Selsub:  couldn't cd to non-standard directory\n");
		exit(1);
	}
	sname = argc > 1 ? argv[1] : 0;
	if (argc > 2) {
		strcpy (level=ans2, argv[2]);
		if (strcmp(level, "-") == 0)	/* no lesson name is - */
			ask = 1;
		else if (strcmp(level, "0") == 0)
			level = 0;
		else
			again = 1;	/* treat as if "again" lesson */
	}
	else
		level = 0;
	if (argc > 3 )
		speed = atoi(argv[3]);
	if ((home = getenv("HOME")) != NULL) {
		sprintf(learnrc, "%s/.learnrc", home);
		if ((fp=fopen(learnrc, "r")) != NULL) {
			char xsub[40], xlev[40]; int xsp;
			fscanf(fp, "%s %s %d", xsub, xlev, &xsp);
			fclose(fp);
			if (*xsub && *xlev && xsp >= 0	/* all read OK */
			    && (argc == 2 && strcmp(sname, xsub) == 0
			      || argc <= 1)) {
				strcpy(sname = subname, xsub);
				strcpy(level = ans2, xlev);
				speed = xsp;
				again = 1;
	printf("[ Taking up where you left off last time:  learn %s %s.\n",
		sname, level);
	printf("%s\n  \"rm $HOME/.learnrc\", and re-enter with \"learn %s\". ]\n",
		"  To start this sequence over leave learn by typing \"bye\", then",
		sname);
			}
		}
	}
	if (!sname) {
		printf("These are the available courses -\n");
		list("Linfo");
		printf("If you want more information about the courses,\n");
		printf("or if you have never used 'learn' before,\n");
		printf("press RETURN; otherwise type the name of\n");
		printf("the course you want, followed by RETURN.\n");
		fflush(stdout);
		if (gets(sname=subname) == NULL)
			exit(0);
		if (sname[0] == '\0') {
			list("Xinfo");
			do {
				printf("\nWhich subject?  ");
				fflush(stdout);
				if (gets(sname=subname) == NULL)
					exit(0);
			} while (sname[0] == '\0');
		}
	}
	chknam(sname);
	total = cntlessons(sname);
	if (!level) {
		printf("If you were in the middle of this subject\n");
		printf("and want to start where you left off, type\n");
		printf("the last lesson number the computer printed.\n");
		printf("If you don't know the number, type in a word\n");
		printf("you think might appear in the lesson you want,\n");
		printf("and I will look for the first lesson containing it.\n");
		printf("To start at the beginning, just hit RETURN.\n");
		fflush(stdout);
		if (gets(ans2) == NULL)
			exit(0);
		if (ans2[0]==0)
			strcpy(ans2,"0");
		else
			again = 1;
		level=ans2;
		getlesson();
	}

	/* make new directory for user to play in */
	if (chdir("/tmp") != 0) {
		perror("/tmp");
		fprintf(stderr, "Selsub:  couldn't cd to public directory\n");
		exit(1);
	}
	sprintf(dir=dirname, "pl%da", getpid());
	sprintf(ans1, "mkdir %s", dir);
	system(ans1);
	if (chdir(dir) < 0) {
		perror(dir);
		fprintf(stderr, "Selsub:  couldn't make play directory with %s.\nBye.\n", ans1);
		exit(1);
	}
	/* after this point, we have a working directory. */
	/* have to call wrapup to clean up */
	(void)sprintf(ans1, "%s/%s/Init", direct, sname);
	if (access(ans1, R_OK)==0) {
		(void)sprintf(ans1, "%s/%s/Init %s", direct, sname, level);
		if (system(ans1) != 0) {
			printf("Leaving learn.\n");
			wrapup(1);
		}
	}
}

chknam(name)
char *name;
{
	if (access(name, 05) < 0) {
		printf("Sorry, there is no subject or lesson named %s.\nBye.\n", name);
		exit(1);
	}
}

#ifndef DIR
#include <sys/dir.h>
#endif

cntlessons(sname)	/* return number of entries in lesson directory; */
char *sname;		/* approximate at best since I don't count L0, Init */
{			/* and lessons skipped by good students */
#if BSD4_2
	struct direct dbuf;
	register struct direct *ep = &dbuf;	/* directory entry pointer */
	int n = 0;
	DIR *dp;

	if ((dp = opendir(sname)) == NULL) {
		perror(sname);
		wrapup(1);
	}
	for (ep = readdir(dp); ep != NULL; ep = readdir(dp)) {
		if (ep->d_ino != 0)
			n++;
	}
	closedir(dp);
	return n - 2;				/* minus . and .. */
#else
	struct stat statbuf;

	stat(sname, &statbuf);
	return statbuf.st_size / 16 - 2;
#endif
}

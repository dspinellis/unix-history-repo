#include "stdio.h"
#include "lrnref"

selsub(argc,argv)
char *argv[];
{
	char ans1[100], *cp;
	static char ans2[30];
	static char dirname[20];
	static char subname[20];

	if (argc > 1 && argv[1][0] == '-') {
		direct = argv[1]+1;
		argc--;
		argv++;
	}
	chknam(direct);
	if (chdir(direct) != 0) {
		fprintf(stderr, "can't cd to %s\,", direct);
		exit(1);
	}
	sname = argc > 1 ? argv[1] : 0;
	if (argc > 2)
		strcpy (level=ans2, argv[2]);
	else
		level = 0;
	if (argc > 3 )
		speed = atoi(argv[3]);
	if (!sname) {
		printf("These are the available courses -\n");
		list("Linfo");
		printf("If you want more information about the courses,\n");
		printf("or if you have never used 'learn' before,\n");
		printf("type 'return'; otherwise type the name of\n");
		printf("the course you want, followed by 'return'.\n");
		fflush(stdout);
		gets(sname=subname);
		if (sname[0] == '\0') {
			list("Xinfo");
			do {
				printf("\nWhich subject?  ");
				fflush(stdout);
				gets(sname=subname);
			} while (sname[0] == '\0');
		}
	}
	chknam(sname);
	if (!level) {
		printf("If you were in the middle of this subject\n");
		printf("and want to start where you left off, type\n");
		printf("the last lesson number the computer printed.\n");
		printf("To start at the beginning, just hit return.\n");
		fflush(stdout);
		gets(ans2);
		if (ans2[0]==0)
			strcpy(ans2,"0");
		for (cp=ans2; *cp; cp++)
			if (*cp == '(' || *cp == ' ')
				*cp= 0;
		level=ans2;
	}

	/* make new directory for user to play in */
	if (chdir("play") != 0) {
		fprintf(stderr, "can't cd to playpen\n");
		exit(1);
	}
	sprintf(dir=dirname, "pl%da", getpid());
	sprintf(ans1, "mkdir %s", dir);
	system(ans1);
	if (chdir(dir) < 0) {
		fprintf(stderr, "Couldn't create working directory.\nBye.\n");
		exit(1);
	}
	/* after this point, we have a working directory. */
	/* have to call wrapup to clean up */
	if (access(sprintf(ans1, "%s/%s/Init", direct, sname), 04)==0)
		if (system(sprintf(ans1, "%s/%s/Init %s", direct,sname, level)) != 0) {
			printf("Leaving learn.\n");
			wrapup(1);
		}
	if (level[0] == '-')	/* no lesson names start with - */
		ask = 1;
	start(level);
}

chknam(name)
char *name;
{
	if (access(name, 05) < 0) {
		printf("Sorry, there is no subject or lesson named %s.\nBye.\n", name);
		exit(1);
	}
}

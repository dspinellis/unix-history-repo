#include "uucp.h"
#include "uucpdefs.h"
#include <sys/types.h>
#include <sys/stat.h>

/*
 *	uucp
 */

int Uid;
int Startjob = 1;
char Path[100], Optns[10];
char Grade = 'n';
int Copy = 1;

main(argc, argv)
char *argv[];
{
	int ret;
	char *sysfile1, *sysfile2, *cp;
	char file1[MAXFULLNAME], file2[MAXFULLNAME];
	extern char *index();

	Optns[0] = '-';
	Optns[1] = '\0';
	while(argc>1 && argv[1][0] == '-'){
		switch(argv[1][1]){
		case 'c':
			Copy = 0;
			break;
		case 'd':
			strcat(Optns, "d");
			break;
		case 'e':
			fprintf(stderr, "-e option removed\n");
			break;
		case 'g':
			Grade = argv[1][2]; break;
		case 'm':
			strcat(Optns, "m");
			break;
		case 'r':
			Startjob = 0;
			break;
		case 's':
			Spool = &argv[1][2]; break;
		case 'x':
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			break;
		default:
			printf("unknown flag %s\n", argv[1]); break;
		}
		--argc;  argv++;
	}
	DEBUG(4, "\n\n** %s **\n", "START");
	ret = gwd(Wrkdir);
	ASSERT(ret == 0, "GWD FAILED %d", ret);
	chdir(Spool);

	Uid = getuid();
	ret = guinfo(Uid, User, Path);
	ASSERT(ret == 0, "CAN NOT FIND UID %d\n", Uid);
	DEBUG(4, "UID %d, ", Uid);
	DEBUG(4, "User %s,", User);
	DEBUG(4, "PATH %s\n", Path);
	if (argc < 3) {
		fprintf(stderr, "usage uucp from ... to\n");
		cleanup(0);
	}


	/*  set up "to" system and file names  */
	if ((cp = index(argv[argc - 1], '!')) != NULL) {
		sysfile2 = argv[argc - 1];
		*cp = '\0';
		if (*sysfile2 == '\0')
			sysfile2 = Myname;
		else
			sprintf(Rmtname, "%.7s", sysfile2);
		if (versys(sysfile2) != 0) {
			fprintf(stderr, "bad system name: %s\n", sysfile2);
			cleanup(0);
		}
		strcpy(file2, cp + 1);
	}
	else {
		sysfile2 = Myname;
		strcpy(file2, argv[argc - 1]);
	}


	/*  do each from argument  */
	while (argc > 2) {
		if ((cp = index(argv[1], '!')) != NULL) {
			sysfile1 = argv[1];
			*cp = '\0';
			if (*sysfile1 == '\0')
				sysfile1 = Myname;
			else
				sprintf(Rmtname, "%.7s", sysfile1);
			if (versys(sysfile1) != 0) {
				fprintf(stderr, "bad system name: %s\n", sysfile1);
				cleanup(0);
			}
			strcpy(file1, cp + 1);
		}
		else {
			sysfile1 = Myname;
			strcpy(file1, argv[1]);
		}
		DEBUG(4, "file1 - %s\n", file1);
		copy(sysfile1, file1, sysfile2, file2);
		--argc;
		argv++;
	}

	if (Startjob)
		xuucico("");
	cleanup(0);
}

cleanup(code)
int code;
{
	logcls();
	rmlock(NULL);
	exit(code);
}


/***
 *	copy(s1, f1, s2, f2)	generate copy files
 *	char *s1, *f1, *s2, *f2;
 *
 *	return codes 0  |  FAIL
 */

copy(s1, f1, s2, f2)
char *s1, *f1, *s2, *f2;
{
	int ret, type;
	struct stat stbuf;
	char cfile[NAMESIZE], dfile[NAMESIZE];
	char file1[MAXFULLNAME], file2[MAXFULLNAME];
	FILE *cfp;
	extern char *index();

	type = 0;
	strcpy(file1, f1);
	strcpy(file2, f2);
	if (strcmp(s1, Myname) != SAME)
		type = 1;
	if (strcmp(s2, Myname) != SAME)
		type += 2;
	if (type & 01)
		if ((index(file1, '*') != NULL
		  || index(file1, '?') != NULL
		  || index(file1, '[') != NULL))
			type = 4;
	switch (type) {
	case 0:
		/* all work here */
		DEBUG(4, "all work here %d\n", type);
		expfile(file1);
		expfile(file2);
		if (chkpth(User, "", file1) != 0
		|| chkpth(User, "", file2) != 0) {
			fprintf(stderr, "permission denied\n");
			cleanup(1);
		}
		xcp(file1, file2);
		logent("WORK HERE", "DONE");
		return(0);
	case 1:
		/* receive file */
		DEBUG(4, "receive file - %d\n", type);
		if (file1[0] != '~')
			expfile(file1);
		expfile(file2);
		if (chkpth(User, "", file2) != 0) {
			fprintf(stderr, "permission denied\n");
			return(FAIL);
		}
		gename(CMDPRE, s1, Grade, cfile);
		strcpy(dfile, cfile);
		dfile[0] = DATAPRE;
		cfp = fopen(cfile, "w");
		ASSERT(cfp != NULL, "CAN NOT OPEN %s", cfile);
		fprintf(cfp, "R %s %s %s %s\n", file1, file2, User, Optns);
		fclose(cfp);
		break;
	case 2:
		/* send file */
		expfile(file1);
		if (file2[0] != '~')
			expfile(file2);
		DEBUG(4, "send file - %d\n", type);

		gename(CMDPRE, s2, Grade, cfile);
		strcpy(dfile, cfile);
		dfile[0] = DATAPRE;
		if (chkpth(User, "", file1) != 0) {
			fprintf(stderr, "permission denied %s\n", file1);
			return(FAIL);
		}
		ret = stat(file1, &stbuf);
		if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
			fprintf(stderr, "directory name illegal - %s\n",
			  file1);
			return(FAIL);
		}
		if (Copy) {
			if (xcp(file1, dfile) != 0) {
				fprintf(stderr, "can't copy %s\n", file1);
				return(FAIL);
			}
			chmod(dfile, 0666);
		}
		else {
			if ((stbuf.st_mode & 04) == 0) {
				fprintf(stderr, "uucico can't access %s (-c specified)\n", file1);
				return(FAIL);
			}
		}
		cfp = fopen(cfile, "w");
		ASSERT(cfp != NULL, "CAN NOT OPEN %s", cfile);
		chmod(cfile, 0200);
		fprintf(cfp, "S %s %s %s %s %s %o\n", file1, file2,
			User, Optns, dfile, stbuf.st_mode & 0777);
		fclose(cfp);
		chmod(cfile, 0666);
		break;
	case 3:
	case 4:
		/*  send uucp command for execution on s2  */
		DEBUG(4, "send uucp command - %d\n", type);
		if (strcmp(s2,  Myname) == SAME) {
			expfile(file2);
			if (chkpth(User, "", file2) != 0) {
				fprintf(stderr, "permission denied\n");
				return(FAIL);
			}
		}
		gename(CMDPRE, s1, Grade, cfile);
		cfp = fopen(cfile, "w");
		ASSERT(cfp != NULL, "CAN NOT OPEN %s", cfile);
		fprintf(cfp, "X %s %s!%s\n", file1, s2, file2);
		fclose(cfp);
		break;
	}
	logent(cfile, "QUEUED");
	return(0);
}

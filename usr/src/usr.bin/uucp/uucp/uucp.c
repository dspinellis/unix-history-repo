#ifndef lint
static char sccsid[] = "@(#)uucp.c	5.4 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <sys/stat.h>
#include "uust.h"

/*
 *	uucp
 */
int Uid;
char *Ropt = " ";
char Path[100], Optns[10], Ename[MAXBASENAME+1];
char Grade = 'n';
#ifdef DONTCOPY
int Copy = 0;
#else !DONTCOPY
int Copy = 1;
#endif !DONTCOPY
char Nuser[32];

/* variables used to check if talking to more than one system. */
int	xsflag = -1;
char	xsys[MAXBASENAME+1] = 0;

long Nbytes = 0;
#define MAXBYTES 50000	/* maximun number of bytes of data per C. file */
#define MAXCOUNT 15	/* maximun number of files per C. file */

main(argc, argv)
char *argv[];
{
	int ret;
	char *sysfile1, *sysfl2;
	register char *cp;
	char file1[MAXFULLNAME], file2[MAXFULLNAME];
	int avoidgwd = 0;

	strcpy(Progname, "uucp");
	uucpname(Myname);
	umask(WFMASK);
	Optns[0] = '-';
	Optns[1] = 'd';
#ifdef DONTCOPY
	Optns[2] = 'c';
#else !DONTCOPY
	Optns[2] = 'C';
#endif !DONTCOPY
	Ename[0] = Nuser[0] = Optns[3] = '\0';
	while(argc>1 && argv[1][0] == '-'){
		switch(argv[1][1]){
		case 'a':
			/* efficiency hack; avoid gwd call */
			avoidgwd = 1;
			break;
		case 'C':
			Copy = 1;
			Optns[2] = 'C';
			break;
		case 'c':
			Copy = 0;
			Optns[2] = 'c';
			break;
		case 'd':
			break;
		case 'f':
			Optns[1] = 'f';
			break;
		case 'e':
			strncpy(Ename, &argv[1][2], MAXBASENAME);
			break;
		case 'g':
			Grade = argv[1][2];
			break;
		case 'm':
			strcat(Optns, "m");
			break;
		case 'n':
			sprintf(Nuser, "%.31s", &argv[1][2]);
			break;
		case 'r':
			Ropt = argv[1];
			break;
		case 's':
			Spool = &argv[1][2]; break;
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
	DEBUG(4, "\n\n** %s **\n", "START");
	if (!avoidgwd) {
		cp = getwd(Wrkdir);
		ASSERT(cp != 0, "GETWD FAILED", Wrkdir, cp);
	}
	ret = subchdir(Spool);
	ASSERT(ret >= 0, "CHDIR FAILED", Spool, ret);

	Uid = getuid();
	ret = guinfo(Uid, User, Path);
	ASSERT(ret == 0, "CAN NOT FIND UID", CNULL, Uid);
	DEBUG(4, "UID %d, ", Uid);
	DEBUG(4, "User %s,", User);
	DEBUG(4, "Ename (%s) ", Ename);
	DEBUG(4, "PATH %s\n", Path);
	if (argc < 3) {
		fprintf(stderr, "usage uucp from ... to\n");
		cleanup(1);
	}


	/*  set up "to" system and file names  */
	if ((cp = index(argv[argc - 1], '!')) != NULL) {
		sysfl2 = argv[argc - 1];
		*cp = '\0';
		if (*sysfl2 == '\0')
			sysfl2 = Myname;
		else
			strncpy(Rmtname, sysfl2, MAXBASENAME);
		if (versys(&sysfl2) != 0) {
			fprintf(stderr, "bad system name: %s\n", sysfl2);
			cleanup(1);
		}
		/* block multi-hop requests immediately */
		if (index(cp+1, '!') != NULL) {
			fprintf(stderr, "uucp handles only adjacent sites.\n");
			fprintf(stderr, "Try uusend for multi-hop delivery.\n");
			cleanup(1);
		}
		strcpy(file2, cp + 1);
	}
	else {
		sysfl2 = Myname;
		strcpy(file2, argv[argc - 1]);
	}
	if (strlen(sysfl2) > MAXBASENAME)
		sysfl2[MAXBASENAME] = '\0';


	/*  do each from argument  */
	while (argc > 2) {
		if ((cp = index(argv[1], '!')) != NULL) {
			sysfile1 = argv[1];
			*cp = '\0';
			if (strlen(sysfile1) > MAXBASENAME)
				sysfile1[MAXBASENAME] = '\0';
			if (*sysfile1 == '\0')
				sysfile1 = Myname;
			else
				strncpy(Rmtname, sysfile1, MAXBASENAME);
			if (versys(&sysfile1) != 0) {
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
		copy(sysfile1, file1, sysfl2, file2);
		--argc;
		argv++;
	}

	clscfile();
	if (*Ropt != '-' && xsflag >= 0)
		xuucico(xsys);
	cleanup(0);
}

cleanup(code)
int code;
{
	logcls();
	rmlock(CNULL);
	if (code)
		fprintf(stderr, "uucp failed. code %d\n", code);
	exit(code);
}


/*
 *	generate copy files
 *
 *	return codes 0  |  FAIL
 */

copy(s1, f1, s2, f2)
register char *s1, *f1, *s2, *f2;
{
	int type, statret;
	struct stat stbuf, stbuf1;
	char dfile[NAMESIZE];
	char file1[MAXFULLNAME], file2[MAXFULLNAME];
	FILE *cfp, *gtcfile();
	char opts[100];

	type = 0;
	opts[0] = '\0';
	strcpy(file1, f1);
	strcpy(file2, f2);
	if (strcmp(s1, Myname) != SAME)
		type = 1;
	if (strcmp(s2, Myname) != SAME)
		type += 2;
	if (type & 01)
		if ((index(f1, '*') != NULL
		  || index(f1, '?') != NULL
		  || index(f1, '[') != NULL))
			type = 4;

	switch (type) {
	case 0:
		/* all work here */
		DEBUG(4, "all work here %d\n", type);
		if (ckexpf(file1))
			 return FAIL;
		if (ckexpf(file2))
			 return FAIL;
		if (stat(subfile(file1), &stbuf) != 0) {
			fprintf(stderr, "can't get file status %s \n copy failed\n",
			  file1);
			return SUCCESS;
		}
		statret = stat(subfile(file2), &stbuf1);
		if (statret == 0
		  && stbuf.st_ino == stbuf1.st_ino
		  && stbuf.st_dev == stbuf1.st_dev) {
			fprintf(stderr, "%s %s - same file; can't copy\n", file1, file2);
			return SUCCESS;
		}
		if (chkpth(User, "", file1) != 0
		  || chkperm(file2, index(Optns, 'd'))
		  || chkpth(User, "", file2) != 0) {
			fprintf(stderr, "permission denied\n");
			cleanup(1);
		}
		if ((stbuf.st_mode & ANYREAD) == 0) {
			fprintf(stderr, "can't read file (%s) mode (%o)\n",
			  file1, (int)stbuf.st_mode);
			return FAIL;
		}
		if (statret == 0 && (stbuf1.st_mode & ANYWRITE) == 0) {
			fprintf(stderr, "can't write file (%s) mode (%o)\n",
			  file2, (int)stbuf.st_mode);
			return FAIL;
		}
		xcp(file1, file2);
		logent("WORK HERE", "DONE");
		return SUCCESS;
	case 1:
		/* receive file */
		DEBUG(4, "receive file - %d\n", type);
		chsys(s1);
		if (file1[0] != '~')
			if (ckexpf(file1))
				 return FAIL;
		if (ckexpf(file2))
			 return FAIL;
		if (chkpth(User, "", file2) != 0) {
			fprintf(stderr, "permission denied\n");
			return FAIL;
		}
		if (Ename[0] != '\0') {
			/* execute uux - remote uucp */
			xuux(Ename, s1, file1, s2, file2, opts);
			return SUCCESS;
		}

		cfp = gtcfile(s1);
		fprintf(cfp, "R %s %s %s %s\n", file1, file2, User, Optns);
		break;
	case 2:
		/* send file */
		if (ckexpf(file1))
			 return FAIL;
		if (file2[0] != '~')
			if (ckexpf(file2))
				 return FAIL;
		DEBUG(4, "send file - %d\n", type);
		chsys(s2);

		if (chkpth(User, "", file1) != 0) {
			fprintf(stderr, "permission denied %s\n", file1);
			return FAIL;
		}
		if (stat(subfile(file1), &stbuf) != 0) {
			fprintf(stderr, "can't get status for file %s\n", file1);
			return FAIL;
		}
		if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
			fprintf(stderr, "directory name illegal - %s\n",
			  file1);
			return FAIL;
		}
		if ((stbuf.st_mode & ANYREAD) == 0) {
			fprintf(stderr, "can't read file (%s) mode (%o)\n",
			  file1, (int)stbuf.st_mode);
			return FAIL;
		}
		if ((Nuser[0] != '\0') && (index(Optns, 'n') == NULL))
			strcat(Optns, "n");
		if (Ename[0] != '\0') {
			/* execute uux - remote uucp */
			if (Nuser[0] != '\0')
				sprintf(opts, "-n%s", Nuser);
			xuux(Ename, s1, file1, s2, file2, opts);
			return SUCCESS;
		}
		Nbytes +=  stbuf.st_size;
		if (Copy) {
			gename(DATAPRE, Myname, Grade, dfile);
			if (xcp(file1, dfile) != 0) {
				fprintf(stderr, "can't copy %s\n", file1);
				return FAIL;
			}
		}
		else {
			/* make a dummy D. name */
			/* cntrl.c knows names < 6 chars are dummy D. files */
			strcpy(dfile, "D.0");
		}
		cfp = gtcfile(s2);
		fprintf(cfp, "S %s %s %s %s %s %o %s\n", file1, file2,
			User, Optns, dfile, (int)stbuf.st_mode & 0777, Nuser);
		break;
	case 3:
	case 4:
		/*  send uucp command for execution on s1  */
		DEBUG(4, "send uucp command - %d\n", type);
		chsys(s1);
		if (strcmp(s2,  Myname) == SAME) {
			if (ckexpf(file2))
				 return FAIL;
			if (chkpth(User, "", file2) != 0) {
				fprintf(stderr, "permission denied\n");
				return FAIL;
			}
		}
		if (Ename[0] != '\0') {
			/* execute uux - remote uucp */
			xuux(Ename, s1, file1, s2, file2, opts);
			return SUCCESS;
		}
		cfp = gtcfile(s1);
		fprintf(cfp, "X %s %s!%s %s %s\n", file1, s2, file2, User, Optns);
		break;
	}
	return SUCCESS;
}

/*
 *	execute uux for remote uucp
 *
 *	return code - none
 */

xuux(ename, s1, f1, s2, f2, opts)
char *ename, *s1, *s2, *f1, *f2, *opts;
{
	char cmd[200];

	DEBUG(4, "Ropt(%s) ", Ropt);
	DEBUG(4, "ename(%s) ", ename);
	DEBUG(4, "s1(%s) ", s1);
	DEBUG(4, "f1(%s) ", f1);
	DEBUG(4, "s2(%s) ", s2);
	DEBUG(4, "f2(%s)\n", f2);
	sprintf(cmd, "uux %s %s!uucp %s %s!%s \\(%s!%s\\)",
	 Ropt, ename, opts,  s1, f1, s2, f2);
	DEBUG(4, "cmd (%s)\n", cmd);
	system(cmd);
	return;
}

FILE *Cfp = NULL;
char Cfile[NAMESIZE];

/*
 *	get a Cfile descriptor
 *
 *	return an open file descriptor
 */

FILE *
gtcfile(sys)
register char *sys;
{
	static char presys[8] = "";
	static int cmdcount = 0;
	register int savemask;

	if (strcmp(presys, sys) != SAME  /* this is !SAME on first call */
	  || Nbytes > MAXBYTES
	  || ++cmdcount > MAXCOUNT) {
		cmdcount = 1;
		Nbytes = 0;
		if (presys[0] != '\0') {
			clscfile();
		}
		gename(CMDPRE, sys, Grade, Cfile);
#ifdef VMS
		savemask = umask(~0600); /* vms must have read permission */
#else !VMS
		savemask = umask(~0200);
#endif !VMS
		Cfp = fopen(subfile(Cfile), "w");
		umask(savemask);
		ASSERT(Cfp != NULL, CANTOPEN, Cfile, 0);
		strcpy(presys, sys);
	}
	return Cfp;
}

/*
 *	close cfile
 *
 *	return code - none
 */

clscfile()
{
	if (Cfp == NULL)
		return;
	fclose(Cfp);
	chmod(subfile(Cfile), ~WFMASK & 0777);
	logent(Cfile, "QUE'D");
	US_CRS(Cfile);
	Cfp = NULL;
}

/*
 * compile a list of all systems we are referencing
 */
chsys(s1)
register char *s1;
{
	if (xsflag < 0)
		xsflag = 0;
	else if (xsflag > 0)
		return;

	if (xsys[0] == '\0') {
		strncpy(xsys, s1, MAXBASENAME);
		return;
	}

	if (strncmp(xsys, s1, MAXBASENAME) == SAME)
		return;

	xsflag++;
	xsys[0] = '\0';
}

#ifndef lint
static char sccsid[] = "@(#)uux.c	5.1 (Berkeley) 7/2/83";
#endif

/*
 * Grade option "-g<g>" added. See cbosgd.2611 (Mark Horton)
 * no-copy option "-c" added. Suggested by Steve Bellovin
 * "-l" is synonym for "-c".
 * "X" files use local system name, avoids conflict. Steve Bellovin
 */
#include "uucp.h"

#define NOSYSPART 0
#define HASSYSPART 1

#define APPCMD(d) {\
char *p;\
for (p = d; *p != '\0';) *cmdp++ = *p++;\
*cmdp++ = ' ';\
*cmdp = '\0';}

#define GENSEND(f, a, b, c, d, e) {\
fprintf(f, "S %s %s %s -%s %s 0666\n", a, b, c, d, e);\
}
#define GENRCV(f, a, b, c) {\
fprintf(f, "R %s %s %s - \n", a, b, c);\
}


/*
 *	
 */

main(argc, argv)
char *argv[];
{
	char cfile[NAMESIZE];	/* send commands for files from here */
	char dfile[NAMESIZE];	/* used for all data files from here */
	char rxfile[NAMESIZE];	/* to be sent to xqt file (X. ...) */
	char tfile[NAMESIZE];	/* temporary file name */
	char tcfile[NAMESIZE];	/* temporary file name */
	char t2file[NAMESIZE];	/* temporary file name */
	int cflag = 0;		/*  commands in C. file flag  */
	int rflag = 0;		/*  C. files for receiving flag  */
	int Copy = 1;		/* Copy spool files */
	char buf[BUFSIZ];
	char inargs[BUFSIZ];
	int pipein = 0;
	int startjob = 1;
	char Grade = 'A';
	char path[MAXFULLNAME];
	char cmd[BUFSIZ];
	char *ap, *cmdp;
	char prm[BUFSIZ];
	char syspart[8], rest[MAXFULLNAME];
	char xsys[8], local[8];
	FILE *fprx, *fpc, *fpd, *fp;
	extern char *getprm(), *lastpart();
	extern FILE *ufopen();
	int uid, ret;
	char redir = '\0';
	int nonoti = 0;
	int nonzero = 0;
	int orig_uid = getuid();

	strcpy(Progname, "uux");
	uucpname(Myname);
	umask(WFMASK);
	Ofn = 1;
	Ifn = 0;
	while (argc>1 && argv[1][0] == '-') {
		switch(argv[1][1]){
		case 'p':
		case '\0':
			pipein = 1;
			break;
		case 'r':
			startjob = 0;
			break;
		case 'c':
		case 'l':
			Copy = 0;
			break;
		case 'g':
			Grade = argv[1][2];
			break;
		case 'x':
			chkdebug(orig_uid);
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			break;
		case 'n':
			nonoti = 1;
			break;
		case 'z':
			nonzero = 1;
			break;
		default:
			fprintf(stderr, "unknown flag %s\n", argv[1]);
				break;
		}
		--argc;  argv++;
	}

	DEBUG(4, "\n\n** %s **\n", "START");

	inargs[0] = '\0';
	for (argv++; argc > 1; argc--) {
		DEBUG(4, "arg - %s:", *argv);
		strcat(inargs, " ");
		strcat(inargs, *argv++);
	}
	DEBUG(4, "arg - %s\n", inargs);
	ret = gwd(Wrkdir);
	if (ret != 0) {
		fprintf(stderr, "can't get working directory; will try to continue\n");
		strcpy(Wrkdir, "/UNKNOWN");
	}
	subchdir(Spool);
	uid = getuid();
	guinfo(uid, User, path);

	sprintf(local, "%.7s", Myname);
	cmdp = cmd;
	*cmdp = '\0';
	gename(DATAPRE, local, 'X', rxfile);
	fprx = ufopen(rxfile, "w");
	ASSERT(fprx != NULL, "CAN'T OPEN", rxfile, 0);
	gename(DATAPRE, local, 'T', tcfile);
	fpc = ufopen(tcfile, "w");
	ASSERT(fpc != NULL, "CAN'T OPEN", tcfile, 0);
	fprintf(fprx, "%c %s %s\n", X_USER, User, local);
	if (nonoti)
		fprintf(fprx, "%c\n", X_NONOTI);
	if (nonzero)
		fprintf(fprx, "%c\n", X_NONZERO);

	/* find remote system name */
	ap = inargs;
	xsys[0] = '\0';
	while ((ap = getprm(ap, prm)) != NULL) {
		if (prm[0] == '>' || prm[0] == '<') {
			ap = getprm(ap, prm);
			continue;
		}


		split(prm, xsys, rest);
		break;
	}
	if (xsys[0] == '\0')
		strcpy(xsys, local);
	sprintf(Rmtname, "%.7s", xsys);
	DEBUG(4, "xsys %s\n", xsys);
	if (versys(xsys) != 0) {
		/*  bad system name  */
		fprintf(stderr, "bad system name: %s\n", xsys);
		fclose(fprx);
		fclose(fpc);
		cleanup(EX_NOHOST);
	}

	if (pipein) {
		gename(DATAPRE, local, 'B', dfile);
		fpd = ufopen(dfile, "w");
		ASSERT(fpd != NULL, "CAN'T OPEN", dfile, 0);
		while (!feof(stdin)) {
			ret = fread(buf, 1, BUFSIZ, stdin);
			fwrite(buf, 1, ret, fpd);
		}
		fclose(fpd);
		if (strcmp(local, xsys) != SAME) {
			GENSEND(fpc, dfile, dfile, User, "", dfile);
			cflag++;
		}
		fprintf(fprx, "%c %s\n", X_RQDFILE, dfile);
		fprintf(fprx, "%c %s\n", X_STDIN, dfile);
	}
	/* parse command */
	ap = inargs;
	while ((ap = getprm(ap, prm)) != NULL) {
		DEBUG(4, "prm - %s\n", prm);
		if (prm[0] == '>' || prm[0] == '<') {
			redir = prm[0];
			continue;
		}

		if (prm[0] == ';') {
			APPCMD(prm);
			continue;
		}

		if (prm[0] == '|' || prm[0] == '^') {
			if (cmdp != cmd)
				APPCMD(prm);
			continue;
		}

		/* process command or file or option */
		ret = split(prm, syspart, rest);
		DEBUG(4, "s - %s, ", syspart);
		DEBUG(4, "r - %s, ", rest);
		DEBUG(4, "ret - %d\n", ret);
		if (syspart[0] == '\0')
			strcpy(syspart, local);

		if (cmdp == cmd && redir == '\0') {
			/* command */
			APPCMD(rest);
			continue;
		}

		/* process file or option */
		DEBUG(4, "file s- %s, ", syspart);
		DEBUG(4, "local - %s\n", local);
		/* process file */
		if (redir == '>') {
			if (rest[0] != '~')
				if (ckexpf(rest))
					cleanup(EX_CANTCREAT);
			fprintf(fprx, "%c %s %s\n", X_STDOUT, rest,
			 syspart);
			redir = '\0';
			continue;
		}

		if (ret == NOSYSPART && redir == '\0') {
			/* option */
			APPCMD(rest);
			continue;
		}

		if (strcmp(xsys, local) == SAME
		 && strcmp(xsys, syspart) == SAME) {
			if (ckexpf(rest))
				cleanup(EX_CANTCREAT);
			if (redir == '<')
				fprintf(fprx, "%c %s\n", X_STDIN, rest);
			else
				APPCMD(rest);
			redir = '\0';
			continue;
		}

		if (strcmp(syspart, local) == SAME) {
			/*  generate send file */
			if (ckexpf(rest))
				cleanup(EX_CANTCREAT);
			gename(DATAPRE, local, 'A', dfile);
			DEBUG(4, "rest %s\n", rest);
			if ((chkpth(User, "", rest) || anyread(rest)) != 0) {
				fprintf(stderr, "permission denied %s\n", rest);
				cleanup(EX_NOINPUT);
			}
			if (Copy) {
				if (xcp(rest, dfile) != 0) {
					fprintf(stderr, "can't copy %s to %s\n", rest, dfile);
					cleanup(EX_NOINPUT);
				}
				GENSEND(fpc, rest, dfile, User, "", dfile);
			}
			else {
				GENSEND(fpc, rest, dfile, User, "c", "D.0");
			}
			cflag++;
			if (redir == '<') {
				fprintf(fprx, "%c %s\n", X_STDIN, dfile);
				fprintf(fprx, "%c %s\n", X_RQDFILE, dfile);
			}
			else {
				APPCMD(lastpart(rest));
				fprintf(fprx, "%c %s %s\n", X_RQDFILE,
				 dfile, lastpart(rest));
			}
			redir = '\0';
			continue;
		}

		if (strcmp(local, xsys) == SAME) {
			/*  generate local receive  */
			gename(CMDPRE, syspart, 'R', tfile);
			strcpy(dfile, tfile);
			dfile[0] = DATAPRE;
			fp = ufopen(tfile, "w");
			ASSERT(fp != NULL, "CAN'T OPEN", tfile, 0);
			if (ckexpf(rest))
				cleanup(EX_CANTCREAT);
			GENRCV(fp, rest, dfile, User);
			fclose(fp);
			rflag++;
			if (rest[0] != '~')
				if (ckexpf(rest))
					cleanup(EX_CANTCREAT);
			if (redir == '<') {
				fprintf(fprx, "%c %s\n", X_RQDFILE, dfile);
				fprintf(fprx, "%c %s\n", X_STDIN, dfile);
			}
			else {
				fprintf(fprx, "%c %s %s\n", X_RQDFILE, dfile,
				  lastpart(rest));
				APPCMD(lastpart(rest));
			}

			redir = '\0';
			continue;
		}

		if (strcmp(syspart, xsys) != SAME) {
			/* generate remote receives */
			gename(DATAPRE, syspart, 'R', dfile);
			strcpy(tfile, dfile);
			tfile[0] = CMDPRE;
			fpd = ufopen(dfile, "w");
			ASSERT(fpd != NULL, "CAN'T OPEN", dfile, 0);
			gename(DATAPRE, local, 'T', t2file);
			GENRCV(fpd, rest, t2file, User);
			fclose(fpd);
			GENSEND(fpc, dfile, tfile, User, "", dfile);
			cflag++;
			if (redir == '<') {
				fprintf(fprx, "%c %s\n", X_RQDFILE, t2file);
				fprintf(fprx, "%c %s\n", X_STDIN, t2file);
			}
			else {
				fprintf(fprx, "%c %s %s\n", X_RQDFILE, t2file,
				  lastpart(rest));
				APPCMD(lastpart(rest));
			}
			redir = '\0';
			continue;
		}

		/* file on remote system */
		if (rest[0] != '~')
			if (ckexpf(rest))
				cleanup(EX_CANTCREAT);
		if (redir == '<')
			fprintf(fprx, "%c %s\n", X_STDIN, rest);
		else
			APPCMD(rest);
		redir = '\0';
		continue;

	}

	fprintf(fprx, "%c %s\n", X_CMD, cmd);
	logent(cmd, "XQT QUE'D");
	fclose(fprx);

	strcpy(tfile, rxfile);
	tfile[0] = XQTPRE;
	if (strcmp(xsys, local) == SAME) {
		/* rti!trt: xmv() works across filesystems, link(II) doesnt */
		xmv(rxfile, tfile);
		if (startjob)
			if (rflag)
				xuucico(xsys);
			else
				xuuxqt();
	}
	else {
		GENSEND(fpc, rxfile, tfile, User, "", rxfile);
		cflag++;
	}

	fclose(fpc);
	if (cflag) {
		gename(CMDPRE, xsys, Grade, cfile);
		/* rti!trt: use xmv() rather than link(II) */
		xmv(tcfile, cfile);
		if (startjob)
			xuucico(xsys);
		cleanup(0);
	}
	else
		unlink(subfile(tcfile));
}

#define FTABSIZE 30
char Fname[FTABSIZE][NAMESIZE];
int Fnamect = 0;

/***
 *	cleanup - cleanup and unlink if error
 *
 *	return - none - do exit()
 */

cleanup(code)
int code;
{
	int i;

	logcls();
	rmlock(CNULL);
	if (code) {
		for (i = 0; i < Fnamect; i++)
			unlink(subfile(Fname[i]));
		fprintf(stderr, "uux failed. code %d\n", code);
	}
	DEBUG(1, "exit code %d\n", code);
	exit(code);
}

/***
 *	ufopen - open file and record name
 *
 *	return file pointer.
 */

FILE *ufopen(file, mode)
char *file, *mode;
{
	if (Fnamect < FTABSIZE)
		strcpy(Fname[Fnamect++], file);
	else
		logent("Fname", "TABLE OVERFLOW");
	return(fopen(subfile(file), mode));
}

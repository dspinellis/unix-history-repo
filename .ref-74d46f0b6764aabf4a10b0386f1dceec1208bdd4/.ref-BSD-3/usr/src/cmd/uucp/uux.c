	/*  uux 2.2  5/24/79  18:33:11  */
#include "uucp.h"
#include "uucpdefs.h"

static char SiD[] = "@(#)uux	2.2";

#define NOSYSPART 0
#define HASSYSPART 1

#define APPCMD(d) {\
char *p;\
for (p = d; *p != '\0';) *cmdp++ = *p++;\
*cmdp++ = ' ';\
*cmdp = '\0';}

#define GENSEND(f, a, b, c, d) {\
fprintf(f, "S %s %s %s - %s 0666\n", a, b, c, d);\
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
	char buf[BUFSIZ];
	char inargs[BUFSIZ];
	int pipein = 0;
	int startjob = 1;
	char path[MAXFULLNAME];
	char cmd[BUFSIZ];
	char *ap, *cmdp;
	char prm[BUFSIZ];
	char syspart[8], rest[MAXFULLNAME];
	char xsys[8], local[8];
	FILE *fprx, *fpc, *fpd, *fp;
	FILE *xqtstr();
	extern char *getprm(), *index(), *lastpart();
	int uid, ret;
	char redir = '\0';

	uucpname(Myname);
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
		case 'x':
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			break;
		default:
			sprintf(stderr, "unknown flag %s\n", argv[1]);
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
	chdir(Spool);
	uid = getuid();
	guinfo(uid, User, path);

	sprintf(local, "%.7s", Myname);
	cmdp = cmd;
	*cmdp = '\0';
	gename(DATAPRE, local, 'X', rxfile);
	fprx = fopen(rxfile, "w");
	ASSERT(fprx != NULL, "CAN'T OPEN %s", rxfile);
	chmod(rxfile, 0666);
	gename(DATAPRE, local, 'T', tcfile);
	fpc = fopen(tcfile, "w");
	ASSERT(fpc != NULL, "CAN'T OPEN %s", tcfile);
	chmod(tcfile, 0666);
	fprintf(fprx, "%c %s %s\n", X_USER, User, local);

	/* find remote system name */
	ap = inargs;
	while ((ap = getprm(ap, prm)) != NULL) {
		if (prm[0] == '>' || prm[0] == '<') {
			ap = getprm(ap, prm);
			continue;
		}

		if (prm[0] == ';') {
			APPCMD(prm);
			continue;
		}

		split(prm, xsys, rest);
		if (xsys[0] == '\0')
			strcpy(xsys, local);
		break;
	}
	DEBUG(4, "xsys %s\n", xsys);
	if (versys(xsys) != 0) {
		/*  bad system name  */
		fprintf(stderr, "bad system name: %s\n", xsys);
		fclose(fprx);
		fclose(fpc);
		unlink(rxfile);
		unlink(tcfile);
		cleanup(101);
	}

	if (pipein) {
		gename(DATAPRE, xsys, 'B', dfile);
		fpd = fopen(dfile, "w");
		ASSERT(fpd != NULL, "CAN'T OPEN %s", dfile);
		chmod(dfile, 0666);
		while (fgets(buf, BUFSIZ, stdin) != NULL)
			fputs(buf, fpd);
		fclose(fpd);
		if (strcmp(local, xsys) != SAME) {
			GENSEND(fpc, dfile, dfile, User, dfile);
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
				expfile(rest);
			fprintf(fprx, "%c %s %s\n", X_STDOUT, rest,
			 syspart);
			redir = '\0';
			continue;
		}

		if (ret == NOSYSPART) {
			/* option */
			APPCMD(rest);
			continue;
		}

		if (strcmp(xsys, local) == SAME
		 && strcmp(xsys, syspart) == SAME) {
			expfile(rest);
			if (redir == '<')
				fprintf(fprx, "%c %s\n", X_STDIN, rest);
			else
				APPCMD(rest);
			redir = '\0';
			continue;
		}

		if (strcmp(syspart, local) == SAME) {
			/*  generate send file */
			expfile(rest);
			gename(DATAPRE, xsys, 'A', dfile);
			DEBUG(4, "rest %s\n", rest);
			if ((chkpth(User, "", rest) || anyread(rest)) != 0) {
				fprintf(stderr, "permission denied %s\n", rest);
				cleanup(1);
			}
			if (xcp(rest, dfile) != 0) {
				fprintf(stderr, "can't copy %s to %s\n", rest, dfile);
				cleanup(1);
			}
			GENSEND(fpc, rest, dfile, User, dfile);
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
			fp = fopen(tfile, "w");
			ASSERT(fp != NULL, "CAN'T OPEN %s", tfile);
			chmod(tfile, 0666);
			expfile(rest);
			GENRCV(fp, rest, dfile, User);
			fclose(fp);
			rflag++;
			if (rest[0] != '~')
				expfile(rest);
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
			fpd = fopen(dfile, "w");
			ASSERT(fpd != NULL, "CAN'T OPEN %s", dfile);
			chmod(dfile, 0666);
			gename(DATAPRE, xsys, 'T', t2file);
			GENRCV(fpd, rest, t2file, User);
			fclose(fpd);
			GENSEND(fpc, dfile, tfile, User, dfile);
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
			expfile(rest);
		if (redir == '<')
			fprintf(fprx, "%c %s\n", X_STDIN, rest);
		else
			APPCMD(rest);
		redir = '\0';
		continue;

	}

	fprintf(fprx, "%c %s\n", X_CMD, cmd);
	fclose(fprx);

	strcpy(tfile, rxfile);
	tfile[0] = XQTPRE;
	if (strcmp(xsys, local) == SAME) {
		link(rxfile, tfile);
		unlink(rxfile);
		if (startjob)
			if (rflag)
				xuucico("");
			else
				xuuxqt();
	}
	else {
		GENSEND(fpc, rxfile, tfile, User, rxfile);
		cflag++;
	}

	fclose(fpc);
	if (cflag) {
		gename(CMDPRE, xsys, 'A', cfile);
		link(tcfile, cfile);
		unlink(tcfile);
		if (startjob)
			xuucico(xsys);
		cleanup(0);
	}
	else
		unlink(tcfile);
}


cleanup(code)
int code;
{
	rmlock(NULL);
	DEBUG(1, "exit code %d\n", code);
	exit(code);
}

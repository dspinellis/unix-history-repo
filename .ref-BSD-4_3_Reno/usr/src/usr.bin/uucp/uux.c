#ifndef lint
static char sccsid[] = "@(#)uux.c	5.14	(Berkeley) 10/27/88";
#endif

#include "uucp.h"
#include <sys/stat.h>
#include <sysexits.h>

#define NOSYSPART 0
#define HASSYSPART 1

#define LQUOTE	'('
#define RQUOTE ')'

#define APPCMD(d) {\
register char *p; for (p = d; *p != '\0';)\
	{*cmdp++ = *p++;\
		if(cmdp>(sizeof(cmd)+&cmd[0])){\
			fprintf(stderr,"argument list too long\n");\
			cleanup(EX_SOFTWARE);\
		}\
	}\
	*cmdp++ = ' '; *cmdp = '\0';}

#define GENSEND(f, a, b, c, d, e) {\
	fprintf(f, "S %s %s %s -%s %s 0666\n", a, b, c, d, e); }
#define GENRCV(f, a, b, c) {fprintf(f, "R %s %s %s - \n", a, b, c);}

struct timeb Now;

main(argc, argv)
int argc;
char **argv;
{
	char cfile[NAMESIZE];	/* send commands for files from here */
	char dfile[NAMESIZE];	/* used for all data files from here */
	char rxfile[NAMESIZE];	/* to be sent to xqt file (X. ...) */
	char tfile[NAMESIZE];	/* temporary file name */
	char tcfile[NAMESIZE];	/* temporary file name */
	char t2file[NAMESIZE];	/* temporary file name */
	int cflag = 0;		/*  commands in C. file flag  */
	int rflag = 0;		/*  C. files for receiving flag  */
#ifdef DONTCOPY
	int Copy = 0;		/* Don't Copy spool files */
#else !DONTCOPY
	int Copy = 1;		/* Copy spool files */
#endif !DONTCOPY
	int Linkit = 0;		/* Try link before copy */
	char buf[2*BUFSIZ];
	char inargs[2*BUFSIZ];
	int pipein = 0;
	int startjob = 1;
	char Grade = 'A';
	long Gradedelta = 100000000L;	/* "huge number" */
	long size = 0L;
	char path[MAXFULLNAME];
	char cmd[2*BUFSIZ];
	char *ap, *cmdp;
	char prm[2*BUFSIZ];
	char syspart[MAXBASENAME+1], rest[MAXFULLNAME];
	char Xsys[MAXBASENAME+1], local[MAXBASENAME+1];
	char *xsys = Xsys;
	FILE *fprx, *fpc, *fpd, *fp;
	extern char *getprm(), *lastpart();
	extern FILE *ufopen();
	int uid, ret, c;
	char redir = '\0';
	int nonoti = 0;
	int nonzero = 0;
	int link_failed;
	char *ReturnTo = NULL;
	extern int LocalOnly;
	extern char *optarg;
	extern int optind;

	strcpy(Progname, "uux");
	uucpname(Myname);
	umask(WFMASK);
	Ofn = 1;
	Ifn = 0;
#ifdef	VMS
	arg_fix(argc, argv);
#endif
	while (((c = getopt(argc, argv, "-prclCg:x:nzLa:")) != EOF) ||
	    (optind < argc && (c = *argv[optind]) == '-' && ++optind))
		switch (c) {
		case '-':
			/* FALLTHROUGH */
		case 'p':
			pipein = 1;
			break;
		case 'r':
			startjob = 0;
			break;
		case 'c':
			Copy = 0;
			Linkit = 0;
			break;
		case 'l':
			Copy = 0;
			Linkit = 1;
			break;
		case 'C':
			Copy = 1;
			Linkit = 0;
			break;
		case 'g':
			Grade = *optarg;
			Gradedelta = atol(optarg+1);
			break;
		case 'x':
			chkdebug();
			Debug = atoi(optarg);
			if (Debug <= 0)
				Debug = 1;
			break;
		case 'n':
			nonoti = 1;
			break;
		case 'z':
			nonzero = 1;
			break;
		case 'L':
			LocalOnly++;
			break;
		case 'a':
			ReturnTo = optarg;
			if (prefix(Myname, ReturnTo) && ReturnTo[strlen(Myname)]				== '!')
				ReturnTo = index(ReturnTo, '!') + 1;
			break;
		case '?':
		default:
			break;
		}

	ap = getwd(Wrkdir);
	if (ap == 0) {
		fprintf(stderr, "can't get working directory; will try to continue\n");
		strcpy(Wrkdir, "/UNKNOWN");
	}

	DEBUG(4, "\n\n** %s **\n", "START");

	inargs[0] = '\0';
	while (optind < argc) {
		DEBUG(4, "arg - %s:", argv[optind]);
		strcat(inargs, " ");
		strcat(inargs, argv[optind++]);
	}
	DEBUG(4, "arg - %s\n", inargs);
	if (subchdir(Spool) < 0) {
		syslog(LOG_WARNING, "chdir(%s) failed: %m", Spool);
		cleanup(1);
	}
	uid = getuid();
	if (guinfo(uid, User, path) != SUCCESS) {
		syslog(LOG_WARNING, "Can't find username for uid %d", uid);
		DEBUG(1, "Using username", "uucp");
		strcpy(User, "uucp");
	}

	strncpy(local, Myname, MAXBASENAME);
	cmdp = cmd;
	*cmdp = '\0';
	gename(DATAPRE, local, 'X', rxfile);
	fprx = ufopen(rxfile, "w");
	if (fprx == NULL) {
		syslog(LOG_WARNING, "fopen(%s) failed: %m", rxfile);
		cleanup(1);
	}
	gename(DATAPRE, local, 'T', tcfile);
	fpc = ufopen(tcfile, "w");
	if (fpc == NULL) {
		syslog(LOG_WARNING, "fopen(%s) failed: %m", tcfile);
		cleanup(1);
	}
	fprintf(fprx, "%c %s %s\n", X_USER, User, local);
	if (nonoti)
		fprintf(fprx, "%c\n", X_NONOTI);
	if (nonzero)
		fprintf(fprx, "%c\n", X_NONZERO);
	if (ReturnTo == NULL || *ReturnTo == '\0')
		ReturnTo = User;
	fprintf(fprx, "%c %s\n", X_RETURNTO, ReturnTo);

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
	if (versys(&xsys) != 0) {
		/*  bad system name  */
		fprintf(stderr, "bad system name: %s\n", xsys);
		fclose(fprx);
		fclose(fpc);
		cleanup(EX_NOHOST);
	}

	strncpy(Rmtname, xsys, MAXBASENAME);
	DEBUG(4, "xsys %s\n", xsys);

	if (pipein) {
		gename(DATAPRE, local, 'B', dfile);
		fpd = ufopen(dfile, "w");
		if (fpd == NULL) {
			syslog(LOG_WARNING, "fopen(%s) failed: %m", dfile);
			cleanup(1);
		}
		while (!feof(stdin)) {
			ret = fread(buf, 1, BUFSIZ, stdin);
			fwrite(buf, 1, ret, fpd);
			if (ferror(stdin)) {
				perror("stdin");
				cleanup(EX_IOERR);
			}
			if (ferror(fpd)) {
				perror(dfile);
				cleanup(EX_IOERR);
			}
			size += ret;
		}
		fclose(fpd);
		strcpy(tfile, dfile);
		if (strcmp(local, xsys) != SAME) {
			register int Len = strlen(local);
			if (Len > SYSNSIZE)
				Len = SYSNSIZE;
			tfile[Len + 2] = 'S';
			GENSEND(fpc, dfile, tfile, User, "", dfile);
			cflag++;
		}
		fprintf(fprx, "%c %s\n", X_RQDFILE, tfile);
		fprintf(fprx, "%c %s\n", X_STDIN, tfile);
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

		if (rest[0] != '\0') {
			struct stat stbuf;
			if (stat(rest, &stbuf) < 0)
				DEBUG(4, "Can't stat %s\n", rest);
			else 
				size += stbuf.st_size;
			DEBUG(4, "size = %ld\n", size);
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
			link_failed = 0;
			if (Linkit) {
				if (link(subfile(rest), subfile(dfile)) != 0)
					link_failed++;
				else
					GENSEND(fpc, rest, dfile, User, "", dfile);
			}
			if (Copy || link_failed) {
				if (xcp(rest, dfile) != 0) {
					fprintf(stderr, "can't copy %s to %s\n", rest, dfile);
					cleanup(EX_NOINPUT);
				}
				GENSEND(fpc, rest, dfile, User, "", dfile);
			}
			if (!Copy && !Linkit) {
				GENSEND(fpc, rest, dfile, User, "c", "D.0");
			}
			cflag++;
			if (redir == '<') {
				fprintf(fprx, "%c %s\n", X_STDIN, dfile);
				fprintf(fprx, "%c %s\n", X_RQDFILE, dfile);
			} else {
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
			if (fp == NULL) {
				syslog(LOG_WARNING, "fopen(%s) failed: %m",
					tfile);
				cleanup(1);
			}
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
			} else {
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
			if (fpd == NULL) {
				syslog(LOG_WARNING, "fopen(%s) failed: %m",
					dfile);
				cleanup(1);
			}
			gename(DATAPRE, local, 'T', t2file);
			GENRCV(fpd, rest, t2file, User);
			fclose(fpd);
			GENSEND(fpc, dfile, tfile, User, "", dfile);
			cflag++;
			if (redir == '<') {
				fprintf(fprx, "%c %s\n", X_RQDFILE, t2file);
				fprintf(fprx, "%c %s\n", X_STDIN, t2file);
			} else {
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
	/*
	 * clean up trailing ' ' in command.
	 */
	if (cmdp > cmd && cmdp[0] == '\0' && cmdp[-1] == ' ')
		*--cmdp = '\0';
	/* block multi-hop uux, which doesn't work */
	for (ap = cmd; *ap && *ap != ' '; ap++)
		if (*ap == '!') {
			fprintf(stderr, "uux handles only adjacent sites.\n");
			fprintf(stderr, "Try uusend for multi-hop delivery.\n");
			cleanup(EX_USAGE);
		}

	fprintf(fprx, "%c %s\n", X_CMD, cmd);
	if (ferror(fprx)) {
		logent(cmd, "COULD NOT QUEUE XQT");
		cleanup(EX_IOERR);
	} else
		logent(cmd, "XQT QUE'D");
	fclose(fprx);

	if (size > 0 && Gradedelta > 0) {
		DEBUG (4, "Grade changed from %c ", Grade);
		Grade += size/Gradedelta;
		if (Grade > 'z')
			Grade = 'z';
		DEBUG(4, "to %c\n", Grade);
	}
	gename(XQTPRE, local, Grade, tfile);
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

	if (ferror(fpc))
		cleanup(EX_IOERR);
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
	exit(0);
}

#define FTABSIZE 30
char Fname[FTABSIZE][NAMESIZE];
int Fnamect = 0;

/*
 *	cleanup and unlink if error
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

/*
 *	open file and record name
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
	return fopen(subfile(file), mode);
}
#ifdef	VMS
/*
 * EUNICE bug:
 *	quotes are not stripped from DCL.  Do it here.
 *	Note if we are running under Unix shell we don't
 *	do the right thing.
 */
arg_fix(argc, argv)
char **argv;
{
	register char *cp, *tp;

	for (; argc > 0; --argc, argv++) {
		cp = *argv;
		if (cp == (char *)0 || *cp++ != '"')
			continue;
		tp = cp;
		while (*tp++) ;
		tp -= 2;
		if (*tp == '"') {
			*tp = '\0';
			*argv = cp;
		}
	}
}
#endif VMS

/*
 *	split into system and file part
 *
 *	return codes:
 *		NOSYSPART
 *		HASSYSPART
 */

split(name, sys, rest)
register char *name, *rest;
char *sys;
{
	register char *c;

	if (*name == LQUOTE) {
		if ((c = index(name + 1, RQUOTE)) != NULL) {
		/* strip off quotes */
			name++;
			while (c != name)
				*rest++ = *name++;
			*rest = '\0';
			*sys = '\0';
			return NOSYSPART;
		}
	}

	if ((c = index(name, '!')) == NULL) {
		strcpy(rest, name);
		*sys = '\0';
		return NOSYSPART;
	}

	*c++ = '\0';
	strncpy(sys, name, MAXBASENAME);
	sys[MAXBASENAME] = '\0';

	strcpy(rest, c);
	return HASSYSPART;
}

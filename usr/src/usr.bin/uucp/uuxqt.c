#ifndef lint
static char sccsid[] = "@(#)uuxqt.c	5.2 (Berkeley) 7/2/83";
#endif

#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	NDIR
#include "ndir.h"
#else
#include <sys/dir.h>
#endif

#define APPCMD(d) {\
char *p;\
for (p = d; *p != '\0';) *cmdp++ = *p++;\
*cmdp++ = ' ';\
*cmdp = '\0';}

/*
 *	uuxqt will execute commands set up by a uux command,
 *	usually from a remote machine - set by uucp.
 */

#define	NCMDS	50
char *Cmds[NCMDS];

int notiok = 1;
int nonzero = 0;

char PATH[MAXFULLNAME] = "PATH=/bin:/usr/bin";
/*  to remove restrictions from uuxqt
 *  define ALLOK 1
 *
 *  to add allowable commands, add to the file CMDFILE
 *  A line of form "PATH=..." changes the search path
 */


main(argc, argv)
char *argv[];
{
	char xcmd[MAXFULLNAME];
	int argnok;
	char xfile[MAXFULLNAME], user[32], buf[BUFSIZ];
	char lbuf[30];
	char cfile[NAMESIZE], dfile[MAXFULLNAME];
	char file[NAMESIZE];
	char fin[MAXFULLNAME], sysout[NAMESIZE], fout[MAXFULLNAME];
	register FILE *xfp, *fp;
	FILE *dfp;
	char path[MAXFULLNAME];
	char cmd[BUFSIZ];
	/* set size of prm to something large -- cmcl2!salkind */
	char *cmdp, prm[1000], *ptr;
	char *getprm(), *lastpart();
	int uid, ret, badfiles;
	register int i;
	int stcico = 0;
	char retstat[30];
	int orig_uid = getuid();

	strcpy(Progname, "uuxqt");
	uucpname(Myname);

	/* Try to run as uucp -- rti!trt */
	setgid(getegid());
	setuid(geteuid());

	umask(WFMASK);
	Ofn = 1;
	Ifn = 0;
	while (argc>1 && argv[1][0] == '-') {
		switch(argv[1][1]){
		case 'x':
			chkdebug(orig_uid);
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			break;
		default:
			fprintf(stderr, "unknown flag %s\n", argv[1]);
				break;
		}
		--argc;  argv++;
	}

	DEBUG(4, "\n\n** %s **\n", "START");
	subchdir(Spool);
	strcpy(Wrkdir, Spool);
	uid = getuid();
	guinfo(uid, User, path);
	DEBUG(4, "User - %s\n", User);
	if (ulockf(X_LOCK, (time_t)  X_LOCKTIME) != 0)
		exit(0);

	fp = fopen(CMDFILE, "r");
	if (fp == NULL) {
		/* Fall-back if CMDFILE missing. Sept 1982, rti!trt */
		logent("CAN'T OPEN", CMDFILE);
		Cmds[0] = "rmail";
		Cmds[1] = "rnews";
		Cmds[2] = "ruusend";
		Cmds[3] = NULL;
		goto doprocess;
	}
	DEBUG(5, "%s opened\n", CMDFILE);
	for (i=0; i<NCMDS-1 && cfgets(xcmd, sizeof(xcmd), fp) != NULL; i++) {
		xcmd[strlen(xcmd)-1] = '\0';
		if (strncmp(xcmd, "PATH=", 5) == 0) {
			strcpy(PATH, xcmd);
			i--; /* kludge */
			continue;
		}
		DEBUG(5, "xcmd = %s\n", xcmd);
		Cmds[i] = malloc((unsigned)(strlen(xcmd)+1));
		strcpy(Cmds[i], xcmd);
	}
	Cmds[i] = 0;
	fclose(fp);

doprocess:
	DEBUG(4, "process %s\n", "");
	while (gtxfile(xfile) > 0) {
		ultouch();	/* rti!trt */
		DEBUG(4, "xfile - %s\n", xfile);

		xfp = fopen(subfile(xfile), "r");
		ASSERT(xfp != NULL, "CAN'T OPEN", xfile, 0);

		/*  initialize to default  */
		strcpy(user, User);
		strcpy(fin, "/dev/null");
		strcpy(fout, "/dev/null");
		sprintf(sysout, "%.7s", Myname);
		badfiles = 0;	/* this was missing -- rti!trt */
		while (fgets(buf, BUFSIZ, xfp) != NULL) {
			switch (buf[0]) {
			case X_USER:
				sscanf(&buf[1], "%s%s", user, Rmtname);
				break;
			case X_STDIN:
				sscanf(&buf[1], "%s", fin);
				i = expfile(fin);
				/* rti!trt: do not check permissions of
				 * vanilla spool file */
				if (i != 0
				 && (chkpth("", "", fin) || anyread(fin) != 0))
					badfiles = 1;
				break;
			case X_STDOUT:
				sscanf(&buf[1], "%s%s", fout, sysout);
				sysout[7] = '\0';
				/* rti!trt: do not check permissions of
				 * vanilla spool file.  DO check permissions
				 * of writing on a non-vanilla file */
				i = 1;
				if (fout[0] != '~' || prefix(sysout, Myname))
					i = expfile(fout);
				if (i != 0
				 && (chkpth("", "", fout)
					|| chkperm(fout, (char *)1)))
					badfiles = 1;
				break;
			case X_CMD:
				strcpy(cmd, &buf[2]);
				if (*(cmd + strlen(cmd) - 1) == '\n')
					*(cmd + strlen(cmd) - 1) = '\0';
				break;
			case X_NONOTI:
				notiok = 0;
				break;
			case X_NONZERO:
				nonzero = 1;
				break;
			default:
				break;
			}
		}

		fclose(xfp);
		DEBUG(4, "fin - %s, ", fin);
		DEBUG(4, "fout - %s, ", fout);
		DEBUG(4, "sysout - %s, ", sysout);
		DEBUG(4, "user - %s\n", user);
		DEBUG(4, "cmd - %s\n", cmd);

		/*  command execution  */
		if (strcmp(fout, "/dev/null") == SAME)
			strcpy(dfile,"/dev/null");
		else
			gename(DATAPRE, sysout, 'O', dfile);

		/* expand file names where necessary */
		expfile(dfile);
		strcpy(buf, PATH);
		strcat(buf, ";export PATH;");
		cmdp = buf + strlen(buf);
		ptr = cmd;
		xcmd[0] = '\0';
		argnok = 0;
		while ((ptr = getprm(ptr, prm)) != NULL) {
			if (prm[0] == ';' || prm[0] == '^'
			  || prm[0] == '&'  || prm[0] == '|') {
				xcmd[0] = '\0';
				APPCMD(prm);
				continue;
			}

			if ((argnok = argok(xcmd, prm)) != 0) 
				/*  command not valid  */
				break;

			if (prm[0] == '~')
				expfile(prm);
			APPCMD(prm);
		}
		if (argnok || badfiles) {
			sprintf(lbuf, "%s XQT DENIED", user);
			logent(cmd, lbuf);
			DEBUG(4, "bad command %s\n", prm);
			notify(user, Rmtname, cmd, "DENIED");
			goto rmfiles;
		}
		sprintf(lbuf, "%s XQT", user);
		logent(buf, lbuf);
		DEBUG(4, "cmd %s\n", buf);

		mvxfiles(xfile);
		subchdir(XQTDIR);
		ret = shio(buf, fin, dfile, (char *)NULL);
/* watcgl.11, dmmartindale, signal and exit values were reversed */
		sprintf(retstat, "signal %d, exit %d", ret & 0377,
		  (ret>>8) & 0377);
		if (strcmp(xcmd, "rmail") == SAME)
			notiok = 0;
		if (strcmp(xcmd, "rnews") == SAME)
			nonzero = 1;
		 if (notiok && (!nonzero || (nonzero && ret != 0)))
			notify(user, Rmtname, cmd, retstat);
		else if (ret != 0 && strcmp(xcmd, "rmail") == SAME) {
			/* mail failed - return letter to sender  */
			retosndr(user, Rmtname, fin);
			sprintf(buf, "ret (%o) from %s!%s", ret, Rmtname, user);
			logent("MAIL FAIL", buf);
		}
		DEBUG(4, "exit cmd - %d\n", ret);
		subchdir(Spool);
		rmxfiles(xfile);
		if (ret != 0) {
			/*  exit status not zero */
			dfp = fopen(subfile(dfile), "a");
			ASSERT(dfp != NULL, "CAN'T OPEN", dfile, 0);
			fprintf(dfp, "exit status %d", ret);
			fclose(dfp);
		}
		if (strcmp(fout, "/dev/null") != SAME) {
			if (prefix(sysout, Myname)) {
				xmv(dfile, fout);
				chmod(fout, BASEMODE);
			}
			else {
				gename(CMDPRE, sysout, 'O', cfile);
				fp = fopen(subfile(cfile), "w");
				ASSERT(fp != NULL, "OPEN", cfile, 0);
				fprintf(fp, "S %s %s %s - %s 0666\n",
				dfile, fout, user, lastpart(dfile));
				fclose(fp);
			}
		}
	rmfiles:
		xfp = fopen(subfile(xfile), "r");
		ASSERT(xfp != NULL, "CAN'T OPEN", xfile, 0);
		while (fgets(buf, BUFSIZ, xfp) != NULL) {
			if (buf[0] != X_RQDFILE)
				continue;
			sscanf(&buf[1], "%s", file);
			unlink(subfile(file));
		}
		unlink(subfile(xfile));
		fclose(xfp);
	}

	if (stcico)
		xuucico("");
	cleanup(0);
}


cleanup(code)
int code;
{
	logcls();
	rmlock(CNULL);
	exit(code);
}


/*******
 *	gtxfile(file)	get a file to execute
 *	char *file;
 *
 *	return codes:  0 - no file  |  1 - file to execute
 * Mod to recheck for X-able files. Sept 1982, rti!trt.
 * Suggested by utzoo.2458 (utzoo!henry)
 * Uses iswrk/gtwrkf to keep files in sequence, May 1983.
 */

gtxfile(file)
register char *file;
{
	char pre[3];
	register int rechecked;

	pre[0] = XQTPRE;
	pre[1] = '.';
	pre[2] = '\0';
	rechecked = 0;
retry:
	if (!gtwrkf(Spool, file)) {
		if (rechecked)
			return(0);
		rechecked = 1;
		DEBUG(4, "iswrk\n", "");
		if (!iswrk(file, "get", Spool, pre))
			return(0);
	}
	DEBUG(4, "file - %s\n", file);
#ifndef UUDIR
	/* skip spurious subdirectories */
	if (strcmp(pre, file) == SAME)
		goto retry;
#endif
	if (gotfiles(file))
		return(1);
	goto retry;
}


/***
 *	gotfiles(file)		check for needed files
 *	char *file;
 *
 *	return codes:  0 - not ready  |  1 - all files ready
 */

gotfiles(file)
register char *file;
{
	struct stat stbuf;
	register FILE *fp;
	char buf[BUFSIZ], rqfile[MAXFULLNAME];

	fp = fopen(subfile(file), "r");
	if (fp == NULL)
		return(0);

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		DEBUG(4, "%s\n", buf);
		if (buf[0] != X_RQDFILE)
			continue;
		sscanf(&buf[1], "%s", rqfile);
		expfile(rqfile);
		if (stat(subfile(rqfile), &stbuf) == -1) {
			fclose(fp);
			return(0);
		}
	}

	fclose(fp);
	return(1);
}


/***
 *	rmxfiles(xfile)		remove execute files to x-directory
 *	char *xfile;
 *
 *	return codes - none
 */

rmxfiles(xfile)
register char *xfile;
{
	register FILE *fp;
	char buf[BUFSIZ], file[NAMESIZE], tfile[NAMESIZE];
	char tfull[MAXFULLNAME];

	if((fp = fopen(subfile(xfile), "r")) == NULL)
		return;

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		if (buf[0] != X_RQDFILE)
			continue;
		if (sscanf(&buf[1], "%s%s", file, tfile) < 2)
			continue;
		sprintf(tfull, "%s/%s", XQTDIR, tfile);
		unlink(subfile(tfull));
	}
	fclose(fp);
	return;
}


/***
 *	mvxfiles(xfile)		move execute files to x-directory
 *	char *xfile;
 *
 *	return codes - none
 */

mvxfiles(xfile)
char *xfile;
{
	register FILE *fp;
	char buf[BUFSIZ], ffile[MAXFULLNAME], tfile[NAMESIZE];
	char tfull[MAXFULLNAME];
	int ret;

	if((fp = fopen(subfile(xfile), "r")) == NULL)
		return;

	while (fgets(buf, BUFSIZ, fp) != NULL) {
		if (buf[0] != X_RQDFILE)
			continue;
		if (sscanf(&buf[1], "%s%s", ffile, tfile) < 2)
			continue;
		expfile(ffile);
		sprintf(tfull, "%s/%s", XQTDIR, tfile);
		/* duke!rti, ncsu!mcm: use xmv, not link(II) */
		unlink(subfile(tfull));
		ret = xmv(ffile, tfull);
		ASSERT(ret == 0, "XQTDIR ERROR", "", ret);
	}
	fclose(fp);
	return;
}


/***
 *	argok(xc, cmd)		check for valid command/argumanet
 *			*NOTE - side effect is to set xc to the
 *				command to be executed.
 *	char *xc, *cmd;
 *
 *	return 0 - ok | 1 nok
 */

argok(xc, cmd)
register char *xc, *cmd;
{
	register char **ptr;

#ifndef ALLOK
	/* don't allow sh command strings `....` */
	/* don't allow redirection of standard in or out  */
	/* don't allow other funny stuff */
	/* but there are probably total holes here */
	/* post-script.  ittvax!swatt has a uuxqt that solves this. */
	/* This version of uuxqt will shortly disappear */
	if (index(cmd, '`') != NULL
	  || index(cmd, '>') != NULL
	  || index(cmd, ';') != NULL
	  || index(cmd, '^') != NULL
	  || index(cmd, '&') != NULL
	  || index(cmd, '|') != NULL
	  || index(cmd, '<') != NULL)
		return(1);
#endif

	if (xc[0] != '\0')
		return(0);

#ifndef ALLOK
	ptr = Cmds;
	while(*ptr != NULL) {
		if (strcmp(cmd, *ptr) == SAME)
			break;
	ptr++;
	}
	if (*ptr == NULL)
		return(1);
#endif
	strcpy(xc, cmd);
	return(0);
}


/***
 *	notify	send mail to user giving execution results
 *	return code - none
 *	This program assumes new mail command - send remote mail
 */

notify(user, rmt, cmd, str)
char *user, *rmt, *cmd, *str;
{
	char text[MAXFULLNAME];
	char ruser[MAXFULLNAME];

	sprintf(text, "uuxqt cmd (%.50s) status (%s)", cmd, str);
	if (prefix(rmt, Myname))
		strcpy(ruser, user);
	else
		sprintf(ruser, "%s!%s", rmt, user);
	mailst(ruser, text, "");
	return;
}

/***
 *	retosndr - return mail to sender
 *
 *	return code - none
 */

retosndr(user, rmt, file)
char *user, *rmt, *file;
{
	char ruser[100];

	if (strcmp(rmt, Myname) == SAME)
		strcpy(ruser, user);
	else
		sprintf(ruser, "%s!%s", rmt, user);

	if (anyread(file) == 0)
		mailst(ruser, "Mail failed.  Letter returned to sender.\n", file);
	else
		mailst(ruser, "Mail failed.  Letter returned to sender.\n", "");
	return;
}

#ifndef lint
static char sccsid[] = "@(#)cntrl.c	5.1 (Berkeley) 7/2/83";
#endif

#include "uucp.h"
#include <sys/types.h>
#include <sys/stat.h>



struct Proto {
	char P_id;
	int (*P_turnon)();
	int (*P_rdmsg)();
	int (*P_wrmsg)();
	int (*P_rddata)();
	int (*P_wrdata)();
	int (*P_turnoff)();
};


extern int gturnon(), gturnoff();
extern int grdmsg(), grddata();
extern int gwrmsg(), gwrdata();
extern int imsg(), omsg();

struct Proto Ptbl[]={
	'g', gturnon, grdmsg, gwrmsg, grddata, gwrdata, gturnoff,
	'\0'
};

int (*Imsg)() = imsg, (*Omsg)() = omsg;	/* avoid SEL compiler limitation */

int (*Rdmsg)()=imsg, (*Rddata)();
int (*Wrmsg)()=omsg, (*Wrdata)();
int (*Turnon)(), (*Turnoff)();


#define YES "Y"
#define NO "N"

/*  failure messages  */
#define EM_MAX		6
#define EM_LOCACC	"N1"	/* local access to file denied */
#define EM_RMTACC	"N2"	/* remote access to file/path denied */
#define EM_BADUUCP	"N3"	/* a bad uucp command was generated */
#define EM_NOTMP	"N4"	/* remote error - can't create temp */
#define EM_RMTCP	"N5"	/* can't copy to remote directory - file in public */
#define EM_LOCCP	"N6"	/* can't copy on local system */

char *Em_msg[] = {
	"COPY FAILED (reason not given by remote)",
	"local access to file denied",
	"remote access to path/file denied",
	"system error - bad uucp command generated",
	"remote system can't create temp file",
	"can't copy to file/directory - file left in PUBDIR/user/file",
	"can't copy to file/directory - file left in PUBDIR/user/file"
};

/*       */


#define XUUCP 'X'	/* execute uucp (string) */
#define SLTPTCL 'P'	/* select protocol  (string)  */
#define USEPTCL 'U'	/* use protocol (character) */
#define RCVFILE 'R'	/* receive file (string) */
#define SNDFILE 'S'	/* send file (string) */
#define RQSTCMPT 'C'	/* request complete (string - yes | no) */
#define HUP     'H'	/* ready to hangup (string - yes | no) */
#define RESET	'X'	/* reset line modes */


#define W_TYPE		wrkvec[0]
#define W_FILE1		wrkvec[1]
#define W_FILE2		wrkvec[2]
#define W_USER		wrkvec[3]
#define W_OPTNS		wrkvec[4]
#define W_DFILE		wrkvec[5]
#define W_MODE		wrkvec[6]
#define W_NUSER		wrkvec[7]

#define	XFRRATE	350000L
#define RMESG(m, s, n) if (rmesg(m, s, n) != 0) {(*Turnoff)(); return(FAIL);} else
#define RAMESG(s, n) if (rmesg('\0', s, n) != 0) {(*Turnoff)(); return(FAIL);} else
#define WMESG(m, s) if(wmesg(m, s) != 0) {(*Turnoff)(); return(FAIL);} else

char Wfile[MAXFULLNAME] = {'\0'};
char Dfile[MAXFULLNAME];

/*
 * To avoid a huge backlog of X. files, start uuxqt every so often.
 * To avoid a huge number of uuxqt zombies, 
 * wait for one occasionally!
 */
static int nXfiles = 0;	/* number of X files since last uuxqt start */
static int nXQTs = 0;	/* number of uuxqts started */

/*******
 *	cntrl(role, wkpre)
 *	int role;
 *	char *wkpre;
 *
 *	cntrl  -  this routine will execute the conversation
 *	between the two machines after both programs are
 *	running.
 *
 *	return codes
 *		SUCCESS - ok
 *		FAIL - failed
 */

cntrl(role, wkpre)
int role;
char *wkpre;
{
	char msg[BUFSIZ], rqstr[BUFSIZ];
	register FILE *fp;
	int filemode;
	struct stat stbuf;
	char filename[MAXFULLNAME], wrktype, *wrkvec[20];
	extern (*Rdmsg)(), (*Wrmsg)();
	extern char *index(), *lastpart();
	int status = 1;
	register int i, narg;
	int mailopt, ntfyopt;
	int ret;
	static int pnum, tmpnum = 0;

	pnum = getpid();
/*
 * ima.247, John Levine, IECC, PO Box 349, Cambridge MA 02238; (617) 491-5450
 * zap Wfile to prevent reuse of wrong C. file
 */
	Wfile[0] = '\0';
top:
	for (i = 0; i < sizeof wrkvec / sizeof wrkvec[0]; i++)
		wrkvec[i] = 0;
	DEBUG(4, "*** TOP ***  -  role=%d, ", role);
	setline(RESET);
	if (role == MASTER) {
		/* get work */
		if ((narg = gtwvec(Wfile, Spool, wkpre, wrkvec)) == 0) {
			WMESG(HUP, "");
			RMESG(HUP, msg, 1);
			goto process;
		}
		wrktype = W_TYPE[0];
		mailopt = index(W_OPTNS, 'm') != NULL;
		ntfyopt = index(W_OPTNS, 'n') != NULL;

		msg[0] = '\0';
		for (i = 1; i < narg; i++) {
			strcat(msg, " ");
			strcat(msg, wrkvec[i]);
		}

		if (wrktype == XUUCP) {
			sprintf(rqstr, "X %s", msg);
			logent(rqstr, "REQUEST");
			goto sendmsg;
		}

		ASSERT(narg > 4, "ARG COUNT<5", "", i);
		sprintf(User, "%.9s", W_USER);
		sprintf(rqstr, "%s %s %s %s", W_TYPE, W_FILE1,
		  W_FILE2, W_USER);
		logent(rqstr, "REQUEST");
		if (wrktype == SNDFILE ) {
			strcpy(filename, W_FILE1);
			i = expfile(filename);
			DEBUG(4, "expfile type - %d", i);
			if (i != 0 && chkpth(User, "", filename))
				goto e_access;
			strcpy(Dfile, W_DFILE);
			fp = NULL;
			if (index(W_OPTNS, 'c') == NULL) {
				fp = fopen(subfile(Dfile), "r");
				if (fp != NULL)
					i = 0;
			}
			if (fp == NULL &&
			   (fp = fopen(subfile(filename), "r")) == NULL) {
				/*  can not read data file  */
				logent("CAN'T READ DATA", "FAILED");
				unlinkdf(Dfile);
				lnotify(User, filename, "can't access");
				goto top;
			}
			/* if file exists but is not generally readable... */
			if (i != 0 && fstat(fileno(fp), &stbuf) == 0
			&&  (stbuf.st_mode & ANYREAD) == 0) {
		e_access:;
				/*  access denied  */
				fclose(fp);
				fp = NULL;
				logent("DENIED", "ACCESS");
				unlinkdf(W_DFILE);
				lnotify(User, filename, "access denied");
				goto top;
			}

			setline(SNDFILE);
		}

		if (wrktype == RCVFILE) {
			strcpy(filename, W_FILE2);
			expfile(filename);
			if (chkpth(User, "", filename)
			 || chkperm(filename, index(W_OPTNS, 'd'))) {
				/*  access denied  */
				logent("DENIED", "ACCESS");
				lnotify(User, filename, "access denied");
				goto top;
			}
			sprintf(Dfile, "%s/TM.%05d.%03d", Spool, pnum, tmpnum++);
			if ((fp = fopen(subfile(Dfile), "w")) == NULL) {
				/*  can not create temp  */
				logent("CAN'T CREATE TM", "FAILED");
				unlinkdf(Dfile);
				goto top;
			}
			setline(RCVFILE);
		}
sendmsg:
		DEBUG(4, "wrktype - %c\n ", wrktype);
		WMESG(wrktype, msg);
		RMESG(wrktype, msg, 1);
		goto process;
	}

	/* role is slave */
	RAMESG(msg, 1);
	goto process;

process:
/*	rti!trt: ultouch is now done in gio.c (yes, kludge)
 *	ultouch();
 */
	DEBUG(4, " PROCESS: msg - %s\n", msg);
	switch (msg[0]) {

	case RQSTCMPT:
		DEBUG(4, "%s\n", "RQSTCMPT:");
		if (msg[1] == 'N') {
			i = atoi(&msg[2]);
			if (i<0 || i>EM_MAX) i=0;
			/* duke!rti: only note failed requests */
			logent(msg, "REQUESTED");
		}
		if (role == MASTER) {
			notify(mailopt, W_USER, W_FILE1, Rmtname, &msg[1]);
		}
		goto top;

	case HUP:
		DEBUG(4, "%s\n", "HUP:");
		if (msg[1] == 'Y') {
			WMESG(HUP, YES);
			(*Turnoff)();
			Rdmsg = Imsg;
			Wrmsg = Omsg;
			return(0);
		}

		if (msg[1] == 'N') {
			ASSERT(role == MASTER, "WRONG ROLE", "", role);
			role = SLAVE;
			goto top;
		}

		/* get work */
		if (!iswrk(Wfile, "chk", Spool, wkpre)) {
			WMESG(HUP, YES);
			RMESG(HUP, msg, 1);
			goto process;
		}

		WMESG(HUP, NO);
		role = MASTER;
		goto top;

	case XUUCP:
		if (role == MASTER) {
			goto top;
		}

		/*  slave part  */
		i = getargs(msg, wrkvec);
		strcpy(filename, W_FILE1);
		if (index(filename, ';') != NULL
		  || index(W_FILE2, ';') != NULL
		  || i < 3) {
			WMESG(XUUCP, NO);
			goto top;
		}
		expfile(filename);
		if (chkpth("", Rmtname, filename)) {
			WMESG(XUUCP, NO);
			logent("XUUCP DENIED", filename);
			goto top;
		}
		sprintf(rqstr, "%s %s", filename, W_FILE2);
		xuucp(rqstr);
		WMESG(XUUCP, YES);
		goto top;

	case SNDFILE:
		/*  MASTER section of SNDFILE  */

		DEBUG(4, "%s\n", "SNDFILE:");
		if (msg[1] == 'N') {
			i = atoi(&msg[2]);
			if (i < 0 || i > EM_MAX)
				i = 0;
			logent(Em_msg[i], "REQUEST");
			notify(mailopt, W_USER, W_FILE1, Rmtname, &msg[1]);
			ASSERT(role == MASTER, "WRONG ROLE", "", role);
			fclose(fp);
			fp = NULL;
			if (msg[1] != '4')
				unlinkdf(W_DFILE);
			goto top;
		}

		if (msg[1] == 'Y') {
			/* send file */
			ASSERT(role == MASTER, "WRONG ROLE", "", role);
			ret = fstat(fileno(fp), &stbuf);
			ASSERT(ret != -1, "STAT FAILED", filename, 0);
			i = 1 + (int)(stbuf.st_size / XFRRATE);
			ret = (*Wrdata)(fp, Ofn);
			fclose(fp);
			fp = NULL;
			if (ret != 0) {
				(*Turnoff)();
				return(FAIL);
			}
			RMESG(RQSTCMPT, msg, i);
/* put the unlink *after* the RMESG -- fortune!Dave-Yost */
			unlinkdf(W_DFILE);
			goto process;
		}

		/*  SLAVE section of SNDFILE  */
		ASSERT(role == SLAVE, "WRONG ROLE", "", role);

		/* request to receive file */
		/* check permissions */
		i = getargs(msg, wrkvec);
		ASSERT(i > 4, "ARG COUNT<5", "", i);
		sprintf(rqstr, "%s %s %s %s", W_TYPE, W_FILE1,
		  W_FILE2, W_USER);
		logent(rqstr, "REQUESTED");
		DEBUG(4, "msg - %s\n", msg);
		strcpy(filename, W_FILE2);
		/* Run uuxqt occasionally */
		if (filename[0] == XQTPRE) {
			if (++nXfiles > 10) {
				nXfiles = 0;
				/* I sure hope the wait(II) does not hang.
				 * One can never tell about UNIX variants.
				 */
				if (++nXQTs > 2)
					wait((int *)0);
				xuuxqt();
			}
		}
		/* rti!trt: expand filename, i is set to 0 if this is
		 * is a vanilla spool file, so no stat(II)s are needed */
		i = expfile(filename);
		DEBUG(4, "expfile type - %d\n", i);
		if (i != 0) {
			if (chkpth("", Rmtname, filename)
			 || chkperm(filename, index(W_OPTNS, 'd'))) {
				WMESG(SNDFILE, EM_RMTACC);
				logent("DENIED", "PERMISSION");
				goto top;
			}
			if (isdir(filename)) {
				strcat(filename, "/");
				strcat(filename, lastpart(W_FILE1));
			}
		}
		sprintf(User, "%.9s", W_USER);

		DEBUG(4, "chkpth ok Rmtname - %s\n", Rmtname);
		sprintf(Dfile, "%s/TM.%05d.%03d", Spool, pnum, tmpnum++);
		if((fp = fopen(subfile(Dfile), "w")) == NULL) {
			WMESG(SNDFILE, EM_NOTMP);
			logent("CAN'T OPEN", "DENIED");
			unlinkdf(Dfile);
			goto top;
		}

		WMESG(SNDFILE, YES);
		ret = (*Rddata)(Ifn, fp);
		/* ittvax!swatt: (try to) make sure IO successful */
		fflush(fp);
		if (ferror(fp) || fclose(fp))
			ret = FAIL;
		if (ret != 0) {
			(*Turnoff)();
			return(FAIL);
		}
		/* copy to user directory */
		ntfyopt = index(W_OPTNS, 'n') != NULL;
		status = xmv(Dfile, filename);
		WMESG(RQSTCMPT, status ? EM_RMTCP : YES);
		if (status == 0) {
			sscanf(W_MODE, "%o", &filemode);
			if (filemode <= 0)
				filemode = BASEMODE;
			chmod(subfile(filename), filemode | BASEMODE);
			arrived(ntfyopt, filename, W_NUSER, Rmtname, User);
		}
		else {
			logent("FAILED", "COPY");
			status = putinpub(filename, Dfile, W_USER);
			DEBUG(4, "->PUBDIR %d\n", status);
			if (status == 0)
				arrived(ntfyopt, filename, W_NUSER,
				  Rmtname, User);
		}

		goto top;

	case RCVFILE:
		/*  MASTER section of RCVFILE  */

		DEBUG(4, "%s\n", "RCVFILE:");
		if (msg[1] == 'N') {
			i = atoi(&msg[2]);
			if (i < 0 || i > EM_MAX)
				i = 0;
			logent(Em_msg[i], "REQUEST");
			notify(mailopt, W_USER, W_FILE1, Rmtname, &msg[1]);
			ASSERT(role == MASTER, "WRONG ROLE", "", role);
			fclose(fp);
			unlinkdf(Dfile);
			goto top;
		}

		if (msg[1] == 'Y') {
			/* receive file */
			ASSERT(role == MASTER, "WRONG ROLE", "", role);
			ret = (*Rddata)(Ifn, fp);
			/* ittvax!swatt: (try to) make sure IO successful */
			fflush(fp);
			if (ferror(fp) || fclose(fp))
				ret = FAIL;
			if (ret != 0) {
				(*Turnoff)();
				return(FAIL);
			}
			/* copy to user directory */
			if (isdir(filename)) {
				strcat(filename, "/");
				strcat(filename, lastpart(W_FILE1));
			}
			status = xmv(Dfile, filename);
			WMESG(RQSTCMPT, status ? EM_RMTCP : YES);
			notify(mailopt, W_USER, filename, Rmtname,
			  status ? EM_LOCCP : YES);
			if (status == 0) {
				sscanf(&msg[2], "%o", &filemode);
				if (filemode <= 0)
					filemode = BASEMODE;
				chmod(subfile(filename), filemode | BASEMODE);
			}
			else {
				logent("FAILED", "COPY");
				putinpub(filename, Dfile, W_USER);
			}
			goto top;
		}

		/*  SLAVE section of RCVFILE  */
		ASSERT(role == SLAVE, "WRONG ROLE", "", role);

		/* request to send file */
		strcpy(rqstr, msg);
		logent(rqstr, "REQUESTED");

		/* check permissions */
		i = getargs(msg, wrkvec);
		ASSERT(i > 3, "ARG COUNT<4", "", i);
		DEBUG(4, "msg - %s\n", msg);
		DEBUG(4, "W_FILE1 - %s\n", W_FILE1);
		strcpy(filename, W_FILE1);
		expfile(filename);
		if (isdir(filename)) {
			strcat(filename, "/");
			strcat(filename, lastpart(W_FILE2));
		}
		sprintf(User, "%.9s", W_USER);
		if (chkpth("", Rmtname, filename) || anyread(filename)) {
			WMESG(RCVFILE, EM_RMTACC);
			logent("DENIED", "PERMISSION");
			goto top;
		}
		DEBUG(4, "chkpth ok Rmtname - %s\n", Rmtname);

		if ((fp = fopen(subfile(filename), "r")) == NULL) {
			WMESG(RCVFILE, EM_RMTACC);
			logent("CAN'T OPEN", "DENIED");
			goto top;
		}

		/*  ok to send file */
		ret = fstat(fileno(fp), &stbuf);
		ASSERT(ret != -1, "STAT FAILED", filename, 0);
		i = 1 + (int)(stbuf.st_size / XFRRATE);
		sprintf(msg, "%s %o", YES, stbuf.st_mode & 0777);
		WMESG(RCVFILE, msg);
		ret = (*Wrdata)(fp, Ofn);
		fclose(fp);
		if (ret != 0) {
			(*Turnoff)();
			return(FAIL);
		}
		RMESG(RQSTCMPT, msg, i);
		goto process;
	}
	(*Turnoff)();
	return(FAIL);
}


/***
 *	rmesg(c, msg, n)	read message 'c'
 *				try 'n' times
 *	char *msg, c;
 *
 *	return code:  0  |  FAIL
 */

rmesg(c, msg, n)
register char *msg, c;
register int n;
{
	char str[50];

	DEBUG(4, "rmesg - '%c' ", c);
	if (n != 1) {
		sprintf(str, "%d", n);
		logent(str, "PATIENCE");
	}
	while ((*Rdmsg)(msg, Ifn) != 0) {
		if (--n > 0)
			continue;
		DEBUG(4, "got %s\n", "FAIL");
		sprintf(str, "expected '%c' got FAIL", c);
		logent(str, "BAD READ");
		return(FAIL);
	}
	if (c != '\0' && msg[0] != c) {
		DEBUG(4, "got %s\n", msg);
		sprintf(str, "expected '%c' got %.25s", c, msg);
		logent(str, "BAD READ");
		return(FAIL);
	}
	DEBUG(4, "got %.25s\n", msg);
	return(0);
}


/***
 *	wmesg(m, s)	write a message (type m)
 *	char *s, m;
 *
 *	return codes: 0 - ok | FAIL - ng
 */

wmesg(m, s)
register char *s, m;
{
	DEBUG(4, "wmesg '%c'", m);
	DEBUG(4, "%.25s\n", s);
	return((*Wrmsg)(m, s, Ofn));
}


/***
 *	notify		mail results of command
 *
 *	return codes:  none
 */

notify(mailopt, user, file, sys, msgcode)
char *user, *file, *sys, *msgcode;
{
	char str[200];
	int i;
	char *msg;

	if (!mailopt && *msgcode == 'Y')
		return;
	if (*msgcode == 'Y')
		msg = "copy succeeded";
	else {
		i = atoi(msgcode + 1);
		if (i < 1 || i > EM_MAX)
			i = 0;
		msg = Em_msg[i];
	}
	sprintf(str, "file %s, system %s\n%s\n",
		file, sys, msg);
	mailst(user, str, "");
	return;
}

/***
 *	lnotify(user, file, mesg)	- local notify
 *
 *	return code - none
 */

lnotify(user, file, mesg)
char *user, *file, *mesg;
{
	char mbuf[200];
	sprintf(mbuf, "file %s on %s\n%s\n", file, Myname, mesg);
	mailst(user, mbuf, "");
	return;
}


/***
 *	startup(role)
 *	int role;
 *
 *	startup  -  this routine will converse with the remote
 *	machine, agree upon a protocol (if possible) and start the
 *	protocol.
 *
 *	return codes:
 *		SUCCESS - successful protocol selection
 *		FAIL - can't find common or open failed
 */

startup(role)
int role;
{
	extern (*Rdmsg)(), (*Wrmsg)();
	extern char *blptcl(), fptcl();
	char msg[BUFSIZ], str[BUFSIZ];

	Rdmsg = Imsg;
	Wrmsg = Omsg;
	if (role == MASTER) {
		RMESG(SLTPTCL, msg, 1);
		if ((str[0] = fptcl(&msg[1])) == NULL) {
			/* no protocol match */
			WMESG(USEPTCL, NO);
			return(FAIL);
		}
		str[1] = '\0';
		WMESG(USEPTCL, str);
		if (stptcl(str) != 0)
			return(FAIL);
		DEBUG(4, "protocol %s\n", str);
		return(SUCCESS);
	}
	else {
		WMESG(SLTPTCL, blptcl(str));
		RMESG(USEPTCL, msg, 1);
		if (msg[1] == 'N') {
			return(FAIL);
		}

		if (stptcl(&msg[1]) != 0)
			return(FAIL);
		DEBUG(4, "Protocol %s\n", msg);
		return(SUCCESS);
	}
}


/*******
 *	char
 *	fptcl(str)
 *	char *str;
 *
 *	fptcl  -  this routine will choose a protocol from
 *	the input string (str) and return the found letter.
 *
 *	return codes:
 *		'\0'  -  no acceptable protocol
 *		any character  -  the chosen protocol
 */

char
fptcl(str)
register char *str;
{
	register struct Proto *p;

	for (p = Ptbl; p->P_id != '\0'; p++) {
		if (index(str, p->P_id) != NULL) {
			return(p->P_id);
		}
	}

	return('\0');
}


/***
 *	char *
 *	blptcl(str)
 *	char *str;
 *
 *	blptcl  -  this will build a string of the
 *	letters of the available protocols and return
 *	the string (str).
 *
 *	return:
 *		a pointer to string (str)
 */

char *
blptcl(str)
register char *str;
{
	register struct Proto *p;
	register char *s;

	for (p = Ptbl, s = str; (*s++ = p->P_id) != '\0'; p++);
	return(str);
}

/***
 *	stptcl(c)
 *	char *c;
 *
 *	stptcl  -  this routine will set up the six routines
 *	(Rdmsg, Wrmsg, Rddata, Wrdata, Turnon, Turnoff) for the
 *	desired protocol.
 *
 *	return codes:
 *		SUCCESS - ok
 *		FAIL - no find or failed to open
 *
 */

stptcl(c)
register char *c;
{
	register struct Proto *p;

	for (p = Ptbl; p->P_id != '\0'; p++) {
		if (*c == p->P_id) {
			/* found protocol - set routines */
			Rdmsg = p->P_rdmsg;
			Wrmsg = p->P_wrmsg;
			Rddata = p->P_rddata;
			Wrdata = p->P_wrdata;
			Turnon = p->P_turnon;
			Turnoff = p->P_turnoff;
			if ((*Turnon)() != 0)
				return(FAIL);
			DEBUG(4, "Proto started %c\n", *c);
			return(SUCCESS);
		}
	}
	DEBUG(4, "Proto start-fail %c\n", *c);
	return(FAIL);
}

/***
 *	putinpub	put file in public place
 *			if successful, filename is modified
 *
 *	return code  0 | FAIL
 */

putinpub(file, tmp, user)
register char *file, *user, *tmp;
{
	char fullname[MAXFULLNAME];
	char *lastpart();
	int status;

	sprintf(fullname, "%s/%s/", PUBDIR, user);
	if (mkdirs(fullname) != 0) {
		/* can not make directories */
		return(FAIL);
	}
	strcat(fullname, lastpart(file));
	status = xmv(tmp, fullname);
	if (status == 0) {
		strcpy(file, fullname);
		chmod(subfile(fullname), BASEMODE);
	}
	return(status);
}

/***
 *	unlinkdf(file)	- unlink D. file
 *
 *	return code - none
 */

unlinkdf(file)
register char *file;
{
	if (strlen(file) > 6)
		unlink(subfile(file));
	return;
}

/***
 *	arrived - notify receiver of arrived file
 *
 *	return code - none
 */

arrived(opt, file, nuser, rmtsys, rmtuser)
char *file, *nuser, *rmtsys, *rmtuser;
{
	char mbuf[200];

	if (!opt)
		return;
	sprintf(mbuf, "%s from %s!%s arrived\n", file, rmtsys, rmtuser);
	mailst(nuser, mbuf, "");
	return;
}

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
extern int imsg();
extern int omsg();

struct Proto Ptbl[]={
	'g', gturnon, grdmsg, gwrmsg, grddata, gwrdata, gturnoff,
	'\0'
};

int (*Rdmsg)()=imsg, (*Rddata)();
int (*Wrmsg)()=omsg, (*Wrdata)();
int (*Turnon)(), (*Turnoff)();


#define YES "Y"
#define NO "N"
#define Y 'Y'
#define N 'N'


#define XUUCP 'X'	/* execute uucp (string) */
#define SLTPTCL 'P'	/* select protocol  (string)  */
#define USEPTCL 'U'	/* use protocol (character) */
#define RCVFILE 'R'	/* receive file (string) */
#define SNDFILE 'S'	/* send file (string) */
#define RQSTCMPT 'C'	/* request complete (string - yes | no) */
#define HUP     'H'	/* ready to hangup (string - yes | no) */


#define W_TYPE		wrkvec[0]
#define W_FILE1		wrkvec[1]
#define W_FILE2		wrkvec[2]
#define W_USER		wrkvec[3]
#define W_OPTNS		wrkvec[4]
#define W_DFILE		wrkvec[5]
#define W_MODE		wrkvec[6]

#define RMESG(m, s) if (rmesg(m, s) != 0) return(FAIL);
#define RAMESG(s) if (rmesg('\0', s) != 0) return(FAIL)
#define WMESG(m, s) if(wmesg(m, s) != 0) return(FAIL)

char Wfile[MAXFULLNAME] = {'\0'};
char Dfile[MAXFULLNAME];

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
	FILE *fp;
	int filemode;
	struct stat stbuf;
	char filename[MAXFULLNAME], wrktype, *wrkvec[20];
	extern (*Rdmsg)(), (*Wrmsg)();
	extern char *index(), *lastpart();
	int status = 1, i;
	int ret;
	static int pnum, tmpnum = 0;

	pnum = getpid();
top:
	DEBUG(4, "*** TOP ***  -  role=%d, ", role);
	if (role == MASTER) {
		/* get work */
		if ((i = gtwvec(Wfile, Spool, wkpre, wrkvec)) == 0) {
			WMESG(HUP, "");
			RMESG(HUP, msg);
			goto process;
		}
		wrktype = W_TYPE[0];

		DEBUG(4, "wrktype %c, ", wrktype);
		if (wrktype == XUUCP) {
			int n;
			msg[0] = '\0';
			for (n = 1; n < i; n++) {
				strcat(msg, " ");
				strcat(msg, wrkvec[n]);
			}
			sprintf(rqstr, "X %s", msg);
			logent(rqstr, "REQUEST");
			goto sendmsg;
		}

		ASSERT(i > 4, "ARG COUNT - %d\n", i);
		sprintf(msg, " %s %s %s %s %s %s",
			W_FILE1, W_FILE2, W_USER,
			W_OPTNS, W_DFILE, W_MODE);
		strcpy(User, W_USER);
		ASSERT(strlen(User) <= 10, "User - %s\n", User);
		sprintf(rqstr, "%s %s %s %s", W_TYPE, W_FILE1,
		  W_FILE2, W_USER);
		logent(rqstr, "REQUEST");
		DEBUG(4, "User - %s\n", User);
		if (wrktype == SNDFILE ) {
			strcpy(filename, W_FILE1);
			expfile(filename);
			if (chkpth(User, "", filename)) {
				/*  access denied  */
				logent("DENIED", "ACCESS");
				unlink(W_DFILE);
				unlink(Wfile);
				goto top;
			}

			strcpy(Dfile, W_DFILE);
			if ((fp = fopen(Dfile, "r")) == NULL
			  && (fp = fopen(filename, "r")) == NULL) {
				/*  can not read data file  */
				logent("CAN'T READ DATA", "FAILED");
				unlink(Wfile);
				unlink(Dfile);
				goto top;
			}
		}

		if (wrktype == RCVFILE) {
			strcpy(filename, W_FILE2);
			expfile(filename);
			if (chkpth(User, "", filename)
			 || chkperm(filename, User, index(W_OPTNS, 'd'))) {
				/*  access denied  */
				logent("DENIED", "ACCESS");
				unlink(Wfile);
				goto top;
			}
			sprintf(Dfile, "%s/TM.%05d.%03d", Spool, pnum, tmpnum++);
			if ((fp = fopen(Dfile, "w")) == NULL) {
				/*  can not create temp  */
				logent("CAN'T CREATE TM", "FAILED");
				unlink(Wfile);
				unlink(Dfile);
				goto top;
			}
			chmod(Dfile, 0666);
		}
sendmsg:
		DEBUG(4, "wrktype - %c, ", wrktype);
		DEBUG(4, " fileno - %d\n", fileno(fp));
		WMESG(wrktype, msg);
		RMESG(wrktype, msg);
		goto process;
	}

	/* role is slave */
	RAMESG(msg);
	goto process;

process:
	DEBUG(4, " PROCESS: msg - %s\n", msg);
	switch (msg[0]) {

	case RQSTCMPT:
		DEBUG(4, "%s\n", "RQSTCMPT:");
		logent((msg[1] == 'N') ? "FAILED" : "SUCCEEDED", "REQUEST");
		if (role == MASTER) {
			notify(W_OPTNS, W_USER, W_FILE1, Rmtname,
			  (msg[1] == N) ? "failed" : "succeeded");
		}
		goto top;

	case HUP:
		DEBUG(4, "%s\n", "HUP:");
		if (msg[1] == Y) {
			WMESG(HUP, YES);
			(*Turnoff)();
			Rdmsg = imsg;
			Wrmsg = omsg;
			return(0);
		}

		if (msg[1] == N) {
			ASSERT(role == MASTER,
				"role - %d", role);
			role = SLAVE;
			goto top;
		}

		/* get work */
		if (!iswrk(Wfile, "chk", Spool, wkpre)) {
			WMESG(HUP, YES);
			RMESG(HUP, msg);
			goto process;
		}

		WMESG(HUP, NO);
		role = MASTER;
		goto top;

	case XUUCP:
		if (role == MASTER) {
			unlink(Wfile);
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
		if (msg[1] == N) {
			logent("DENIED", "REQUEST");
			ASSERT(role == MASTER,
				"role - %d", role);
			fclose(fp);
			unlink(W_DFILE);
			unlink(Wfile);
			goto top;
		}

		if (msg[1] == Y) {
			/* send file */
			ASSERT(role == MASTER,
				"role - %d", role);
			ret = (*Wrdata)(fp, Ofn);
			fclose(fp);
			if (ret != 0)
				return(FAIL);
			unlink(W_DFILE);
			RMESG(RQSTCMPT, msg);
			goto process;
		}

		/*  SLAVE section of SNDFILE  */
		ASSERT(role == SLAVE,
			"role - %d", role);

		/* request to receive file */
		/* check permissions */
		i = getargs(msg, wrkvec);
		ASSERT(i > 4, "ARG COUNT - %d\n", i);
		sprintf(rqstr, "%s %s %s %s", W_TYPE, W_FILE1,
		  W_FILE2, W_USER);
		logent(rqstr, "REQUESTED");
		DEBUG(4, "msg - %s\n", msg);
		DEBUG(4, "W_FILE2 - %s\n", W_FILE2);
		strcpy(filename, W_FILE2);
		expfile(filename);
		if (chkpth("", Rmtname, filename)
		 || chkperm(filename, Loginuser, index(W_OPTNS, 'd'))) {
			WMESG(SNDFILE, NO);
			logent("DENIED", "PERMISSION");
			goto top;
		}
		if (isdir(filename)) {
			strcat(filename, "/");
			strcat(filename, lastpart(W_FILE1));
		}
		strcpy(User, W_USER);
		ASSERT(strlen(User) <= 10, "User - %s\n", User);

		DEBUG(4, "chkpth ok Rmtname - %s\n", Rmtname);
		sprintf(Dfile, "%s/TM.%05d.%03d", Spool, pnum, tmpnum++);
		if((fp = fopen(Dfile, "w")) == NULL) {
			WMESG(SNDFILE, NO);
			logent("CAN'T OPEN", "DENIED");
			unlink(Dfile);
			goto top;
		}
		chmod(Dfile, 0666);

		WMESG(SNDFILE, YES);
		ret = (*Rddata)(Ifn, fp);
		fclose(fp);
		if (ret != 0)
			return(FAIL);
		/* copy to user directory */
		status = xmv(Dfile, filename);
		WMESG(RQSTCMPT, status ? NO : YES);
		logent(status ? "FAILED" : "SUCCEEDED", "COPY");
		sscanf(W_MODE, "%o", &filemode);
		DEBUG(4, "mode - %o\n", filemode);
		if (filemode <= 0)
			filemode = 0666;
		if (status == 0) {
			filemode |= 0666;
			chmod(filename, filemode | 0666);
		}
		goto top;

	case RCVFILE:
		/*  MASTER section of RCVFILE  */

		DEBUG(4, "%s\n", "RCVFILE:");
		if (msg[1] == N) {
			logent("REQUEST", "DENIED");
			ASSERT(role == MASTER,
				"role - %d", role);
			unlink(Wfile);
			fclose(fp);
			goto top;
		}

		if (msg[1] == Y) {
			/* receive file */
			ASSERT(role == MASTER,
				"role - %d", role);
			ret = (*Rddata)(Ifn, fp);
			fclose(fp);
			if (ret != 0)
				return(FAIL);
			/* copy to user directory */
			if (isdir(filename)) {
				strcat(filename, "/");
				strcat(filename, lastpart(W_FILE1));
			}
			status = xmv(Dfile, filename);
			WMESG(RQSTCMPT, status ? NO : YES);
			logent(status ? "FAILED" : "SUCCEEDED", "COPY");
			notify(W_OPTNS, W_USER, filename, Rmtname,
			  status ? "failed" : "succeeded");
			sscanf(&msg[2], "%o", &filemode);
			DEBUG(4, "mode - %o\n", filemode);
			if (filemode <= 0)
				filemode = 0666;
			if (status == 0) {
				unlink(Dfile);
				filemode |= 0666;
				chmod(filename, filemode | 0666);
			}
			goto top;
		}

		/*  SLAVE section of RCVFILE  */
		ASSERT(role == SLAVE,
			"role - %d", role);

		/* request to send file */
		strcpy(rqstr, msg);
		logent(rqstr, "REQUESTED");

		/* check permissions */
		i = getargs(msg, wrkvec);
		ASSERT(i > 3, "ARG COUNT - %d\n", i);
		DEBUG(4, "msg - %s\n", msg);
		DEBUG(4, "W_FILE1 - %s\n", W_FILE1);
		strcpy(filename, W_FILE1);
		expfile(filename);
		if (isdir(filename)) {
			strcat(filename, "/");
			strcat(filename, lastpart(W_FILE2));
		}
		strcpy(User, W_USER);
		ASSERT(strlen(User) <= 10, "User - %s\n", User);
		if (chkpth("", Rmtname, filename) != 0) {
			WMESG(RCVFILE, NO);
			logent("DENIED", "PERMISSION");
			goto top;
		}
		DEBUG(4, "chkpth ok Rmtname - %s\n", Rmtname);

		if ((fp = fopen(filename, "r")) == NULL) {
			WMESG(RCVFILE, NO);
			logent("CAN'T OPEN", "DENIED");
			goto top;
		}

		/*  ok to send file */
		ret = stat(filename, &stbuf);
		ASSERT(ret != -1, "STAT FAILED %s", filename);
		sprintf(msg, "%s %o", YES, stbuf.st_mode & 0777);
		WMESG(RCVFILE, msg);
		ret = (*Wrdata)(fp, Ofn);
		fclose(fp);
		if (ret != 0)
			return(FAIL);
		RMESG(RQSTCMPT, msg);
		goto process;
	}
	return(FAIL);
}


/***
 *	rmesg(c, msg)	read message 'c'
 *	char *msg, c;
 *
 *	return code:  0  |  FAIL
 */

rmesg(c, msg)
char *msg, c;
{
	char str[50];

	DEBUG(4, "rmesg - '%c' ", c);
	if ((*Rdmsg)(msg, Ifn) != 0) {
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
char *s, m;
{
	DEBUG(4, "wmesg '%c'", m);
	DEBUG(4, "%.25s\n", s);
	return((*Wrmsg)(m, s, Ofn));
}


/***
 *	notify(options, user, file, sys, stwork)	mail results of copy
 *	char *options, *user, *file, *sys, *stword);
 *
 *	return codes:  none
 */

notify(options, user, file, sys, stword)
char *options, *user, *file, *sys, *stword;
{
	char str[200];
	if (index(options, 'm') == NULL)
		return;
	sprintf(str, "file %s, system %s, copy %s\n", file, sys, stword);
	mailst(user, str);
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
	extern imsg(), omsg();
	extern char *blptcl(), fptcl();
	char msg[BUFSIZ], str[BUFSIZ];

	Rdmsg = imsg;
	Wrmsg = omsg;
	if (role == MASTER) {
		RMESG(SLTPTCL, msg);
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
		RMESG(USEPTCL, msg);
		if (msg[1] == N) {
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
char *str;
{
	struct Proto *p;
	extern char *index();

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
char *str;
{
	struct Proto *p;
	char *s;

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
char *c;
{
	struct Proto *p;

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

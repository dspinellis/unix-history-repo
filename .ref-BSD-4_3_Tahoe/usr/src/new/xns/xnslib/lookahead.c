/*
 * This file implements functions used by both server and daemon
 * for the XNS courier library
 */

/*
 $Log:	lookahead.c,v $
 * Revision 2.1  86/11/19  06:24:48  jqj
 * Per Ed Flint:  in LookAheadCallMsg, don't decrement byteswanted when
 * skipping non-RPC packets.
 * 
 * Revision 2.0  85/11/21  07:22:10  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.4  85/09/28  06:54:25  jqj
 * 1/ 4.3 version.
 * 2/ fix bug in error reporting -- it had always reported NoSuchVersionNumber
 * even when NoSuchProgram was appropriate.
 * 
 */

#ifndef lint
static char rcsid[] = "$Header: lookahead.c,v 2.1 86/11/19 06:24:48 jqj Exp $";
#endif

#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>		/* for xn.h */
#include <sys/socket.h>
#include <sys/uio.h>
#include <netns/ns.h>		/* for XNS addresses and courierconnectin.h */
#include <netns/idp.h>
#include <netns/sp.h>		/* for spphdr */
#include "courier.h"
#include "realcourierconnection.h"
#include "courierdb.h"
#ifndef COURLIB
#define COURLIB "/usr/new/lib/xnscourier"
#endif

#define MAKEVEC(idx, addr, len) our_iovec[idx].iov_base = (caddr_t)addr;\
				our_iovec[idx].iov_len = len;

#if DEBUG
extern int CourierServerDebuggingFlag;
#endif

extern CourierConnection *_serverConnection;
extern Unspecified tid;

int
LookAheadCallMsg(progptr, versionptr, skippedwords)
	LongCardinal *progptr;
	Cardinal *versionptr;
	Unspecified skippedwords[];
/* Returns number of words set in skippedwords i.e. from packets we
 * had to read to get to the program/version pair.  Sets *progptr
 * to the program number, and *versionptr to the version number.
 */
/* Returns -1 if timeout expired.  SPP connection is closed. */
{
	register CourierConnection *f = _serverConnection;
	static struct timeval timeout = {90,0};	/* 90sec. timeout */
	int	fdmask,
		count,
		byteswanted,
		bytesread;
	struct sphdr hdrbuf;
	Unspecified databuf[MAXWORDS];
	Unspecified *bp;
	Cardinal msgtype;
	Unspecified msgtid;
	static Cardinal ourversion = COURIERVERSION;
	Cardinal versionl, versionh;
	static struct iovec our_iovec[3];	
	static struct msghdr ourmsg = {0, 0, our_iovec, 3, 0, 0};

	fdmask = 1<<(f->fd);
	count = 0;
	bytesread = 0;
	byteswanted = 14;	/* CverL, CverH, CALL, tid, Prg1, Prg2, Ver */
	MAKEVEC(0, &hdrbuf, sizeof(struct sphdr));
	MAKEVEC(1, skippedwords, byteswanted);
	MAKEVEC(2, databuf, SPPMAXDATA);
	/* wantversion =df need to read a courier version # from stream */
	if (f->state != wantversion) {
		/* pretend we've gotten a version */
		bp = skippedwords;
		bp += externalize_Cardinal(&ourversion, bp);
		bp += externalize_Cardinal(&ourversion, bp);
		bytesread += 4;
		byteswanted -= 4;
		our_iovec[1].iov_len -= 4;
		our_iovec[1].iov_base += 4;
		/* tell other routines there is a version */
		f->state = wantversion;
	}
	if (select(f->fd+1,&fdmask,(int*)NULL,(int*)NULL,&timeout) <= 0) {
		(void) sppclose(f->fd);
		f->state = closed;
		return(-1);
	}
	while (byteswanted > 0) {
		count = recvmsg(f->fd, &ourmsg, MSG_PEEK)
		  - sizeof(struct sphdr);
		if (count < 0 || hdrbuf.sp_dt == SPPSST_END) {
			(void) sppclosereply(f->fd);
			f->state = closed;
			return(-1);
		}
		if (hdrbuf.sp_dt != SPPSST_RPC &&
		    (bytesread > 0 || count != 4)) {
				/* throw away bad packets */
			(void) readv(f->fd, our_iovec, 3);
			continue;	/* don't decrement byteswanted */
		}
		else if (count <= byteswanted) {
				/* actually read the packet we peeked */
			count = readv(f->fd, our_iovec, 3) -
			  sizeof(struct sphdr);
			bytesread += count;
			our_iovec[1].iov_len -= count;
			our_iovec[1].iov_base += count;
		}
		byteswanted -= count;
	}
	bp = skippedwords;
	bp += internalize_Cardinal(&versionl, bp);
	bp += internalize_Cardinal(&versionh, bp);
	if (versionl > COURIERVERSION || versionh < COURIERVERSION) {
		(void) sppclose(f->fd);
		f->state = closed;
		return(-1);
		/*NOTREACHED*/
	}
	/*
	 * note we haven't actually read the packet containing the
	 * remote procedure number, though we may have PEEKed it.
	 */
	bp += internalize_Cardinal(&msgtype, bp);
	if (msgtype != CALL) {
		SendRejectMessage(unspecifiedError, 0, NULL);
		(void) sppclose(f->fd);
		f->state = closed;
		return(-1);
		/*NOTREACHED*/
	}
	bp += internalize_Unspecified(&msgtid, bp);
	bp += internalize_LongCardinal(progptr, bp);
	bp += internalize_Cardinal(versionptr, bp);
	return(bytesread/sizeof(Unspecified));
	/* all that work, and we have to do it over again */
}

ExecCourierProgram(programnum, versionnum, skipcount, skippedwords)
	LongCardinal programnum;
	Cardinal versionnum;
	int skipcount;
	Unspecified skippedwords[];
/*
 * Exec the appropriate courier program, passing it asciized skippedwords
 * in the argument list.
 * Does not return unless the exec failed or the server was not found.
 * If the server cannot be EXECed, then the appropriate message is sent
 * back on the wire and the current message is flushed.
 */
{
	struct courierdbent *cdbent;
	char *argv[12];
	int i, argc;
	extern char *malloc();
	char tmpbuf[1024];

	cdbent = getcourierservice(programnum, versionnum);
	if (cdbent != NULL &&
	    (cdbent->cr_serverbin == NULL || *cdbent->cr_serverbin == '\0')) {
		sprintf(tmpbuf,"%s/%s%dd",
			COURLIB,
			cdbent->cr_programname, cdbent->cr_version);
		if (access(tmpbuf,1) == 0)
		  	cdbent->cr_serverbin = tmpbuf;
	}
	if (cdbent == NULL || cdbent->cr_serverbin == NULL ||
	    *cdbent->cr_serverbin == '\0') {
		register Cardinal curval;
		Cardinal range[2];
		range[0] = 077777; range[1] = curval = 0;
		setcourierdbent();
		while ((cdbent = getcourierdbent()) != NULL) {
			if (cdbent->cr_programnumber != programnum) continue;
			curval = cdbent->cr_version;
			if (curval < range[0]) range[0] = curval;
			if (curval > range[1]) range[1] = curval;
		}
		Deallocate(ReadMessage(_serverConnection, NULL, 0));
		/* flush message */
		if (curval > 0)
		  SendRejectMessage(noSuchVersionNumber, 2, range);
		else SendRejectMessage(noSuchProgramNumber, 0, NULL);
#if DEBUG
		(void) fprintf(stderr, "xnscourierd: no program %d(%d)\n",
			       programnum, versionnum);
#endif
		return;		/* can't find server */
	}
	argc = 0;
	argv[argc] = malloc(4); /* allow 3 digits per file descriptor */
	sprintf(argv[argc++],"%d",(int)_serverConnection->fd);
	for (i = 0; i < skipcount; i++) {
		argv[argc] = malloc(8); /* allow 7 digits per Unspecified */
		sprintf(argv[argc++],"%d",(int) skippedwords[i]);
	}
	argv[argc] = (char *) 0;
	execv(cdbent->cr_serverbin, argv);
	Deallocate(ReadMessage(_serverConnection, NULL, 0));/* flush message */
	SendRejectMessage(unspecifiedError, 0, NULL);
#if DEBUG
	(void) fprintf(stderr, "xnscourierd: can't exec %s\n",
		       cdbent->cr_serverbin);
#endif
	return;
}


SendRejectMessage(rejecttype, nwords, arguments)
	Cardinal rejecttype;
	Cardinal nwords;
	Unspecified *arguments;
{
#define REJECTHDRLEN 3
	static Cardinal msgtype = REJECT;
	Unspecified *bp, buf[REJECTHDRLEN];

#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[SendRejectMessage %d, length %d]\n",
			rejecttype, nwords);
#endif
	bp = buf;
	bp += externalize_Cardinal(&msgtype, bp);
	bp += externalize_Unspecified(&tid, bp);
	bp += externalize_Cardinal(&rejecttype, bp);
	CourierWrite(_serverConnection, (bp-buf), buf, nwords, arguments);
}


SendAbortMessage(errorvalue, nwords, arguments)
	LongCardinal errorvalue;
	Cardinal nwords;
	Unspecified *arguments;
/* note that arguments does NOT include the error value */
{
#define ABORTHDRLEN 3
	Cardinal shorterror;
	static Cardinal msgtype = ABORT;
	Unspecified *bp, buf[ABORTHDRLEN];

#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[SendAbortMessage %d %d]\n",
				errorvalue, nwords);
#endif
	bp = buf;
	bp += externalize_Cardinal(&msgtype, bp);
	bp += externalize_Unspecified(&tid, bp);
	shorterror = (Cardinal) (errorvalue - ERROR_OFFSET);
	bp += externalize_Cardinal(&shorterror, bp);
	CourierWrite(_serverConnection, (bp-buf), buf, nwords, arguments);
}

/*ARGSUSED*/
NoSuchProcedureValue(prog_name, proc)
	String prog_name;
	Cardinal proc;
{
	SendRejectMessage(noSuchProcedureValue, 0, (Unspecified*) NULL);
#if DEBUG
	if (CourierServerDebuggingFlag)
		fprintf(stderr, "[NoSuchProcedureValue %d in %s]\n",
			proc, prog_name);
#endif
}

/*
 * $XConsortium: Xstreams.c,v 1.26 91/07/23 12:15:13 rws Exp $
 */

#ifdef STREAMSCONN

/*
 * Copyright 1991 USL, Inc.
 * Copyright 1991 Massachusetts Institute of Technology
 * Copyright 1988, 1989 AT&T, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that copyright notice and this permission
 * notice appear in supporting documentation, and that the name of
 * AT&T, USL, or MIT not be used in advertising or publicity
 * pertaining to distribution of the software without specific,
 * written prior permission.  AT&T, USL, and MIT make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * AT&T, USL, AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL AT&T, USL, OR MIT BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#define _USHORT_H	/* prevent conflicts between BSD sys/types.h and
                           interlan/il_types.h */

#define NEED_REPLIES
#include <X11/Xlibint.h>
#include <X11/Xos.h>
#include "Xlibnet.h"
#include <X11/Xauth.h>
#include <X11/Xproto.h>

#include <stdio.h>
#include <tiuser.h>		/* TLI user defs */
#include <sys/param.h>
#include <sys/utsname.h>
#include <signal.h>

#include <sys/stat.h>
#include <errno.h>
#include <sys/stropts.h>

extern int errno;
extern char *sys_errlist[];

#ifdef SVR4
#include <netdir.h>
#include <netconfig.h>
#ifndef __STDC__
/* buggy SVR4 include file */
char *setnetpath();
struct netconfig *getnetconfigent();
struct netconfig *getnetpath();
int endnetpath();
#endif
#endif


#include "Xstreams.h"


#ifdef DEBUG
#define PRMSG(x,a,b)	fprintf(stderr, x,a,b); fflush(stderr)
#else
#define PRMSG(x,a,b)
#endif


#define	LISTEN_QUE_SIZE	8	/* maximum # of connections for gen. listen */
#define	CLEAR		1
/*
 * Ridiculously high value for maximum number of connects per stream.
 * Transport Provider will determine actual maximum to be used.
 */

#define	MAXCONNECTIONS	100	/* maximum # of connections for gen. listen */


#define MAXLEN	80
#define BUFFERSIZE 2048
#define NOBUFFERNEEDED 512


typedef struct {
	char *DataBuffer;
	int   FirstBytePtr;
	int   LastBytePtr;
	} InputBuffer;

#ifdef SVR4
InputBuffer _XsInputBuffer[MAXCONNECTIONS] = {NULL};
#else
#ifndef NOFILES_MAX
#define NOFILES_MAX	128
#endif
InputBuffer _XsInputBuffer[NOFILES_MAX] = {NULL};
#endif /* SVR4*/


static char	*ptmx = "/dev/ptmx";
static char	*dispno = "0";

#ifdef  att
extern char *GetXWINHome ();
#endif

static  char _dispno[MAX_DISP_DIGITS];

extern int t_errno;

static char	** addheader();
static char	** addtliheader();

static	struct	t_bind bind_ret, bind_req;
static	struct	t_call call;
static  char	ret_buf[MAXLEN], req_buf[MAXLEN], call_buf[MAXLEN];


static int named = 0;	/* not using named streams connection */

/*
** The following stubs functions should be kept to keep the
** att libX11_s library (Shared library) happy. In the early versions
** of XWIN, these functions were used.
*/

#ifdef USL_COMPAT
void CloseTcpStream () {}
void WriteTcpStream () {}
void ReadTcpStream () {}
void CallTcpServer () {}
void ConnectTcpClient () {}
void SetupTcpStream () {}

int CloseTliStream(){}
int WriteTliStream(){}
int ReadTliStream(){} 
int CloseLocalStream(){} 
int WriteLocalStream(){} 
#endif /* USL_COMPAT */

#ifdef USL_SHARELIB
#define fopen (*_libX_fopen)
extern FILE *fopen();
#define t_bind (*_libX_t_bind)
extern int t_bind();
#undef t_bind
#define _iob (*_libX__iob)
extern FILE _iob[];
#endif /* USL_SHARELIB */

extern int t_errno;

#ifdef SYSV
#define SIGNAL_T int
#else
#define SIGNAL_T void
#endif
typedef SIGNAL_T (*PFV)();
extern PFV signal();

#define SUCCESS		"1"

extern char _XsTypeOfStream[];
extern Xstream _XsStream[];

static networkInfo Network;
static int NameServer = -1;
static int CallTheNameServer();
static void checkNewEvent();
static int OpenVirtualCircuit();
static void LookForEvents();
static int CheckListenQue();
static void ClearCall(), RemoveCall();
static int OpenLocalServer();
static int OpenNamedServer();
static int nameserver();

/* Routines everybody shares */



_XsErrorCall()
{
	fprintf(stderr, "ErrorCall: invalid or unsupported subroutine call\n");
	return(-1);
}

/*
 * Following are some general queueing routines.  The call list head contains
 * a pointer to the head of the queue and to the tail of the queue.  Normally,
 * calls are added to the tail and removed from the head to ensure they are
 * processed in the order received, however, because of the possible interruption
 * of an acceptance with the resulting requeueing, it is necessary to have a
 * way to do a "priority queueing" which inserts at the head of the queue for
 * immediate processing
 */

/*
 * Que:
 *
 * add calls to tail of queue
 */


static	void
Que(head, lc, flag)
register struct listenQue *head;
register struct listenCall *lc;
char	 flag;
{
	if(flag == CLEAR)
		ClearCall(lc->CurrentCall);
	
	if (head->QueTail == (struct listenCall *) NULL) {
		lc->NextCall = (struct listenCall *) NULL;
		head->QueHead = head->QueTail = lc;
	}
	else {
		lc->NextCall = head->QueTail->NextCall;
		head->QueTail->NextCall = lc;
		head->QueTail = lc;
	}
}


/*
 * pQue:
 *
 * priority queuer, add calls to head of queue
 */

static void
pQue(head, lc)
register struct listenQue *head;
register struct listenCall *lc;
{
	if (head->QueHead == (struct listenCall *) NULL) {
		lc->NextCall = (struct listenCall *) NULL;
		head->QueHead = head->QueTail = lc;
	}
	else {
		lc->NextCall = head->QueHead;
		head->QueHead = lc;
	}
}


/*
 * dequeue:
 *
 * remove a call from the head of queue
 */


static struct listenCall *
deQue(head)
register struct listenQue *head;
{
	register struct listenCall *ret;

	if (head->QueHead == (struct listenCall *) NULL){
		PRMSG("Fatal error. Queue is empty (shouldn't happen)\n",0,0);
		exit(1);	
		}
	ret = head->QueHead;
	head->QueHead = ret->NextCall;
	if (head->QueHead == (struct listenCall *) NULL)
		head->QueTail = (struct listenCall *) NULL;
	return(ret);
}





/* Routines for handling local Named streams  */

#ifdef SVR4
_XsSetupNamedStream(display, stype)
char *	display;
char	*stype;
{
	int 	munix, sunix;
	char *	slave;
	char	buf[MAX_AUTO_BUF_LEN];
	int	type = X_NAMED_STREAM;
	int 	fld[2], ret;
	struct stat sbuf;

	PRMSG("Calling SetupNamedStream()\n",0,0);

/* if file not there create it, depends on SetupLocalStream to decide whether
   server already running  , no checking is done here */

	munix = atoi(display);
	sprintf(buf, "%s.%d", NAMED_LISTENER, munix);
	PRMSG("Calling SetupNamedStream()-(%s)\n",buf,0);

	if(stat(buf,  &sbuf)!= 0)	{
	   if(errno ==ENOENT)	{
		if(( munix = creat(buf, (mode_t) 0666) ) == -1)	{
		    PRMSG(" Can't create: %s\n", buf,0);
		    return(-1);
		}
		close(munix);
		if(chmod(buf,(mode_t) 0666)<0)	{
		    PRMSG( "Cannot chmod %s", buf,0);
		    perror(" ");
		    return(-1);
		}
	   }
	   else	{
		PRMSG("stat err=%d,-%s\n", errno, sys_errlist[errno]);
		return(-1);
	   }
	}

	if(pipe(fld) != 0)	{
	    fprintf(stderr,"pipe failed, errno=%d:%s\n", errno, sys_errlist[errno]);
	    return(-1);
	}

	if((ret=ioctl(fld[0], I_PUSH,"connld")) != 0)	{
	    fprintf(stderr,"ioctl error:%s\n", sys_errlist[errno]);
	    return(-1);
	}

	if((fattach(fld[0], buf)) !=0)	{
	    fprintf(stderr,"fattach failed:%s\n", sys_errlist[errno]);
	    return(-1);
	}

	_XsTypeOfStream[fld[1]] = type;
	NetworkInfo->_nnets++;

	return(fld[1]);
}

/* Enhanced Application Compatibility Support */

int
_XsSetupSpStream (display, stype)
char *display;
char *stype;
{
}

int
_XsConnectSpClient (connmaster)		/* Add connection to new slave */
	int			connmaster; /* Master request connection */
{
}
/* End Enhanced Application Compatibility Support */

#endif /* SVR4 */




/* Routines for handling local streams (streams-pipes) */

_XsSetupLocalStream(display, stype)
char *	display;
char	*stype;
{
	int 	munix, sunix;
	char *	slave;
	char	buf[MAX_AUTO_BUF_LEN];
	int	type = X_LOCAL_STREAM;
	int 	nameserver();

	PRMSG("Calling SetupLocalStream()\n",0,0);

	SetupNetworkInfo();
	dispno = display;

	NetworkInfo->_nnets = NetworkInfo->_npeers = 0;
	NetworkInfo->_peer = NULL;
	NetworkInfo->_peerlen = NULL;

#ifdef SVR4
	NetworkInfo->_net[0] = (struct netconfig *) 0;
#else
	NetworkInfo->_net[0] = (char *) 0;
#endif
	NetworkInfo->_nnets++;


	munix = atoi(display);
/*
	if(munix != 0){
		fprintf(stderr, "Only display # 0 can be used on this server\n");
		return(-1);
		}
*/
			
	sprintf(buf, "%s.%d", LOCAL_LISTENER, munix);
/*
	if(open(buf, O_RDWR) >= 0){
		fprintf(stderr, "Server is already running\n");
		return(-1);
		}
*/
	if( (munix = open(ptmx, O_RDWR)) < 0 ){
		fprintf(stderr,"Cannot open %s", ptmx);
		perror(" ");
		return(-1);
	}
	grantpt(munix);
	unlockpt(munix);

	if(unlink(buf) < 0 && errno != ENOENT){
		fprintf(stderr, "Cannot unlink %s", buf);
		perror(" ");
		return(-1);
		}

        if(! (slave = (char *) ptsname(munix))) {
		close(munix);
		perror("Cannot get slave pt-name");
		return(-1);
		}

	if( link(slave, buf) <0 ){
		fprintf(stderr, "Cannot link %s to %s", slave, buf);
		perror(" ");
		return(-1);
		}
	if( chmod(buf, 0666) < 0){
		close(munix);
		fprintf(stderr, "Cannot chmod %s", buf);
		perror(" ");
		return(-1);
		}

	sunix = open(buf, O_RDWR);
	if(sunix < 0){
		fprintf(stderr, "Cannot open %s", buf);
		perror(" ");
		close(munix);
		return(-1);
		}

	_XsTypeOfStream[munix] = type;
	_XsTypeOfStream[sunix] = CLOSED_STREAM;

	return(munix);
}

_XsConnectLocalClient(ufd, MoreConnections)

int	ufd;
char	* MoreConnections;
{
	
	int fd;
	int read_in;
	unsigned char length;
	char buf[MAX_AUTO_BUF_LEN];
#ifdef SVR4
	struct strrecvfd str;
#endif 


	PRMSG("Calling ConnectLocalClient(%d)\n", ufd,0);

/* MoreConnections is set to zero because if any more connections are underway
 * select() will return immediately. It is nicer if we can process all connections
 * that exist the way we handle TLI connections by setting MoreConnections.
 * May be I will end up doing it later.
 */
	*MoreConnections = 0;

#ifdef SVR4

    if( _XsTypeOfStream[ufd] == X_NAMED_STREAM)		{
	PRMSG("Calling ConnectLocalClient(%d) - thru named streams\n", ufd,0);
	if (ioctl(ufd, I_RECVFD, &str) < 0)	{
            fprintf(stderr,"I_RECVFD failed\n");
            return(-1);
	}

	_XsTypeOfStream[str.fd] = _XsTypeOfStream[ufd];
	PRMSG("ConnectNamedClient(%d) return success\n", str.fd,0);
	return(str.fd);
    }

/* Enhanced Application Compatibility Support */

/* End Enhanced Application Compatibility Support */

#endif /* SVR4 */

	PRMSG("Calling ConnectLocalClient(%d) - thru psuedo tty\n", ufd,0);

	if( (read_in = read(ufd, &length, 1)) <= 0 ){
		if( !read_in )  /* client closed fd */
			perror("0 bytes read");
		else	perror("Error in reading the local connection msg length");
		return(-1);
		}


	if( (read_in = read(ufd, buf, length)) <= 0 ){
		if( !read_in )  /* client closed fd */
			perror("0 bytes read");
		else	perror("Error in reading the local connection slave name");
		return(-1);
		}

	buf[ length ] = '\0';

	if( (fd = open(buf,O_RDWR)) < 0 ){
		strcat(buf," open fail, clientfd");
		perror(buf);
		return(-1);
		}

	write(fd,SUCCESS,1);

	_XsTypeOfStream[fd] = _XsTypeOfStream[ufd];
	PRMSG("ConnectLocalClient(%d) return success\n", ufd,0);
	return(fd);
}

static void dummy (sig)
int sig;
{
}

_XsCallLocalServer(host, idisplay, nettype)
char	*host;
int	idisplay;
#ifdef SVR4
struct netconfig *nettype;
#else
char	*nettype;
#endif

{
	char	buf[MAX_AUTO_BUF_LEN];
	char    *listener;
	int	type = X_LOCAL_STREAM;
	int	fd;


	PRMSG("Calling CallLocalServer(%s)\n", host,0);

        sprintf(_dispno, "%d", idisplay);
	dispno = _dispno;

	/*
	 * Open channel to server
	 */
	

#ifdef SVR4
	if (strncmp("NAMED", (char *) getenv("XLOCAL"),	 5) == 0)	{
                    named = 1;
                    type = X_NAMED_STREAM;
                    listener = NAMED_LISTENER;
                    sprintf(buf, "%s.%d", listener, idisplay);
                    if((fd = OpenNamedServer(buf)) < 0)
                    {
			PRMSG("Cannot open %s\n", buf,0);
	#ifdef DEBUG
			perror("XIO");	/* Sorry, but I don't have the dpy handy */
	#endif
			return(-1);
                    }
	}

/* Enhanced Application Compatibility Support */

/* End Enhanced Application Compatibility Support */
#endif


	if(!named)	{
            type = X_LOCAL_STREAM;
            listener = LOCAL_LISTENER;
            sprintf(buf, "%s.%d", listener, idisplay);
	PRMSG("buf for local listener %s\n",buf,0);
            if((fd = OpenLocalServer(buf)) < 0)
            {
		PRMSG("Cannot open %s\n", buf,0);
#ifdef DEBUG
		perror("XIO");	/* Sorry, but I don't have the dpy handy */
#endif
		return(-1);
            }
	}


	_XsTypeOfStream[fd] = type;
        if (_XsInputBuffer[fd].DataBuffer == NULL)
		if ((_XsInputBuffer[fd].DataBuffer = (char *) malloc(BUFFERSIZE)) == NULL)
        	{
	               errno = ENOMEM;
                       perror("Client can't connect to local server");
                       return (-1);
        	}
	_XsInputBuffer[fd].LastBytePtr = 0;
	_XsInputBuffer[fd].FirstBytePtr = 0;

	PRMSG("Calling CallLocalServer(%s) return success\n", host,fd);

	return(fd);
}




#ifdef SVR4
static int
OpenNamedServer(node)
char *node;
{
	int fld;
	
	PRMSG("Calling 4.0 -- opening (%s)\n", node,0);

	fld = open(node, O_RDWR);
	if(fld <0)	{
	    fprintf(stderr,"OpenNamedServer failed:%s\n", sys_errlist[errno]);
	    return(-1);
	}

	if(isastream(fld) != 1)	{
	    fprintf(stderr,"OpenNamedServer failed: %s is not a NamedStream\n",
			node);
	    return(-1);
	}

	return (fld);
}

/* Enhanced Application Compatibility Support */

static int
_XsOpenSpServer(node)
char *node;
{
}
/* End Enhanced Application Compatibility Support */

#endif /* SVR4 */

static int
OpenLocalServer(node)
    char	*node;
{
	int 	server, fd, c;
	char	buf[MAX_AUTO_BUF_LEN], *slave;
	PFV	savef;

	if ((server = open (node, O_RDWR)) < 0) 
	{
#ifdef DEBUG
		fprintf(stderr, "open(%s) failed\n", node);
		perror(" ");
#endif
		return(-1);
	}

	/*
	 * Open streams based pipe and get slave name
	 */


	if ((fd = open (ptmx, O_RDWR)) < 0) {
		close (server);
		PRMSG("Cannot open %s\n", ptmx, 0);
		return(-1);
	}

	grantpt (fd);

	unlockpt (fd);

        if (! (slave = (char *) ptsname(fd))) {
		close(fd);
		close(server);
		PRMSG("Cannot get slave pt-name", 0, 0);
		return(-1);
	}


	if (chmod(slave, 0666) < 0) 
	{
		close(fd);
		close(server);
		PRMSG("Cannot chmod %s\n", buf,0);
		return(-1);
	}

	c = strlen (slave);
	
	buf[0] = c;
	sprintf(&buf[1], slave);
	
	/*
	 * write slave name to server
	 */

	write(server, buf, c+1);
	close (server);
	/*
	 * wait for server to respond
	 */
	savef = signal (SIGALRM, dummy);
	alarm (CONNECT_TIMEOUT);

	if (read (fd, &c, 1) != 1) 
	{
		fprintf(stderr, "No reply from the server.\n");
		close(fd);
		fd = -1;
	}	
	alarm (0);
	signal (SIGALRM, savef);

	return(fd);
}

#ifdef DEBUG
static dumpBytes (len, data)
    int len;
    char *data;
{
	int i;

	fprintf(stderr, "%d: ", len);
	for (i = 0; i < len; i++)
		fprintf(stderr, "%02x ", data[i] & 0377);
	fprintf(stderr, "\n");
}
#endif

#define XSTREAMS_COMPILE /* magic symbol to avoid lint problems */
#ifdef SVR4
#include "nameaddr.c"
#else
#include "nameserver.c"
#endif
#undef XSTREAMS_COMPILE


_XsReadLocalStream(fd, buf, count, do_buffering)

int	fd;
char	*buf;
int	count;
int	do_buffering;
{
	int	amount;
	InputBuffer *ioptr = &_XsInputBuffer[fd];


	if (do_buffering == NO_BUFFERING){
		amount = read(fd, buf, count);

  		return (amount);
	}

        if (ioptr->LastBytePtr <= ioptr->FirstBytePtr)
	{
           errno = 0;
           if(count > BUFFERSIZE)
           {
            	ioptr->LastBytePtr = ioptr->FirstBytePtr = 0;
		amount = read(fd, buf, count);

                return(amount);
           }
           ioptr->LastBytePtr = read(fd, ioptr->DataBuffer, BUFFERSIZE);
	
	   ioptr->FirstBytePtr = 0;
	}

	if (ioptr->LastBytePtr > 0)
	{
           amount = ioptr->LastBytePtr - ioptr->FirstBytePtr;
           amount = amount > count ? count : amount;
           memcpy(buf, &ioptr->DataBuffer[ioptr->FirstBytePtr], amount);
           ioptr->FirstBytePtr += amount;
           return amount;
	}
	else	{
	  return (ioptr->LastBytePtr);
	}

}

_XsWriteLocalStream(fd, buf, count)
int	fd;
char	*buf;
int	count;
{
/* obsolete code */
	return (write(fd, buf, count));
}

_XsCloseLocalStream(fd)
int	fd;
{
/* obvsolete code */
	return (close(fd));
} 




_XsConnectTliClient(sfd,MoreConnections)

int    sfd;
char  * MoreConnections;
{
	register	char	type = _XsTypeOfStream[sfd];
	register	struct  listenQue *freeq, *pendq;

	freeq = &Network.FreeList[type];
	pendq = &Network.PendingQue[type];

	PRMSG("Calling ConnectTliClient(%d)\n", sfd,0);
	LookForEvents(freeq, pendq, sfd);
	return (CheckListenQue(freeq, pendq, sfd, MoreConnections));
}


static void
checkNewEvent(fd)
int	fd;
{
	int	t;

        t = t_look(fd);
        switch(t)
        {
        case T_DATA	  :
	        fprintf(stderr, "T_DATA received\n");	    
		break;
  	case T_EXDATA	  :
       		fprintf(stderr, "T_EXDATA received\n");    
		break;
  	case T_DISCONNECT :
	        t_rcvdis(fd, NULL);
       		fprintf(stderr, "T_DISCONNECT received\n");
		break;
 	case T_ERROR	  :
        	fprintf(stderr, "T_ERROR received\n");	    
		break;
  	case T_UDERR	  :
        	fprintf(stderr, "T_UDERR received\n");	    
		break;
  	case T_ORDREL	  :
        	fprintf(stderr, "T_ORDREL received\n");    
		break;
  	}
}

_XsReadTliStream(fd, buf, count, do_buffering)
int	fd;
char	*buf;
int	count;
int	do_buffering;
{
	int n;
	int flags;

/* obsolete code */
}

_XsWriteTliStream(fd, buf, count)
int	fd;
char	*buf;
int	count;
{


/* obsolete code */
}

static void
OnError(sig)
int	sig;
{
}

_XsCloseTliStream(fd)
int	fd;
{


/* obsolete code */
}

/*
 * LookForEvents:	handle an asynchronous event
 */

static void
LookForEvents(FreeHead, PendHead, fd)
struct listenQue *FreeHead;
struct listenQue *PendHead;
int fd;
{
	int	address;
	short	port, nf;
	struct t_discon disc;
	register struct listenCall *current;
	register struct t_call *call;
	int t;
	char	buf[MAX_AUTO_BUF_LEN];
	int	flag, i;

	if((t = t_look(fd)) < 0) {
		PRMSG("t_look failed. t_errno %d\n", t_errno,0);
		return;
		}
	switch (t) {
	case 0:
		PRMSG("t_look 0\n",0,0);
		break;
		/* no return */
	case T_LISTEN:
		PRMSG("t_look T_LISTEN\n",0,0);
		current = deQue(FreeHead);
		call = current->CurrentCall;

		if (t_listen(fd, call) < 0) {
			PRMSG("t_listen failed\n",0,0);
			return;
		}

		Que(PendHead, current, ~CLEAR);
		PRMSG("incoming call seq # %d", call->sequence,0);
		break;
	case T_DISCONNECT:
		PRMSG("t_look T_DISCONNECT\n",0,0);
		if (t_rcvdis(fd, &disc) < 0) {
			PRMSG("Received T_DISCONNECT but t_rcvdis failed\n",0,0);
			exit(1);	
		}
		PRMSG("incoming disconnect seq # %d", disc.sequence,0);
		RemoveCall(FreeHead, PendHead, &disc);
		t_close(fd);
		_XsTypeOfStream[fd] = -1;
		break;
	case T_DATA :
		if((i = t_rcv(fd, buf, MAX_AUTO_BUF_LEN, &flag)) > 0)
		break;
	default:
		PRMSG("t_look default %o %x\n", t, t);
		break;
	}
}


/*
 * CheckListenQue:	try to accept a connection
 */

static int
CheckListenQue(FreeHead, PendHead, fd, MoreConnections)
struct listenQue *FreeHead;
struct listenQue *PendHead;
int fd;
char * MoreConnections;
{
	register struct listenCall *current;
	register struct t_call *call;
	int pid, nfd, n;
	char	*retptr, *ptr;

	int	address;
	short	port, nf;

	PRMSG( "in CheckListenQue",0,0);
	if (!(EMPTY(PendHead))) 
	{
		current = deQue(PendHead);
		call = current->CurrentCall;
		PRMSG( "try to accept #%d", call->sequence,0);
		if((nfd = OpenVirtualCircuit(fd)) < 0)
		{
			PRMSG( "OpenVirtualCircuit failed\n",0,0);
			Que(FreeHead, current, CLEAR);
			*MoreConnections = !EMPTY(PendHead);
			return(-1);  /* let transport provider generate disconnect */
		}

		n = t_accept(fd, nfd, call);
		if (n < 0){

			PRMSG( "t_accept failed\n",0,0);
			if (t_errno == TLOOK) {
				t_close(nfd);
				PRMSG( "t_accept collision",0,0);
				PRMSG( "save call #%d", call->sequence,0);
				pQue(PendHead, current);
				*MoreConnections = !EMPTY(PendHead);
				return(-1);
			}
			else {
				PRMSG( "t_accept failed but not t_look\n",0,0);
				t_close(nfd);
				Que(FreeHead, current, CLEAR);
				*MoreConnections = !EMPTY(PendHead);
				return(-1);
			}
		}
		_XsTypeOfStream[nfd] = _XsTypeOfStream[fd];
		retptr = NULL;




		if( GetNetworkInfo (nfd, Network._net[_XsTypeOfStream[fd]],
		 ConvertTliCallToName, addtliheader(call),  &retptr, NULL) <= 0)
		
		{
			retptr = NULL;

		}
		ptr = NULL;
		if(retptr != NULL)
		{
			ptr = retptr;
			retptr += sizeof(xHostEntry);
		}
		GetNetworkInfo (nfd, Network._net[_XsTypeOfStream[fd]], PEER_ALLOC, &retptr, NULL);
		if(ptr != NULL)
			Xfree(ptr);
		PRMSG( "Accepted call %d", call->sequence,0);
		PRMSG("Channel %d is opened\n", nfd,0);

		Que(FreeHead, current, CLEAR);


                (void) ioctl(nfd, I_POP, "timod");
		if(ioctl(nfd, I_PUSH, "tirdwr") < 0)
		{
                 	t_close(nfd);
                        return(-1);
		}

		PRMSG( "Accepted call %d", call->sequence,0);
		PRMSG("Channel %d is opened\n", nfd,0);

		*MoreConnections = !EMPTY(PendHead);
		return(nfd);
	}	

	*MoreConnections = !EMPTY(PendHead);
	return(-1);
}


/*
 * ClearCall:	clear out a call structure
 */

static void
ClearCall(call)
struct t_call *call;
{
	call->sequence = 0;
	call->addr.len = 0;
	call->opt.len = 0;
	call->udata.len = 0;
	memset(call->addr.buf, 0, call->addr.maxlen);
	memset(call->opt.buf, 0, call->opt.maxlen);
	memset(call->udata.buf, 0, call->udata.maxlen);
}


/*
 * RemoveCall: remove call from pending list
 */

static void
RemoveCall(freeq, pendq, disc)
struct listenQue *freeq;
struct listenQue *pendq;
struct t_discon *disc;
{
	register struct listenCall *p, *oldp;

	PRMSG( "Removing call, sequence # is %d", disc->sequence,0);
	if (EMPTY(pendq)) {
		disc->sequence = -1;
		return;
	}
	p = pendq->QueHead;
	oldp = (struct listenCall *) NULL;
	while (p) {
		if (p->CurrentCall->sequence == disc->sequence) {
			if (oldp == (struct listenCall *) NULL) {
				pendq->QueHead = p->NextCall;
				if (pendq->QueHead == (struct listenCall *) NULL) {
					pendq->QueTail = (struct listenCall *) NULL;
				}
			}
			else if (p == pendq->QueTail) {
				oldp->NextCall = p->NextCall;
				pendq->QueTail = oldp;
			}
			else {
				oldp->NextCall = p->NextCall;
			}
			Que(freeq, p, CLEAR);
			disc->sequence = -1;
			return;
		}
		oldp = p;
		p = p->NextCall;
	}
	disc->sequence = -1;
	return;
}


static int
nameserver(fd, nettype, service, arg1, arg2, arg3)
    int       fd;
#ifdef SVR4
    struct netconfig *nettype;
#else
    char     *nettype;
#endif 
    int	      service;
    char     **arg1, **arg2;
    int	     *arg3;
{

    char	*ptr;
    int	n;
    int	type;

	if (fd >= 0)
	    type = _XsTypeOfStream[fd];
	else
	    type = X_TLI_STREAM;

	if(type < X_TLI_STREAM || type >= Network._nnets)
	{
		if(type == X_LOCAL_STREAM || type == X_NAMED_STREAM)
			return(0);
		if(fd >= 0)

    		{
		PRMSG("in nameserver type %d unknown d\n", type, fd);
		return(-1);
		}
    	}

    if(nettype == NULL)
	nettype = Network._net[type];


    switch(service){
      case	OpenDaemonConnection :
#ifdef SVR4 
	return(InitializeNetPath());
#else
	if(NameServer < 0 )
	    NameServer = OpenLocalServer(NAME_SERVER_NODE);
	return(NameServer);
#endif /* SVR4 */

      case	ConvertTliCallToName :
      case	ConvertNetAddrToName :
      case	ConvertNameToNetAddr :
      case	ConvertNameToTliBind :
      case	ConvertNameToTliCall :
	if((n = CallTheNameServer(service, nettype, arg1, arg2, arg3)) < 0)
	    return(-1);
	return(n);

      case	PEER_NAME  :
	if( fd < Network._npeers )
	{
	    *arg2 = Network._peer[fd];
	    return(1);
	}
	return(-1);
      case	PEER_ALLOC :
	if(fd >= Network._npeers)
	    return(-1);

	if(*arg1 == NULL){
	    n = 0;
	}
	else	n = strlen(*arg1);

	Network._peerlen[fd] = n;

	if(n > 0){
	    if(Network._peerlen[fd] > UNAME_LENGTH)
		Network._peerlen[fd] = UNAME_LENGTH;
	    bcopy(*arg1, Network._peer[fd], Network._peerlen[fd]);
	    Network._peer[fd][Network._peerlen[fd]] = '\0';
	}
	else {	
	    Network._peer[fd][0] = '\0';
	}
	return(1);
				
      case	PEER_FREE  :
	if(fd < Network._npeers && Network._peer[fd] != NULL)
	{
	    Network._peer[fd][0] = '\0';
	    Network._peerlen[fd] = 0;
	}
	return(1);
    }
}


static	int	_hlen = 0;
static	char	*_hptr = NULL;

static char	**
addheader(string, len)
char	*string;
int	len;
{

	int	n, m, p;
	char	*ptr;

	n = len;
	m = n + sizeof(xHostEntry);
	p = m + 2 * sizeof(int);
	
	if(p > _hlen){
		if(_hptr == NULL)
			_hptr = malloc(p);
		else	_hptr = realloc(_hptr, p);
		}
	if(_hptr == NULL){
		fprintf(stderr, "addheader(): malloc failed\n");
		exit(1);
		}
	else if(p > _hlen)
		_hlen = p;
		
	ptr = _hptr;
	
	*(int *) ptr = m;
	ptr += sizeof(int);
	*(int *) ptr = 1;
	ptr += sizeof(int);

	((xHostEntry *) ptr)-> length = n;
	ptr += sizeof(xHostEntry);
	memcpy(ptr, string, n);
	
	return(&_hptr);
}

static char	**
addtliheader(call)
struct t_call *call;
{

	
	char	*ptr;
	int	a, o, u;
	int	ra, ro, ru, rentlen;


	a = call->addr.len;
	o = call->opt.len;
	u = call->udata.len;

	ra = ((a + sizeof(xHostEntry) +3) >>2) << 2;
	ro = ((o + sizeof(xHostEntry) +3) >>2) << 2;
	ru = ((u + sizeof(xHostEntry) +3) >>2) << 2;

	rentlen = ra + ro + ru + 2 * sizeof(int);

	if(rentlen > _hlen){
		if(_hptr == NULL)
                        _hptr = malloc(rentlen);
		else	_hptr = realloc(_hptr, rentlen);
		}
	if(_hptr == NULL){
		fprintf(stderr, "addheader(): malloc failed\n");
		exit(1);
		}
	else if(rentlen > _hlen)
		_hlen = rentlen;

        ptr = _hptr;
	
        *(int *) ptr = rentlen - 2 * sizeof(int);
	ptr += sizeof(int);
	*(int *) ptr = 1;
	ptr += sizeof(int);

	((xHostEntry *) ptr)-> length = a;
	if(a > 0){
		memcpy(ptr + sizeof(xHostEntry), call->addr.buf, a);
		}

	
	ptr += ra;
	((xHostEntry *) ptr)-> length = o;
	if(o > 0)
		memcpy(ptr + sizeof(xHostEntry), call->opt.buf, o);
	
	ptr += ro;
	((xHostEntry *) ptr)-> length = u;
	if(u > 0){
		memcpy(ptr + sizeof(xHostEntry), call->udata.buf, u);
                }

	return(&_hptr);
}


int _XBytesReadable (fd, ptr)
int fd;
int * ptr;
{
	int inbuf;
	int n;
	int flg;
	InputBuffer *ioptr = &_XsInputBuffer[fd];

	inbuf = ioptr->LastBytePtr - ioptr->FirstBytePtr;

        if (inbuf >= SIZEOF(xReply))
	{
		*ptr = inbuf;
		return (0);
	}

	if (ioptr->FirstBytePtr > 0)
	{
		/* move tidbit to front of buffer */
		bcopy(&ioptr->DataBuffer[ioptr->FirstBytePtr],
		      ioptr->DataBuffer, inbuf);

		/* Adjust pointers in buffer to reflect move */
		ioptr->LastBytePtr = inbuf;
		ioptr->FirstBytePtr = 0;
	}

	if (inbuf < 0)
	{
		inbuf = 0;
		ioptr->LastBytePtr = 0;
	}
	/* Read no more than number of bytes left in buffer */

        errno = 0;


   	n = read(fd, &ioptr->DataBuffer[inbuf], BUFFERSIZE-inbuf);
	if (n > 0)
	{
		ioptr->LastBytePtr += n;
		*ptr = ioptr->LastBytePtr;
		return (0);
	}
	else
	{
		if (errno == EWOULDBLOCK)
		{
			*ptr = ioptr->LastBytePtr;
			return (0);
		}
		else
		{
			if (n == 0 )
			{
				errno = EPIPE;
				return (-1);
			}
			else
			{
				if (errno != EINTR)
					return (-1);
				else
				{
					*ptr = ioptr->LastBytePtr;
					return (0);
				}
			}
		}
	}
}




#ifndef SVR4

#include <sys/poll.h>

#define POLLERROR		(POLLHUP | POLLNVAL | POLLERR)
#define PFD(fds, i, x) { 	if (fds) 		if (ev & (x)) 			BITSET (fds, i); 		else 			BITCLEAR (fds, i); }
#define ERROR(x) { 	errno = x; 	return -1; }
/*
	simulate BSD select system call with SYSV poll system call
	note that efds parameter is not fully supported (or understood)
*/

extern long ulimit();

int
pollselect (nfds, rfds, wfds, efds, timeout)
int nfds;
unsigned long *rfds;
unsigned long *wfds;
unsigned long *efds;
struct timeval *timeout;
{
	int i, rc, ev, timevalue;
	struct pollfd pfds[NOFILES_MAX];
	static long _NOFILE = 0;

	PRMSG("in pollselect\n", 0,0);

	if (_NOFILE == 0) {
		_NOFILE = ulimit(4, (long)0);
		if (_NOFILE > NOFILES_MAX)
			_NOFILE = NOFILES_MAX;
	}

 	if (nfds > _NOFILE)
		nfds = _NOFILE;   /* make poll happy */

	for (i = 0; i < nfds; i++)
	{
		ev = 0;

		if (rfds && GETBIT (rfds, i)) ev |= POLLIN;
		if (wfds && GETBIT (wfds, i)) ev |= POLLOUT;
		if (ev || (efds && GETBIT (efds, i)))
			pfds[i].fd = i;
		else
			pfds[i].fd = -1;
		pfds[i].events = ev;
	}
	if (timeout)
		timevalue = timeout->tv_sec * 1000 + timeout->tv_usec / 1000;
	else
		timevalue = -1;

	while (1)	{
	   rc = poll (pfds, (unsigned long)nfds, timevalue);

	   if(rc<0 && errno == EAGAIN)
		continue;
	   else	break;
	}
	if(rc>0)	{
		if (!efds)
			for (i = 0; i < nfds; ++i)
			{
				ev = pfds[i].revents;
				if (ev & POLLERROR)
					ERROR (EBADF);
			}

		for (i = 0; i < nfds; ++i)
		{
			ev = pfds[i].revents;
			PFD (rfds, i, POLLIN);
			PFD (wfds, i, POLLOUT);
			PFD (efds, i, POLLERROR);
		}
	} 

	if(rc==0)	{
		i = (nfds+ 7)/8;
		if ( rfds != NULL)
			memset((char *) rfds, 0, i);
		if ( wfds != NULL)
			memset((char *) wfds, 0, i);
		if ( efds != NULL)
			memset((char *) efds, 0, i);
	
	}
	
	return rc;
}
#define SELECT	pollselect


#else
#define SELECT	select
#endif	/* ndef SVR4 */


/*	The following routine is used for USL Compatibility between
 *	Operating System versions of SVR4.0 and SVR3.2. In System
 *	V Release 3.2 the select call is not available, and the
 *	pollselect is used to poll for connections. In SVR4.0 the
 * 	system supplies select and it is used. However, the arguments
 *	to these routines are  not identical so this routine 
 * 	takes care of operating system differences by calling the
 * 	proper os dependent routine. It is called (by USL) from the
 *	server, Xlib  and Xt.
*/

int
_XSelect(nfds, r_mask, w_mask, e_mask, timeout)
  int			nfds;
  unsigned long		*r_mask, *w_mask, *e_mask;
  struct timeval	*timeout;
{
  int			retval = 0;
  int			count = 0;
  int			i;
  unsigned long		save_mask[MSKCNT];
  static struct timeval	notime;

/*PRMSG("IN XSelect from Xstreams nfds=%d r_mask=%0x\n",nfds ,r_mask);*/
  if (r_mask) {
    CLEARBITS(save_mask);

    for (i = 0; i < nfds; ++i) {
      if (GETBIT(r_mask, i)
      && (_XsInputBuffer[i].LastBytePtr - _XsInputBuffer[i].FirstBytePtr) > 0) {
        BITCLEAR(r_mask, i);
        BITSET(save_mask, i);
        ++count;
      }
    }
  }
  if (count) {
    if (_XANYSET(r_mask)
    || (w_mask && _XANYSET(w_mask))
    || (e_mask && _XANYSET(e_mask))) {
      retval = SELECT(nfds, r_mask, w_mask, e_mask, &notime);
    }

    for (i = 0; i < nfds; ++i) {
      if (GETBIT(save_mask, i)) {
        BITSET(r_mask, i);
      }
    }
    return retval < 0 ? retval : retval + count;
  } else {
    return SELECT(nfds, r_mask, w_mask, e_mask, timeout);

  }
} 	/* XSelect() */



#else /* not STREAMSCONN */
#ifndef lint
static int dummy;	/* prevent ranlibs from complaining */
#endif
#endif /* STREAMSCONN */

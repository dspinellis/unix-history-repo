/* IBM has disclaimed copyright on this module.  */

/***************************************************************/
/*                                                             */
/* Function: hftctl                                            */
/*                                                             */
/* Syntax:                                                     */
/*    #include <sys/ioctl.h>                                   */
/*    #include <sys/hft.h>                                     */
/*                                                             */
/*    int hftctl(fildes, request, arg )                        */
/*    int fildes, request;                                     */
/*    char *arg;                                               */
/*                                                             */
/* Description:                                                */
/*                                                             */
/*    Does the following:                                      */
/*      1. determines if fildes is pty                         */
/*         does normal ioctl it is not                         */
/*      2. places fildes into raw mode                         */
/*      3. converts ioctl arguments to datastream              */
/*      4. waits for 2 secs for acknowledgement before         */
/*         timimg out.                                         */
/*      5. places response in callers buffer ( just like       */
/*         ioctl.                                              */
/*      6. returns fildes to its original mode                 */
/*                                                             */
/*    User of this program should review steps 1,4, and 3.     */
/*    hftctl makes no check on the request type. It must be    */
/*    a HFT ioctl that is supported remotely.                  */
/*    This program will use the SIGALRM and alarm(2).  Any     */
/*    Previous alarms are lost.                                */
/*                                                             */
/*    Users of this program are free to modify it any way      */
/*    they want.                                               */
/*                                                             */
/* Return Value:                                               */
/*                                                             */
/*    If ioctl fails, a value of -1 is returned and errno      */
/*    is set to indicate the error.                            */
/*                                                             */
/***************************************************************/


#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <sys/devinfo.h>
#include <termios.h>
#include <termio.h>
#include <sys/hft.h>
#include <sys/tty.h>
/* #include <sys/pty.h> */
#define REMOTE 0x01

#undef ioctl
static char     SCCSid[] = "com/gnuemacs/src,3.1,9021-90/05/03-5/3/90";

/*************** LOCAL DEFINES **********************************/

typedef int    (*FUNC)();     /* pointer to a function        */

#define QDEV   ((HFQPDEVCH<<8)|HFQPDEVCL)
#define QLOC   ((HFQLOCCH<<8)|HFQLOCCL)
#define QPS    ((HFQPRESCH<<8)|HFQPRESCL)

#ifndef TCGETS 
#define TCGETS TCGETA
#endif
#ifndef TCSETS
#define TCSETS TCSETA
#endif

/*************** EXTERNAL / GLOBAL DATA AREA ********************/

       int              hfqry();
       int              hfskbd();
       char            *malloc();

extern int              errno;
static jmp_buf          hftenv;
static int              is_ack_vtd;
static FUNC             sav_alrm;
static struct hfctlreq  req =
			{ 0x1b,'[','x',0,0,0,21,HFCTLREQCH,HFCTLREQCL};
static struct hfctlack  ACK =
			{ 0x1b,'[','x',0,0,0,21,HFCTLACKCH,HFCTLACKCL};

       /* FUNC             signal(); */

/*************** LOCAL MACROS ***********************************/

#define HFTYPE(p)   ((p->hf_typehi<<8)|(p->hf_typelo))

#define BYTE4(p)    ((p)[0]<<24 | (p)[1]<<16 | (p)[2]<<8 | (p)[3])

					/* read a buffer        */
#define RD_BUF(f,p,l) \
        while ((l)) \
          if ((j = read((f),(p),(l))) < 0) \
             if (errno != EINTR) return (-1); \
             else continue; \
          else { (l) -= j; (p) += j; }

/*************** function prototypes ***************************/
#ifdef __STDC__
static GT_ACK (int fd, int req, char *buf);
static WR_REQ (int fd, int request, int cmdlen, char *cmd, int resplen);
static hft_alrm(int sig);
#else
static GT_ACK ();
static WR_REQ ();
static hft_alrm ();
#endif

/*************** HFTCTL FUNCTION *******************************/

hftctl (fd, request, arg)
     int     fd;
     int     request;
     union {
       struct hfintro *intro;
       struct hfquery *query;
       char           *c;
     } arg;
{

  int             i;
  int             fd_flag;	/* fcntl flags          */
  register union {
    struct hfintro         *cmd; /* p.cmd - intro des.   */
    struct hfqphdevc       *ph;	/* p.ph  - physical dev.*/
    char            *c;		/* p.c   - char ptr     */
  }               p;		/* general pointer      */
  int             pty_new;	/* pty modes            */
  int             pty_old;
  int             retcode;
  struct termios   term_new;	/* terminal attributes  */
  struct termios   term_old;
  struct devinfo	devInfo; /* defined in sys/devinfo.h */


  if (ioctl (fd, IOCINFO, &devInfo) == -1) return(-1);

  if (devInfo.devtype != DD_PSEU) /* is it a pty? */
    return (ioctl(fd, request, arg)); /* no, do IOCTL */

  /******* START PTY **************/
  /**  Pty found, possible HFT    */
  /** set new file des as raw     */
  /** as you can.                 */
  /********************************/

  /* Get current state of file    */
  /* descriptor & save            */
  if ((fd_flag = fcntl (fd, F_GETFL, 0)) == -1) return (-1);
  if (ioctl (fd, TCGETS, &term_old) == -1) return (-1);
  /* set terminal attr to raw     */
  /* and to delay on read         */
  pty_new = pty_old | REMOTE;
  memcpy (&term_new, &term_old, sizeof (term_new));
  term_new.c_iflag = 0;
  term_new.c_oflag = 0;
  term_new.c_lflag = 0;
  /* term_new.c_line  = 0; */
  for (i = 1; i <= 5; i++)
    term_new.c_cc[i] = 0;
  term_new.c_cc[0] = -1;
  ioctl (fd, TCSETS, &term_new);
  if (fcntl (fd, F_SETFL, fd_flag & ~O_NDELAY) == -1)
    return(-1);
  /* call spacific function       */
  if (request == HFSKBD)
    retcode = hfskbd (fd, request, arg.c);
  else				/* assume HFQUERY */
    retcode = hfqry (fd, request, arg.c);

  fcntl (fd, F_SETFL, fd_flag); /* reset terminal to original   */
  ioctl (fd, TCSETS, &term_old);


  return (retcode);             /* return error                 */
}

/*************** HFSKBD  FUNCTION ******************************/
static int
hfskbd (fd, request, arg)
        int     fd;
        int     request;
        struct hfbuf *arg;
{
  WR_REQ(fd, request, arg->hf_buflen, arg->hf_bufp,0);
  return (GT_ACK(fd, request, arg->hf_bufp));
}

/*************** HFQUERY FUNCTION ******************************/
static int
hfqry (fd, request, arg)
        int     fd;
        int     request;
        struct hfquery *arg;
{
  WR_REQ(fd, request, arg->hf_cmdlen, arg->hf_cmd, arg->hf_resplen);
  return (GT_ACK(fd, request, arg->hf_resp));
}


/*************** GT_ACK FUNCTION ******************************/
static int
GT_ACK (fd, req, buf)
        int     fd;
        int     req;
        char   *buf;
{
  struct hfctlack ack;
  int             i = sizeof (ack);
  int             j = 0;
  union {
    char            *c;
    struct hfctlack *ack;
  }               p;

  is_ack_vtd = 0;		/* flag no ACT VTD yet         */

  if (setjmp (hftenv))		/* set environment in case     */
    {				/* of time out                 */
      errno = ENODEV;		/* if time out, set errno      */
      return (-1);		/* flag error                  */
    }

  alarm(3);			/* time out in 3 secs          */
  sav_alrm = (FUNC) signal (SIGALRM, hft_alrm); /* prepare to catch time out */

  p.ack = &ack;
  while (! is_ack_vtd)		/* do until valid ACK VTD      */
    {
      RD_BUF(fd, p.c, i);	/* read until a ACK VTD is fill*/

      if (! memcmp (&ack, &ACK, sizeof (HFINTROSZ)) /* the ACK intro &  */
	  && (ack.hf_request == req)) /* is it the response we want ?*/
	{			/* yes, ACK VTD found          */
	  is_ack_vtd = 1;	/* quickly, flag it            */
	  break;		/* get the %$%#@ out of here   */
	}

      p.ack = &ack;		/* no, then skip 1st           */
      ++p.c;			/* char and start over         */
      i = sizeof (ack) - 1;	/* one less ESC to cry over    */

      while ((*p.c != 0x1b) && i) /* scan for next ESC           */
	{ ++p.c; --i; }		/* if any                      */

      (i ? memcpy (&ack, p.c, i) : 0); /* if any left over, then move */
      p.ack = &ack;		/* ESC to front of ack struct  */
      p.c += i;			/* skip over whats been read   */
      i = sizeof (ack) - i;	/* set whats left to be read   */
    }				/***** TRY AGAIN               */

  alarm(0);			/* ACK VTD received, reset alrm*/
  signal (SIGALRM, sav_alrm);	/* reset signal                */

  if (i = ack.hf_arg_len)	/* any data following ?        */
    {				/* yes,                        */
      RD_BUF(fd,buf,i);		/* read until it is received   */
    }

  if (errno = ack.hf_retcode)	/* set errno based on returned */
    return (-1);		/* code, if 0, then no error   */
  else
    return (0);			/* if set, then error returned */
}

/*************** HFT_ALRM FUNCTION ******************************/
static int
hft_alrm (sig)                    /* Function hft_alrm - handle  */
        int sig;		/* alarm signal               */
{
  signal (SIGALRM, sav_alrm);	/* reset to previous          */

  if (is_ack_vtd)		/* has ack vtd arrived ?      */
    return(0);			/* yes, then continue         */
  else				/* no, then return with error */
    longjmp (hftenv, -1);

}

/*********************************************************************/
/***                                                               ***/
/***  NOTE: Both the HFCTLREQ and the arg structure should be      ***/
/***        sent in one io write operation.  If terminal           ***/
/***        emulators are in NODELAY mode then multiple writes     ***/
/***        may cause bogus information to be read by the emulator ***/
/***        depending on the timing.                               ***/
/***                                                               ***/
/*********************************************************************/

static int
WR_REQ (fd, request, cmdlen, cmd, resplen)
	int             fd;
	int             request;
	int             cmdlen;
	char           *cmd;
	int             resplen;
{
  struct {
    char            *c;
    struct hfctlreq *req;
  }              p;
  int            size;

  req.hf_request = request;
  req.hf_arg_len = cmdlen;
  req.hf_rsp_len = resplen;

  if (cmdlen)			/* if arg structure to pass    */
    {
      size = sizeof (struct hfctlreq) + cmdlen;
      if ((p.c = malloc(size)) == NULL) /* malloc one area            */
	return (-1);

      memcpy (p.c, &req, sizeof (req)); /* copy CTL REQ struct         */
      memcpy (p.c + sizeof (req), cmd, cmdlen); /* copy arg struct     */
    }
  else
    {
      p.req = &req;		/* otherwise use only CTL REQ  */
      size = sizeof (req);
    }

  /* write request to terminal   */
  if (write(fd,p.c,size) == -1) return (-1);
  if (p.req != &req)		/* free if allocated           */
    free (p.c);
  return (0);

}

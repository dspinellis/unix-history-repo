#ifndef lint
static char sccsid[] = "@(#)uucpdefs.c	5.5 (Berkeley) 10/9/85";
#endif

#include "uucp.h"

char Progname[64];
int Ifn, Ofn;
char RRmtname[MAXFULLNAME];
char *Rmtname = RRmtname;
char User[128];
char Loginuser[16];
char Myname[MAXBASENAME+1];
char Wrkdir[WKDSIZE];

char *Spool = SPOOL;
char DLocal[64];
char DLocalX[64];
int Debug = 0;
time_t Retrytime;
short Usrf = 0;			/* Uustat global flag */
int IsTcpIp = 0;	/* 1 == TCP/IP connection, else 0.  kludge to suppress ioctl */
char MaxGrade = '\177';
char DefMaxGrade = '\177';
int nologinflag = 0;
char NOLOGIN[] = "/etc/nologin";

/* Save some data space */
char DEVNULL[] = "/dev/null";
char CANTOPEN[] = "CAN'T OPEN";
char _FAILED[] = "FAILED";

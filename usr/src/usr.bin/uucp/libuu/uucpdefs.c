#ifndef lint
static char sccsid[] = "@(#)uucpdefs.c	5.1 (Berkeley) %G%";
#endif

#include "uucp.h"

char Progname[10];
int Ifn, Ofn;
char Rmtname[16];
char User[16];
char Loginuser[16];
char Myname[16];
int Bspeed;
char Wrkdir[WKDSIZE];

char *Spool = SPOOL;
#ifdef	UUDIR
char DLocal[16];
char DLocalX[16];
#endif
int Debug = 0;
int Pkdebug = 0;
int Packflg = 0;
int Pkdrvon = 0;
long Retrytime;
int Unet = 0;	/* 1 == UNET connection, else 0.  kludge to suppress ioctl */

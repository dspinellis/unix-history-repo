/*	@(#)defs.h	1.7 83/07/28		*/

#include "ww.h"
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define NWINDOW 9

int nread;
int nreade;
int nreadz;
int nreadc;
struct timeval starttime;
struct timezone timezone;

char ibuf[512];
char *ibufp;
int ibufc;
#define bgetc()		(ibufc ? ibufc--, *ibufp++&0x7f : -1)
#define bpeekc()	(ibufc ? *ibufp&0x7f : -1)
#define bungetc(c)	(ibufp > ibuf ? ibufc++, *--ibufp = (c) : -1)

struct ww *selwin;
struct ww *cmdwin;

char quit;
char terse;
char escapec;

/*	@(#)defs.h	1.3 83/07/22		*/

#include "ww.h"
#include <signal.h>
#include <sys/time.h>
#include <sys/resource.h>

#define ESCAPE CTRL(p)

int nread;
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

	/* flags to doclose() */
#define CLOSE_ONE	0
#define CLOSE_ALL	1
#define CLOSE_DEAD	2

/*	onepass.h	4.2	87/12/10	*/

#ifndef _ONEPASS_
#define	_ONEPASS_
/*
 * Definitions for creating a one-pass
 * version of the compiler.
 */

#ifdef _PASS2_
#define crslab crs2lab
#define where where2
#define xdebug x2debug
#define tdebug t2debug
#ifndef deflab
#define deflab def2lab
#endif
#define edebug e2debug
#define eprint e2print
#define getlab get2lab
#define filename ftitle
#endif

/* NOPREF must be defined for use in first pass tree machine */
#define NOPREF	020000		/* no preference for register assignment */

#include "ndu.h"
#endif

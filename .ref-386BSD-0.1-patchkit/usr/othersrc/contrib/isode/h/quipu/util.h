/* util.h - various useful utility definitions */

/*
 * $Header: /f/osi/h/quipu/RCS/util.h,v 7.2 91/02/22 09:26:10 mrose Interim $
 *
 *
 * $Log:	util.h,v $
 * Revision 7.2  91/02/22  09:26:10  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:38:41  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:56:45  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef _DIDUTIL_

#define _DIDUTIL_

#ifndef _H_UTIL			/* PP interwork */

#include <errno.h>
#include <stdio.h>              /* minus the ctype stuff */
#include <ctype.h>
#include <setjmp.h>
#include "manifest.h"
#include "logger.h"

#endif

#ifndef       _GENERAL_
#include "general.h"
#endif

#include "quipu/config.h"
/* declarations that should have been in the system files */

#ifndef _H_UTIL			/* PP interwork */

extern char *strcat ();
extern char *strcpy ();
extern char *strncpy ();
extern char *strdup ();
extern char *multcat ();
extern char *multcpy ();
extern char *index ();
extern char *rindex ();
extern char *gets ();
extern long lseek ();

/* some common logical values */

#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif
#ifndef YES
#define YES     1
#endif
#ifndef NO
#define NO      0
#endif
#ifndef OK
#define OK      0
#endif
#ifndef DONE
#define DONE    1
#endif
#ifndef NOTOK
#define NOTOK   -1
#endif
#ifndef MAYBE
#define MAYBE   1
#endif

/* stdio extensions */

#ifndef lowtoup
#define lowtoup(chr) (islower(chr)?toupper(chr):chr)
#endif
#ifndef uptolow
#define uptolow(chr) (isupper(chr)?tolower(chr):chr)
#endif
#ifndef MIN
#define MIN(a,b) (( (b) < (a) ) ? (b) : (a) )
#endif
#ifndef MAX
#define MAX(a,b) (( (b) > (a) ) ? (b) : (a) )
#endif
#ifndef	MAXINT
#define MAXINT (~(1 << ((sizeof(int) * 8) - 1)))
#endif

#define isstr(ptr) ((ptr) != 0 && *(ptr) != '\0')
#define isnull(chr) ((chr) == '\0')
#define isnumber(c) ((c) >= '0' && (c) <= '9')

/*
 * provide a timeout facility
 */

extern  jmp_buf _timeobuf;

#define timeout(val)    (setjmp(_timeobuf) ? 1 : (_timeout(val), 0))

/*
 * some common extensions
 */
#define LINESIZE 1024    /* what we are prepared to consider a line length */
#define FILNSIZE 256    /* max filename length */
#define LOTS    1024    /* a max sort of thing */
#define MAXFILENAMELEN 15	/* size of largest fine name allowed */

# define        MAXFORK 10      /* no. of times to try a fork() */

#ifndef NULLCP
#define NULLCP ((char *)0)
#define NULLVP ((char **) 0)

extern char * malloc ();
extern char * smalloc ();

#define _H_UTIL
#endif
#endif 
#endif


/********************************************
version.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	version.c,v $
 * Revision 5.3  92/03/03  16:42:23  brennan
 * patch 1
 * 
 * Revision 5.2  92/01/22  05:34:10  brennan
 * version 1.1
 * 
 * Revision 5.1  91/12/05  07:56:33  brennan
 * 1.1 pre-release
 * 
*/

#include "mawk.h"
#include "patchlev.h"

#define  VERSION_STRING  \
  "mawk 1.1%s%s Feb 1992, Copyright (C) Michael D. Brennan\n\n"

#define  DOS_STRING     ""

/* If use different command line syntax for MSDOS
   mark that in VERSION  */

#if  MSDOS 
#undef   DOS_STRING

#if  SM_DOS

#if  HAVE_REARGV
#define  DOS_STRING     "SM"
#else
#define  DOS_STRING     "SMDOS"
#endif  

#else  /* LM_DOS  */

#if  HAVE_REARGV
#define  DOS_STRING     "LM"
#else
#define  DOS_STRING     "LMDOS"
#endif  

#endif
#endif  /* MSDOS */

#ifdef THINK_C
#undef DOS_STRING
#define DOS_STRING ":Mac"
#endif

/* print VERSION and exit */
void print_version()
{ static char fmt[] = "%-14s%10u\n" ;

  printf(VERSION_STRING, PATCH_STRING, DOS_STRING) ;
  fflush(stdout) ;
  fprintf(stderr, "compiled limits:\n") ;
  fprintf(stderr, fmt,  "largest field", MAX_FIELD) ;
  fprintf(stderr, fmt,  "sprintf buffer",SPRINTF_SZ) ;
  exit(0) ;
}

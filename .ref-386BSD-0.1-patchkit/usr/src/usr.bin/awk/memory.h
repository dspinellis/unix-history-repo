
/********************************************
memory.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	memory.h,v $
 * Revision 5.1  91/12/05  07:59:28  brennan
 * 1.1 pre-release
 * 
*/


/*  memory.h  */

#ifndef  MEMORY_H
#define  MEMORY_H

#include "zmalloc.h"

#define  new_CELL()  (CELL *) zmalloc(sizeof(CELL))
#define  free_CELL(p)  zfree(p,sizeof(CELL))

#ifndef  SUPPRESS_NEW_STRING_PROTO
STRING  *PROTO( new_STRING, (char *, ...) ) ;
#endif

#ifdef   DEBUG
void  PROTO( DB_free_STRING , (STRING *) ) ;

#define  free_STRING(s)  DB_free_STRING(s)

#else

#define  free_STRING(sval)   if ( -- (sval)->ref_cnt == 0 )\
                                zfree(sval, (sval)->len+STRING_OH) ; else
#endif


#endif   /* MEMORY_H */

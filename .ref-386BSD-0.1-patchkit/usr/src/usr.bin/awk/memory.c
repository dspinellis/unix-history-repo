
/********************************************
memory.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	memory.c,v $
 * Revision 5.1  91/12/05  07:56:21  brennan
 * 1.1 pre-release
 * 
*/


/* memory.c */

#include "mawk.h"

#if     HAVE_PROTOS
#define SUPPRESS_NEW_STRING_PROTO  /* get compiler off our back on
         the definition of new_STRING() */
#endif

#include "memory.h"

STRING null_str = {0, 1, "" } ;

  
STRING *new_STRING(s, xlen)   
  char *s ;  unsigned xlen ;
  /* WARNING: if s != NULL, don't access xlen
     because it won't be there   */
{ register STRING *sval ;
  unsigned len ;

  if ( s )
  {
    if ( *s == 0 ){ sval = &null_str ; null_str.ref_cnt++ ; }
    else
    {
      len = strlen(s) ;
      sval = (STRING *) zmalloc(len + STRING_OH) ;
      sval->len = len ;
      sval->ref_cnt = 1 ;
      (void) strcpy(sval->str, s) ;
    }
  }
  else  
  { sval = (STRING *) zmalloc( xlen + STRING_OH ) ;
    sval->ref_cnt = 1 ; sval->len = xlen ;
    /* zero out the end marker */
    sval->str[xlen] = 0 ; 
  }

  return sval ;
}


#ifdef   DEBUG

void  DB_free_STRING(sval)
  register STRING *sval ;
{ if ( -- sval->ref_cnt == 0 )  zfree(sval, sval->len+STRING_OH) ; }

#endif

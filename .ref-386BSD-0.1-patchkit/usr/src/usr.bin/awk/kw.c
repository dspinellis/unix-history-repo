
/********************************************
kw.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	kw.c,v $
 * Revision 5.1  91/12/05  07:56:12  brennan
 * 1.1 pre-release
 * 
*/


/* kw.c */


#include "mawk.h"
#include "symtype.h"
#include "parse.h"
#include "init.h"


static struct kw {
char *text ;
short kw ;
}  keywords[] = {

"print", PRINT,
"printf", PRINTF,
"do" , DO ,
"while" , WHILE ,
"for" , FOR ,
"break" , BREAK ,
"continue" , CONTINUE ,
"if" , IF ,
"else", ELSE ,
"in" , IN ,
"delete", DELETE ,
"split" , SPLIT ,
"match" , MATCH_FUNC ,
"BEGIN" , BEGIN,
"END" ,   END ,
"exit" , EXIT ,
"next" , NEXT ,
"return", RETURN,
"getline", GETLINE,
"sub" , SUB,
"gsub", GSUB,
"function", FUNCTION,
(char *) 0 , 0 } ;

/* put keywords in the symbol table */
void kw_init()
{ register struct kw *p = keywords ;
  register SYMTAB *q ;

  while ( p->text )
  { q = insert( p->text ) ;
    q->type = ST_KEYWORD ;
    q->stval.kw = p++ -> kw ;
  }
}

/* find a keyword to emit an error message */
char *find_kw_str( kw_token )
  int kw_token ;
{ struct kw *p ;

  for( p = keywords ; p->text ; p++ )
        if ( p->kw == kw_token )  return p->text ;
  /* search failed */
  return (char *) 0 ;
}


/********************************************
rexpdb.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/*$Log:	rexpdb.c,v $
 * Revision 3.2  91/08/13  09:10:09  brennan
 * VERSION .9994
 * 
 * Revision 3.1  91/06/07  10:33:30  brennan
 * VERSION 0.995
 * 
*/


#include "rexp.h"
#include <ctype.h>

/*  print a machine for debugging  */

static  char *xlat[] = {
"M_STR"  ,
"M_CLASS" ,
"M_ANY" ,
"M_START" ,
"M_END" ,
"M_U",
"M_1J" ,
"M_2JA" ,
"M_2JB" ,
"M_ACCEPT" } ;

void  REmprint(m, f)
  VOID *m ; FILE *f ;
{ register STATE *p = (STATE *) m ;
  char *end_on_string ;

  while ( 1 )
  { 
    if ( p->type >= END_ON ) 
    { p->type -= END_ON ; end_on_string = "$" ; }
    else end_on_string = "" ;

    if ( p->type < 0 || p->type >= END_ON )
    { fprintf(f, "unknown STATE type\n") ; return ; }

    fprintf(f, "%-10s" , xlat[p->type]) ;
    switch( p->type )
    {
     case M_STR : fprintf(f, "%s", p->data.str ) ;
                  break ;

     case M_1J:
     case M_2JA:  
     case M_2JB : fprintf(f, "%d", p->data.jump) ;
                 break ;
     case M_CLASS:
          { unsigned char *q = (unsigned char *) p->data.bvp ;
            unsigned char *r = q +  sizeof(BV) ;
            while ( q < r )  fprintf(f, "%x " , *q++) ;
          }
          break ;
    }
    fprintf(f, "%s\n" , end_on_string) ;
    if ( end_on_string[0] )  p->type += END_ON ;
    if ( p->type == M_ACCEPT )  return ;
    p++ ;
   }
}


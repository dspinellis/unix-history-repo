
/********************************************
split.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	split.c,v $
 * Revision 5.1  91/12/05  07:56:31  brennan
 * 1.1 pre-release
 * 
*/

/* split.c */

#define  TEMPBUFF_GOES_HERE

#include "mawk.h"
#include "symtype.h"
#include "bi_vars.h"
#include "bi_funct.h"
#include "memory.h"
#include "scan.h"
#include "regexp.h"
#include "field.h"

SPLIT_OV  *split_ov_list ;

static  int  PROTO(re_ov_split, (char *, PTR) ) ;
static  int  PROTO(space_ov_split, (char *, char *) ) ;

/* split string s of length slen on SPACE without changing s.
   load the pieces into STRINGS and ptrs into
   split_buff[]
   return the number of pieces */

int space_split( s , slen)  
  register char *s ;
  unsigned slen ;
{ char *back = s + slen ;
  int i = 0 ;
  int len ;
  char *q ;
  STRING  *sval ;
  int lcnt = MAX_SPLIT/3 ;

#define EAT_SPACE()   while ( scan_code[*(unsigned char*)s] ==\
                              SC_SPACE )  s++ 
#define EAT_NON_SPACE()   \
    *back = ' ' ; /* sentinel */\
    while ( scan_code[*(unsigned char*)s] != SC_SPACE )  s++ ;\
    *back = 0 


  while ( lcnt-- )
  { 
    EAT_SPACE() ;
    if ( *s == 0 )  goto done ;
    /* mark the front with q */
    q = s++ ;
    EAT_NON_SPACE() ;
    sval = split_buff[i++] = new_STRING((char *) 0, len = s - q ) ;
    (void) memcpy(sval->str, q, SIZE_T(len)) ;

    EAT_SPACE() ;
    if ( *s == 0 )  goto done ;
    q = s++ ;
    EAT_NON_SPACE() ;
    sval = split_buff[i++] = new_STRING((char *) 0, len = s - q ) ;
    (void) memcpy(sval->str, q, SIZE_T(len)) ;

    EAT_SPACE() ;
    if ( *s == 0 )  goto done ;
    q = s++ ;
    EAT_NON_SPACE() ;
    sval = split_buff[i++] = new_STRING((char *) 0, len = s - q ) ;
    (void) memcpy(sval->str, q, SIZE_T(len)) ;

  }
  /* we've overflowed */
  return  i + space_ov_split(s, back) ;

done:
  return i ;
}

static  int  space_ov_split(s, back)
  register char *s ;
  char *back ;

{
  SPLIT_OV  dummy ;
  register SPLIT_OV *tail = &dummy ;
  char *q ;
  int cnt = 0 ;
  unsigned len ;

  while ( 1 )
  {
    EAT_SPACE() ;
    if ( *s == 0 )  break ; /* done */
    q = s++ ;
    EAT_NON_SPACE() ;

    tail = tail->link = (SPLIT_OV*) zmalloc(sizeof(SPLIT_OV)) ;
    tail->sval = new_STRING((char *) 0 , len = s-q) ;
    (void) memcpy(tail->sval->str, q, SIZE_T(len)) ;
    cnt++ ;
  }

  tail->link = (SPLIT_OV*) 0 ;
  split_ov_list = dummy.link ;
  return cnt ;
}


char *re_pos_match(s, re, lenp)
  register char *s ; 
  PTR re ; unsigned *lenp ;
{
  while ( s = REmatch(s, re, lenp) )
        if ( *lenp )   return s ;
        else
        if ( *s == 0 )  break ;
        else s++ ;

  return (char *) 0 ;
}

int re_split(s, re)
  char *s ;
  PTR  re ;
{ register char *t ;
  int i = 0 ;
  unsigned mlen, len ;
  STRING *sval ;
  int lcnt = MAX_SPLIT / 3 ;

  while ( lcnt-- )
  {
    if ( !(t = re_pos_match(s, re, &mlen)) )  goto done ;
    sval = split_buff[i++] = new_STRING( (char *)0, len = t-s) ;
    (void) memcpy(sval->str, s, SIZE_T(len)) ;
    s = t + mlen ;

    if ( !(t = re_pos_match(s, re, &mlen)) )  goto done ;
    sval = split_buff[i++] = new_STRING( (char *)0, len = t-s) ;
    (void) memcpy(sval->str, s, SIZE_T(len)) ;
    s = t + mlen ;

    if ( !(t = re_pos_match(s, re, &mlen)) )  goto done ;
    sval = split_buff[i++] = new_STRING( (char *)0, len = t-s) ;
    (void) memcpy(sval->str, s, SIZE_T(len)) ;
    s = t + mlen ;
  }
  /* we've overflowed */
  return  i + re_ov_split(s, re) ;

done:
  split_buff[i++] = new_STRING(s) ;
  return i ;
}

/*
  we've overflowed split_buff[] , put
  the rest on the split_ov_list
  return number of pieces
*/

static int  re_ov_split(s, re)
  char *s ;
  PTR  re ;
{
  SPLIT_OV  dummy ;
  register SPLIT_OV  *tail = &dummy ;
  int cnt = 1 ;
  char *t ;
  unsigned len, mlen ;

  while ( t = re_pos_match(s, re, &mlen) )
  {
    tail = tail->link = (SPLIT_OV *) zmalloc(sizeof(SPLIT_OV)) ;
    tail->sval = new_STRING( (char *)0, len = t-s) ;
    (void) memcpy(tail->sval->str, s, SIZE_T(len)) ;
    s = t + mlen ;
    cnt++ ;
  }
  /* and one more */
  tail = tail->link = (SPLIT_OV *) zmalloc(sizeof(SPLIT_OV)) ;
  tail->sval = new_STRING(s) ;
  tail->link = (SPLIT_OV*) 0 ;
  split_ov_list = dummy.link ;

  return cnt ;
}
    
/*  split(s, X, r)
    split s into array X on r

    entry: sp[0] holds r
           sp[-1] pts at X
           sp[-2] holds s
*/
CELL *bi_split(sp)
  register CELL *sp ;
{ 
  int cnt ;   /* the number of pieces */


  if ( sp->type < C_RE )  cast_for_split(sp) ;
        /* can be C_RE, C_SPACE or C_SNULL */
  sp -= 2 ;
  if ( sp->type < C_STRING )  cast1_to_s(sp) ;

  if ( string(sp)->len == 0 ) /* nothing to split */
  { free_STRING( string(sp) ) ;
    sp->type = C_DOUBLE ; sp->dval = 0.0 ;
    return sp ;
  }

  switch ( (sp+2)->type )
  {
    case C_RE :
        cnt = re_split(string(sp)->str, (sp+2)->ptr) ;
        break ;

    case C_SPACE :
        cnt = space_split(string(sp)->str, string(sp)->len) ;
        break ;

    /* this case could be done by C_RE, but very slowly.
       Since it is the common way to eliminate fields,
       we'll treat the special case for speed */
    case C_SNULL : /* split on empty string */
        cnt = 1 ;
        split_buff[0] = (STRING *) sp->ptr ;
        string(sp)->ref_cnt++ ;
        break ;

    default : bozo("bad splitting cell in bi_split") ;
  }


  free_STRING( string(sp) ) ;
  sp->type = C_DOUBLE ;
  sp->dval = (double) cnt ;

  load_array((ARRAY)(sp+1)->ptr, cnt) ;

  return sp ;
}



/********************************************
re_cmpl.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	re_cmpl.c,v $
 * Revision 5.1  91/12/05  07:56:25  brennan
 * 1.1 pre-release
 * 
*/


/*  re_cmpl.c  */

#include "mawk.h"
#include "memory.h"
#include "scan.h"
#include "regexp.h"
#include "repl.h"


static  CELL *PROTO( REPL_compile, (STRING *) ) ;

typedef struct re_node {
STRING  *sval ;
PTR     re ;
struct re_node *link ;
}  RE_NODE ;

static RE_NODE *re_list ;  /* a list of compiled regular expressions */

static char efmt[] = "regular expression compile failed (%s)\n%s" ;

PTR re_compile( sval )
  STRING *sval ;
{ register RE_NODE *p ;
  RE_NODE *q ;
  char *s ;

  /* search list */
  s = sval->str ;
  p = re_list ;
  q = (RE_NODE *) 0 ;
  while ( p )
    if ( strcmp(s, p->sval->str) == 0 )  /* found */
        if ( !q ) /* already at front */  goto _return ;
        else /* delete from list for move to front */
        { q->link = p->link ; goto found ; }
    else
    { q = p ; p = p->link ; }

  /* not found */
  p = (RE_NODE *) zmalloc( sizeof(RE_NODE) ) ;
  p->sval = sval ;

  sval->ref_cnt++ ;
  if( !(p->re = REcompile(s)) )
        if ( mawk_state == EXECUTION )
            rt_error(efmt, REerrlist[REerrno] , s) ;
        else /* compiling */
        { compile_error(efmt, REerrlist[REerrno] , s) ;
          return (PTR) 0 ;
        }


found :
/* insert p at the front of the list */
  p->link = re_list ; re_list = p ;

_return :
  
#ifdef  DEBUG
  if ( dump_RE )  REmprint(p->re, stderr) ;
#endif
  return p->re ;
}



/* this is only used by da() */

#if  ! SM_DOS

char *re_uncompile( m )
  PTR  m ;
{ register RE_NODE *p ;

  for( p = re_list ; p ; p = p->link )
        if ( p->re == m )  return  p->sval->str ;
#ifdef  DEBUG
  bozo("non compiled machine") ;
#endif
}
#endif /* not SM_DOS */
  


/*=================================================*/
/*  replacement  operations   */

/* create a replacement CELL from a STRING *  */

static CELL *REPL_compile( sval )
  STRING  *sval ;
{ int i = 0 ;
  register char *p = sval->str ;
  register char *q ;
  char *xbuff ;
  CELL *cp ;

  q = xbuff = (char *) zmalloc( sval->len + 1 ) ;

  while ( 1 )
  {
      switch( *p )
      {
        case  0  :  *q = 0 ;
                    goto  done  ;

        case  '\\':
                if ( p[1] == '&' )
                { *q++ = '&' ; p += 2 ; continue ; }
                else  break ;

        case  '&':
                /* if empty we don't need to make a node */
                if ( q != xbuff )
                { *q = 0 ;
                  split_buff[i++] = new_STRING(xbuff) ;
                }
                /* and a null node for the '&'  */
                split_buff[i++] = (STRING *) 0  ;
                /*  reset  */
                p++ ;  q = xbuff ;
                continue ;

        default :
                break ;
      }

      *q++ = *p++ ;
  }

done :   
  /* if we have one empty string it will get made now */
  if ( q > xbuff || i == 0 )
          split_buff[i++] =  new_STRING(xbuff) ;

  /* This will never happen */
  if ( i > MAX_SPLIT )
      overflow("replacement pieces", MAX_SPLIT) ;

  cp = new_CELL() ;
  if ( i == 1 )
  {
    cp->type = C_REPL ;
    cp->ptr = (PTR) split_buff[0] ;
  }
  else
  {
    STRING **sp = (STRING**)
                  (cp->ptr = zmalloc(sizeof(STRING *)*i)) ;
    int j = 0 ;

    while ( j < i ) *sp++ = split_buff[j++] ;

    cp->type = C_REPLV ;
    cp->vcnt = i ;
  }
  zfree(xbuff, sval->len+1) ;
  return cp ;
}

/* free memory used by a replacement CELL  */

void  repl_destroy( cp )
  register CELL *cp ;
{ register STRING **p ;
  unsigned cnt ;

  if ( cp->type == C_REPL )  free_STRING(string(cp)) ;
  else  /* an C_REPLV  */
  {
    p = (STRING **) cp->ptr ;
    for( cnt = cp->vcnt ; cnt ; cnt--) 
    {
      if ( *p ) free_STRING( *p ) ;
      p++ ;
    }
    zfree( cp->ptr, cp->vcnt * sizeof(STRING *) ) ;
  }
}

/* copy a C_REPLV cell to another CELL */

CELL  *replv_cpy( target, source )
  CELL *target , *source ;
{ STRING **t, **s ;
  unsigned cnt ;

  target->type = C_REPLV ;
  target->vcnt = source->vcnt ;

  target->ptr = (PTR) zmalloc( target->vcnt * sizeof(STRING *) ) ;
  cnt = target->vcnt ;
  t = (STRING **) target->ptr ;
  s = (STRING **) source->ptr ;
  while ( cnt-- )
  { 
    if ( *t = *s++ )   (*t)->ref_cnt++ ;
    t++ ;
  }
  return target ;
}

/* here's our old friend linked linear list with move to the front
   for compilation of replacement CELLs  */

typedef  struct repl_node {
  struct repl_node  *link ;
  STRING  *sval  ;  /* the input */
  CELL    *cp ;  /* the output */
}  REPL_NODE  ;

static  REPL_NODE  *repl_list ;

/* search the list (with move to the front) for a compiled
   separator.
   return a ptr to a CELL (C_REPL or C_REPLV)
*/

CELL *repl_compile( sval )
  STRING *sval ;
{ register REPL_NODE *p ;
  REPL_NODE *q ;
  char *s ;

  /* search the list */
  s = sval->str ;
  p = repl_list ;
  q = (REPL_NODE *) 0 ;
  while ( p )
    if ( strcmp(s, p->sval->str) == 0 )  /* found */
        if ( !q ) /* already at front */  return p->cp ;
        else /* delete from list for move to front */
        { q->link = p->link ; goto found ; }
    else
    { q = p ; p = p->link ; }

  /* not found */
  p = (REPL_NODE *) zmalloc( sizeof(REPL_NODE) ) ;
  p->sval = sval ;
  sval->ref_cnt++ ;
  p->cp = REPL_compile(sval) ;

found :
/* insert p at the front of the list */
  p->link = repl_list ; repl_list = p ;
  return p->cp ;
}

/* return the string for a CELL or type REPL or REPLV,
   this is only used by da()  */

#if  ! SM_DOS

char *repl_uncompile( cp )
  CELL *cp ;
{
  register REPL_NODE *p = repl_list ;

  if ( cp->type == C_REPL )
    while ( p )
      if ( p->cp->type == C_REPL &&
           p->cp->ptr  == cp->ptr )   return p->sval->str ;
      else  p = p->link ;
  else
    while ( p )
      if ( p->cp->type == C_REPLV &&
           memcmp( cp->ptr, p->cp->ptr, SIZE_T(cp->vcnt * sizeof(STRING*))) 
           == 0  )   return  p->sval->str ;
      else  p = p->link ;

#if  DEBUG
  bozo("unable to uncompile an repl") ;
#endif
}
#endif /* ! SM_DOS */

/*
  convert a C_REPLV to  C_REPL
     replacing the &s with sval
*/

CELL  *replv_to_repl( cp, sval)
  CELL *cp ; STRING *sval ;
{ register STRING **p ;
  STRING **sblock = (STRING **) cp->ptr ;
  unsigned cnt , vcnt = cp->vcnt ;
  unsigned len ;
  char *target ;

#ifdef  DEBUG
  if ( cp->type != C_REPLV ) bozo("not replv") ;
#endif

  p = sblock ; cnt = vcnt ; len = 0 ;
  while ( cnt-- )
      if ( *p )  len += (*p++)->len ;
      else
      { *p++ = sval ; sval->ref_cnt++ ; len += sval->len ; }

  cp->type = C_REPL ;
  cp->ptr = (PTR) new_STRING((char *) 0, len) ;

  p = sblock ; cnt = vcnt ; target = string(cp)->str ;
  while ( cnt-- )
  { (void) memcpy(target, (*p)->str, SIZE_T((*p)->len)) ;
    target += (*p)->len ;
    free_STRING(*p) ;
    p++ ;
  }

  zfree( sblock, vcnt * sizeof(STRING *) ) ;
  return cp ;
}


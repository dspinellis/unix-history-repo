
/********************************************
bi_funct.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	bi_funct.c,v $
 * Revision 5.1  91/12/05  07:55:35  brennan
 * 1.1 pre-release
 * 
*/


#include "mawk.h"
#include "bi_funct.h"
#include "bi_vars.h"
#include "memory.h"
#include "init.h"
#include "files.h"
#include "fin.h"
#include "field.h"
#include "regexp.h"
#include "repl.h"
#include <math.h>


/* statics */
static STRING *PROTO(gsub, (PTR, CELL *, char *, int) ) ;
static void  PROTO( fplib_err, (char *, double, char *) ) ;


/* global for the disassembler */
BI_REC  bi_funct[] = { /* info to load builtins */

"index" , bi_index , 2, 2 ,
"substr" , bi_substr, 2, 3,
"length" , bi_length, 0, 1,
"sprintf" , bi_sprintf, 1, 255,
"sin", bi_sin , 1, 1 ,
"cos", bi_cos , 1, 1 ,
"atan2", bi_atan2, 2,2,
"exp", bi_exp, 1, 1,
"log", bi_log , 1, 1 ,
"int", bi_int, 1, 1,
"sqrt", bi_sqrt, 1, 1,
"rand" , bi_rand, 0, 0,
"srand", bi_srand, 0, 1,
"close", bi_close, 1, 1,
"system", bi_system, 1, 1,
"toupper", bi_toupper, 1, 1,
"tolower", bi_tolower, 1, 1,

(char *) 0, (PF_CP) 0, 0, 0 } ;



void bi_funct_init()
{ register BI_REC *p = bi_funct ;
  register SYMTAB *stp ;

  while ( p->name )
  { stp = insert( p->name ) ;
    stp->type = ST_BUILTIN ;
    stp->stval.bip = p++ ;
  }
  /* seed rand() off the clock */
  { CELL c ;

    c.type = 0 ; (void) bi_srand(&c) ;
  }

}

/**************************************************
 string builtins (except split (in split.c) and [g]sub (at end))
 **************************************************/

CELL *bi_length(sp)
  register  CELL *sp ;
{ unsigned len ;

  if ( sp->type == 0 )  cellcpy(sp, field) ;
  else sp-- ;

  if ( sp->type < C_STRING ) cast1_to_s(sp) ;
  len = string(sp)->len ;

  free_STRING( string(sp) ) ;
  sp->type = C_DOUBLE ;
  sp->dval = (double) len ;

  return sp ;
}

char *str_str(target, key , key_len)
  register char *target, *key ;
  unsigned key_len ;
{ 
  switch( key_len )
  { case 0 :  return (char *) 0 ;
    case 1 :  return strchr( target, *key) ;
    case 2 :
        while ( target = strchr(target, *key) )
          if ( target[1] == key[1] )  return  target ;
          else target++ ;
        /*failed*/
        return (char *) 0 ;
  }
  key_len-- ;
  while ( target = strchr(target, *key) )
        if ( memcmp(target+1, key+1, SIZE_T(key_len)) == 0 ) return target ;
        else target++ ;
  /*failed*/
  return (char *) 0 ;
}



CELL *bi_index(sp)
  register CELL *sp ;
{ register int idx ;
  unsigned len ;
  char *p ;

  sp-- ;
  if ( TEST2(sp) != TWO_STRINGS )
        cast2_to_s(sp) ;

  if ( len = string(sp+1)->len )
    idx = (p = str_str(string(sp)->str,string(sp+1)->str,len))
          ? p - string(sp)->str + 1 : 0 ;

  else  /* index of the empty string */
    idx = 1 ;
  
  free_STRING( string(sp) ) ;
  free_STRING( string(sp+1) ) ;
  sp->type = C_DOUBLE ;
  sp->dval = (double) idx ;
  return sp ;
}

/*  substr(s, i, n)
    if l = length(s)
    then get the characters
    from  max(1,i) to min(l,n-i-1) inclusive */

CELL *bi_substr(sp)
  CELL *sp ;
{ int n_args, len ;
  register int i, n ;
  STRING *sval ;  /* substr(sval->str, i, n) */

  n_args = sp->type ;
  sp -= n_args ;
  if ( sp->type != C_STRING )  cast1_to_s(sp) ;
      /* don't use < C_STRING shortcut */
  sval = string(sp) ;

  if ( (len = sval->len) == 0 )  /* substr on null string */
  {  if ( n_args == 3 )  cell_destroy(sp+2) ;
     cell_destroy(sp+1) ;
     return sp ;
  }

  if ( n_args == 2 )  
  { n = MAX__INT  ;  
    if ( sp[1].type != C_DOUBLE ) cast1_to_d(sp+1) ; 
  }
  else
  { if ( TEST2(sp+1) != TWO_DOUBLES ) cast2_to_d(sp+1) ;
    n = (int) sp[2].dval ;
  }
  i = (int) sp[1].dval - 1 ; /* i now indexes into string */

  if ( i < 0 ) { n += i ; i = 0 ; }
  if ( n > len - i )  n = len - i ;

  if ( n <= 0 )  /* the null string */
  { 
    sp->ptr = (PTR) &null_str ;
    null_str.ref_cnt++ ;
  }
  else  /* got something */
  { 
    sp->ptr = (PTR) new_STRING((char *)0, n) ;
    (void) memcpy(string(sp)->str, sval->str + i, SIZE_T(n)) ;
  }

  free_STRING(sval) ;
  return sp ;
} 

/*
  match(s,r)
  sp[0] holds r, sp[-1] holds s
*/

CELL *bi_match(sp)
  register CELL *sp ;
{ 
  char *p ;
  unsigned length ;

  if ( sp->type != C_RE )  cast_to_RE(sp) ;
  if ( (--sp)->type < C_STRING )  cast1_to_s(sp) ;

  cell_destroy(RSTART) ;
  cell_destroy(RLENGTH) ;
  RSTART->type = C_DOUBLE ;
  RLENGTH->type = C_DOUBLE ;

  p = REmatch(string(sp)->str, (sp+1)->ptr, &length) ;

  if ( p )
  { sp->dval = (double) ( p - string(sp)->str + 1 ) ;
    RLENGTH->dval = (double) length ;
  }
  else
  { sp->dval = 0.0 ;
    RLENGTH->dval = -1.0 ; /* posix */
  }

  free_STRING(string(sp)) ;
  sp->type = C_DOUBLE ;

  RSTART->dval = sp->dval ;

  return sp ;
}

CELL *bi_toupper(sp)
  CELL *sp ;
{ STRING *old ;
  register char *p, *q ;

  if ( sp->type != C_STRING )  cast1_to_s(sp) ;
  old = string(sp) ;
  sp->ptr = (PTR) new_STRING((char *) 0, old->len) ;

  q = string(sp)->str ; p = old->str ;

  while ( *p )
  {
    *q = *p++ ;
    if ( *q >= 'a' && *q <= 'z' )  *q += 'A' - 'a' ;
    q++ ;
  }
  free_STRING(old) ;
  return sp ;
}

CELL *bi_tolower(sp)
  CELL *sp ;
{ STRING *old ;
  register char *p, *q ;

  if ( sp->type != C_STRING )  cast1_to_s(sp) ;
  old = string(sp) ;
  sp->ptr = (PTR) new_STRING((char *) 0, old->len) ;

  q = string(sp)->str ; p = old->str ;

  while ( *p )
  {
    *q = *p++ ;
    if ( *q >= 'A' && *q <= 'Z' )  *q += 'a' - 'A' ;
    q++ ;
  }
  free_STRING(old) ;
  return sp ;
}


/************************************************
  arithemetic builtins
 ************************************************/

static void fplib_err( fname, val, error)
  char *fname ;
  double val ;
  char *error ;
{
  rt_error("%s(%g) : %s" , fname, val, error) ;
}


CELL *bi_sin(sp)
  register CELL *sp ;
{ 
#if ! STDC_MATHERR
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  sp->dval = sin( sp->dval ) ;
  return sp ;
#else
  double x ;

  errno = 0 ;
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  x = sp->dval ;
  sp->dval = sin( sp->dval ) ;
  if ( errno )  fplib_err("sin", x, "loss of precision") ;
  return sp ;
#endif
}

CELL *bi_cos(sp)
  register CELL *sp ;
{ 
#if ! STDC_MATHERR
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  sp->dval = cos( sp->dval ) ;
  return sp ;
#else
  double x ;

  errno = 0 ;
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  x = sp->dval ;
  sp->dval = cos( sp->dval ) ;
  if ( errno )  fplib_err("cos", x, "loss of precision") ;
  return sp ;
#endif
}

CELL *bi_atan2(sp)
  register CELL *sp ;
{ 
#if  !  STDC_MATHERR
  sp-- ;
  if ( TEST2(sp) != TWO_DOUBLES ) cast2_to_d(sp) ;
  sp->dval = atan2(sp->dval, (sp+1)->dval) ;
  return sp ;
#else

  errno = 0 ;
  sp-- ;
  if ( TEST2(sp) != TWO_DOUBLES ) cast2_to_d(sp) ;
  sp->dval = atan2(sp->dval, (sp+1)->dval) ;
  if ( errno ) rt_error("atan2(0,0) : domain error") ;
  return sp ;
#endif
}

CELL *bi_log(sp)
  register CELL *sp ;
{ 
#if ! STDC_MATHERR
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  sp->dval = log( sp->dval ) ;
  return sp ;
#else
  double  x ;

  errno = 0 ;
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  x = sp->dval ;
  sp->dval = log( sp->dval ) ;
  if ( errno )  fplib_err("log", x, "domain error") ;
  return sp ;
#endif
}

CELL *bi_exp(sp)
  register CELL *sp ;
{ 
#if  ! STDC_MATHERR
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  sp->dval = exp(sp->dval) ;
  return sp ;
#else
  double  x ;

  errno = 0 ;
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  x = sp->dval ;
  sp->dval = exp(sp->dval) ;
  if ( errno && sp->dval)  fplib_err("exp", x, "overflow") ;
     /* on underflow sp->dval==0, ignore */
  return sp ;
#endif
}

CELL *bi_int(sp)
  register CELL *sp ;
{ if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  sp->dval = sp->dval >= 0.0 ? floor( sp->dval ) : ceil(sp->dval)  ;
  return sp ;
}

CELL *bi_sqrt(sp)
  register CELL *sp ;
{ 
#if  ! STDC_MATHERR
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  sp->dval = sqrt( sp->dval ) ;
  return sp ;
#else
  double x ;

  errno = 0 ;
  if ( sp->type != C_DOUBLE )  cast1_to_d(sp) ;
  x = sp->dval ;
  sp->dval = sqrt( sp->dval ) ;
  if ( errno )  fplib_err("sqrt", x, "domain error") ;
  return sp ;
#endif
}

#ifdef  __TURBOC__
long  biostime(int, long) ;
#define  time(x)  biostime(0,0L)
#else
#ifdef THINK_C
#include <time.h>
#else
#include <sys/types.h>
#endif
#endif


/* For portability, we'll use our own random number generator , taken
   from:  Park, SK and Miller KW, "Random Number Generators:
   Good Ones are Hard to Find", CACM, 31, 1192-1201, 1988.
*/

static long seed ;  /* must be >=1 and <= 2^31-1 */
static CELL cseed ; /* argument of last call to srand() */

#define         M       0x7fffffff   /* 2^31-1 */

CELL *bi_srand(sp)
  register CELL *sp ;
{ CELL c ;

  if ( sp->type == 0 ) /* seed off clock */
  { (void) cellcpy(sp, &cseed) ;
    cell_destroy(&cseed) ;
    cseed.type = C_DOUBLE ;
    cseed.dval = (double) time((time_t*) 0) ;
  }
  else /* user seed */
  { sp-- ;
    /* swap cseed and *sp ; don't need to adjust ref_cnts */
    c = *sp ; *sp = cseed ; cseed = c ;
  }

  /* The old seed is now in *sp ; move the value in cseed to
     seed in range 1 to M */

  (void) cellcpy(&c, &cseed) ;
  if ( c.type == C_NOINIT )  cast1_to_d(&c) ;

  seed =  c.type == C_DOUBLE ? ((int)c.dval & M) % M + 1 :
                        hash(string(&c)->str) % M + 1 ;

  cell_destroy(&c) ;

  /* crank it once so close seeds don't give a close 
       first result  */
#define   A     16807
#define   Q     127773   /* M/A */
#define   R     2836     /* M%A */
  seed = A * (seed%Q) - R * (seed/Q) ;
  if ( seed <= 0 )  seed += M ;

  return sp ;
}
    
CELL *bi_rand(sp)
  register CELL *sp ;
{ 
  register long test ;

  test = A * (seed%Q) - R * (seed/Q) ;
  if ( test <= 0 )  test += M ;

  (++sp)->type = C_DOUBLE ;
  sp->dval = (double)( seed = test ) / (double) M ;
  return sp ;

#undef   A
#undef   M
#undef   Q
#undef   R
}

/*************************************************
 miscellaneous builtins
 close, system and getline
 *************************************************/

CELL *bi_close(sp)
  register CELL *sp ;
{ int x ;

  if ( sp->type < C_STRING ) cast1_to_s(sp) ;
  x = file_close( (STRING *) sp->ptr) ;
  free_STRING( string(sp) ) ;
  sp->type = C_DOUBLE ;
  sp->dval = (double) x ;
  return sp ;
}

#if   HAVE_REAL_PIPES

CELL *bi_system(sp)
  CELL *sp ;
{ int pid ;
  unsigned ret_val ;

  if ( sp->type < C_STRING ) cast1_to_s(sp) ;

  fflush(stdout) ; fflush(stderr) ;

  switch( pid = fork() )
  { case -1 :  /* fork failed */

       errmsg(errno, "could not create a new process") ;
       ret_val = 127 ;
       break ;

    case  0  :  /* the child */
       (void) execl(shell, shell, "-c", string(sp)->str, (char *) 0) ;
       /* if get here, execl() failed */
       errmsg(errno, "execute of %s failed", shell) ;
       fflush(stderr) ;
       _exit(127) ;

    default   :  /* wait for the child */
       ret_val = wait_for(pid) ;
       break ;
  }

  cell_destroy(sp) ;
  sp->type = C_DOUBLE ;
  sp->dval = (double) ret_val ;
  return sp ;
}

#endif /* HAVE_REAL_PIPES */

#ifdef  THINK_C

CELL *bi_system( sp )
  register CELL *sp ;
{ rt_error("no system call for the Macintosh Toy Operating System!!!") ;
  return sp ;
}

#endif


#if   MSDOS


CELL *bi_system( sp )
  register CELL *sp ;
{ int retval ;

  if ( sp->type < C_STRING ) cast1_to_s(sp) ;
  retval = DOSexec(string(sp)->str) ;
  free_STRING(string(sp)) ;
  sp->type = C_DOUBLE ;
  sp->dval = (double) retval ;
  return sp ;
}

#endif


/*  getline()  */

/*  if type == 0 :  stack is 0 , target address

    if type == F_IN : stack is F_IN, expr(filename), target address

    if type == PIPE_IN : stack is PIPE_IN, target address, expr(pipename)
*/

CELL *bi_getline(sp)
  register CELL *sp ;
{ 
  CELL tc , *cp ;
  char *p ;
  unsigned len ;
  FIN *fin_p ;


  switch( sp->type )
  { 
    case 0 :
        sp-- ;
        if ( ! main_fin )  open_main() ;
    
        if ( ! (p = FINgets(main_fin, &len)) )
                goto  eof ;

        cp = (CELL *) sp->ptr ;
        if ( TEST2(NR) != TWO_DOUBLES ) cast2_to_d(NR) ;
        NR->dval += 1.0 ;
        FNR->dval += 1.0 ;
        break ;

    case  F_IN :
        sp-- ;
        if ( sp->type < C_STRING ) cast1_to_s(sp) ;
        fin_p = (FIN *) file_find(sp->ptr, F_IN) ;
        free_STRING(string(sp) ) ;
        sp-- ;

        if ( ! fin_p )   goto open_failure ;
        if ( ! (p = FINgets(fin_p, &len)) )  
        {
          FINsemi_close(fin_p) ;
          goto eof ; 
        }
        cp = (CELL *) sp->ptr ;
        break ;

    case PIPE_IN :
        sp -= 2 ;
        if ( sp->type < C_STRING ) cast1_to_s(sp) ;
        fin_p = (FIN *) file_find(sp->ptr, PIPE_IN) ;
        free_STRING(string(sp)) ;

        if ( ! fin_p )   goto open_failure ;
        if ( ! (p = FINgets(fin_p, &len)) ) 
        { 
          FINsemi_close(fin_p) ;
#if  HAVE_REAL_PIPES
          /* reclaim process slot */
          (void) wait_for(0) ;
#endif
          goto eof ; 
        }
        cp = (CELL *) (sp+1)->ptr ;
        break ;

    default : bozo("type in bi_getline") ;

  }

  /* we've read a line , store it */

    if ( len == 0 )
    { tc.type = C_STRING ; 
      tc.ptr = (PTR) &null_str ; 
      null_str.ref_cnt++ ;
    }
    else
    { tc.type = C_MBSTRN ;
      tc.ptr = (PTR) new_STRING((char *) 0, len) ;
      (void) memcpy( string(&tc)->str, p, SIZE_T(len)) ;
    }

    slow_cell_assign(cp, &tc) ;

    cell_destroy(&tc) ;

  sp->dval = 1.0  ;  goto done ;

open_failure :
  sp->dval = -1.0  ; goto done ;

eof :
  sp->dval = 0.0  ;  /* fall thru to done  */

done :
  sp->type = C_DOUBLE  ;
  return sp ;
}

/**********************************************
 sub() and gsub()
 **********************************************/

/* entry:  sp[0] = address of CELL to sub on
           sp[-1] = substitution CELL
           sp[-2] = regular expression to match
*/

CELL *bi_sub( sp )
  register CELL *sp ;
{ CELL *cp ; /* pointer to the replacement target */
  CELL tc ;  /* build the new string here */
  CELL sc ;  /* copy of the target CELL */
  char *front, *middle, *back ; /* pieces */
  unsigned front_len, middle_len, back_len ;

  sp -= 2 ;
  if ( sp->type != C_RE )  cast_to_RE(sp) ;
  if ( sp[1].type != C_REPL && sp[1].type != C_REPLV )
              cast_to_REPL(sp+1) ;
  cp = (CELL *) (sp+2)->ptr ;
  /* make a copy of the target, because we won't change anything
     including type unless the match works */
  (void) cellcpy(&sc, cp) ;
  if ( sc.type < C_STRING ) cast1_to_s(&sc) ;
  front = string(&sc)->str ;

  if ( middle = REmatch(front, sp->ptr, &middle_len) )
  { 
    front_len = middle - front ;
    back = middle + middle_len ; 
    back_len = string(&sc)->len - front_len - middle_len ;

    if ( (sp+1)->type == C_REPLV ) 
    { STRING *sval = new_STRING((char *) 0, middle_len) ;

      (void) memcpy(sval->str, middle, SIZE_T(middle_len)) ;
      (void) replv_to_repl(sp+1, sval) ;
      free_STRING(sval) ;
    }

    tc.type = C_STRING ;
    tc.ptr = (PTR) new_STRING((char *) 0, 
             front_len + string(sp+1)->len + back_len ) ;

    { char *p = string(&tc)->str ;

      if ( front_len )
      { (void) memcpy(p, front, SIZE_T(front_len)) ;
        p += front_len ;
      }
      if ( string(sp+1)->len )
      { (void) memcpy(p, string(sp+1)->str, SIZE_T(string(sp+1)->len)) ;
        p += string(sp+1)->len ;
      }
      if ( back_len )  (void) memcpy(p, back, SIZE_T(back_len)) ;
    }

    slow_cell_assign(cp, &tc) ;

    free_STRING(string(&tc)) ;
  }

  free_STRING(string(&sc)) ;
  repl_destroy(sp+1) ;
  sp->type = C_DOUBLE ;
  sp->dval = middle != (char *) 0 ? 1.0 : 0.0 ;
  return sp ;
}

static  unsigned repl_cnt ;  /* number of global replacements */

/* recursive global subsitution 
   dealing with empty matches makes this mildly painful
*/

static STRING *gsub( re, repl, target, flag)
  PTR  re ;
  CELL *repl ;  /* always of type REPL or REPLV, 
       destroyed by caller */
  char *target ;
  int flag ; /* if on, match of empty string at front is OK */
{ char *front, *middle ;
  STRING *back ;
  unsigned front_len, middle_len ;
  STRING  *ret_val ;
  CELL xrepl ; /* a copy of repl so we can change repl */

  if ( ! (middle = REmatch(target, re, &middle_len)) )
      return  new_STRING(target) ; /* no match */

  (void) cellcpy(&xrepl, repl) ;

  if ( !flag && middle_len == 0 && middle == target ) 
  { /* match at front that's not allowed */

    if ( *target == 0 )  /* target is empty string */
    { repl_destroy(&xrepl) ;
      null_str.ref_cnt++ ;
      return & null_str ;
    }
    else
    { char xbuff[2] ;

      front_len = 0 ;
      /* make new repl with target[0] */
      repl_destroy(repl) ;
      xbuff[0] = *target++ ;  xbuff[1] = 0 ;
      repl->type = C_REPL ;
      repl->ptr = (PTR) new_STRING( xbuff ) ;
      back = gsub(re, &xrepl, target, 1) ;
    }
  }
  else  /* a match that counts */
  { repl_cnt++ ;

    front = target ;
    front_len = middle - target ;

    if ( *middle == 0 )  /* matched back of target */
    { back = &null_str ; null_str.ref_cnt++ ; }
    else back = gsub(re, &xrepl, middle + middle_len, 0) ;
      
    /* patch the &'s if needed */
    if ( repl->type == C_REPLV )
    { STRING *sval = new_STRING((char *) 0, middle_len) ;

      (void) memcpy(sval->str, middle, SIZE_T(middle_len)) ;
      (void) replv_to_repl(repl, sval) ;
      free_STRING(sval) ;
    }
  }

  /* put the three pieces together */
  ret_val = new_STRING((char *)0,
              front_len + string(repl)->len + back->len); 
  { char *p = ret_val->str ;

    if ( front_len )
    { (void) memcpy(p, front, SIZE_T(front_len)) ; p += front_len ; }
    if ( string(repl)->len )
    { (void) memcpy(p, string(repl)->str, SIZE_T(string(repl)->len)) ;
      p += string(repl)->len ;
    }
    if ( back->len ) (void) memcpy(p, back->str, SIZE_T(back->len)) ;
  }

  /* cleanup, repl is freed by the caller */
  repl_destroy(&xrepl) ;
  free_STRING(back) ;

  return ret_val ;
}

/* set up for call to gsub() */
CELL *bi_gsub( sp )
  register CELL *sp ;
{ CELL *cp ;  /* pts at the replacement target */
  CELL sc  ;  /* copy of replacement target */
  CELL tc  ;  /* build the result here */

  sp -= 2 ;
  if ( sp->type != C_RE ) cast_to_RE(sp) ;
  if ( (sp+1)->type != C_REPL && (sp+1)->type != C_REPLV )
          cast_to_REPL(sp+1) ;

  (void) cellcpy(&sc, cp = (CELL *)(sp+2)->ptr) ;
  if ( sc.type < C_STRING ) cast1_to_s(&sc) ;

  repl_cnt = 0 ;
  tc.ptr = (PTR) gsub(sp->ptr, sp+1, string(&sc)->str, 1) ;

  if ( repl_cnt )
  { 
    tc.type = C_STRING ;
    slow_cell_assign(cp, &tc) ;
  }

  /* cleanup */
  free_STRING(string(&sc)) ; free_STRING(string(&tc)) ;
  repl_destroy(sp+1) ;

  sp->type = C_DOUBLE ;
  sp->dval = (double) repl_cnt ;
  return sp ;
}

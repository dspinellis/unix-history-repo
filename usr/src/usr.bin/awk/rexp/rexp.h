
/********************************************
rexp.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	rexp.h,v $
 * Revision 3.6  92/01/21  17:31:45  brennan
 * moved ison() macro out of rexp[23].c
 * 
 * Revision 3.5  91/10/29  10:53:55  brennan
 * SIZE_T
 * 
 * Revision 3.4  91/08/13  09:10:02  brennan
 * VERSION .9994
 * 
 * Revision 3.3  91/06/15  09:40:25  brennan
 * gcc defines __STDC__ but might not have stdlib.h
 * 
 * Revision 3.2  91/06/10  16:18:19  brennan
 * changes for V7
 * 
 * Revision 3.1  91/06/07  10:33:18  brennan
 * VERSION 0.995
 * 
 * Revision 1.3  91/06/05  08:57:57  brennan
 * removed RE_xmalloc()
 * 
 * Revision 1.2  91/06/03  07:23:26  brennan
 * changed type of RE_error_trap
 * 
 * Revision 1.1  91/06/03  07:05:41  brennan
 * Initial revision
 * 
*/

#ifndef  REXP_H
#define  REXP_H

#ifdef THINK_C
#define MAWK		/* THINK C doesn't allow compile-time definitions */
#define SIZE_T(x) (size_t)(x)
#endif

#ifndef SIZE_T
#define SIZE_T(x) (x)
#endif

#include  <stdio.h>
#include  <setjmp.h>

char *strchr() ;

#ifndef   PROTO
#ifdef    __STDC__
#define  PROTO(name, args)   name  args
#else
#define  PROTO(name, args)   name()
#endif
#endif   

#ifdef  __STDC__
#define  VOID   void
#else
#define  VOID   char
#endif

VOID  *malloc(), *realloc() ;
void free() ;


VOID  *PROTO( RE_malloc, (unsigned) ) ;
VOID  *PROTO( RE_realloc, (void *,unsigned) ) ;


/*  finite machine  state types  */

#define  M_STR     	0
#define  M_CLASS   	1
#define  M_ANY     	2
#define  M_START   	3
#define  M_END     	4
#define  M_U       	5
#define  M_1J      	6
#define  M_2JA     	7
#define  M_2JB     	8
#define  M_ACCEPT  	9
#define  U_ON      	10

#define  U_OFF     0
#define  END_OFF   0
#define  END_ON    (2*U_ON)


typedef  unsigned char BV[32] ;  /* bit vector */

typedef  struct
{ char type ;
  unsigned char  len ;  /* used for M_STR  */
  union
   { 
     char *str  ;  /* string */
     BV   *bvp ;   /*  class  */
     int   jump ;
   }  data ;
}     STATE  ;

#define  STATESZ  (sizeof(STATE))

typedef  struct
{ STATE  *start, *stop ; }   MACHINE ;


/*  tokens   */
#define  T_OR   1       /* | */
#define  T_CAT  2       
#define  T_STAR 3       /* * */
#define  T_PLUS 4       /* + */
#define  T_Q    5       /* ? */
#define  T_LP   6       /* ( */
#define  T_RP   7       /* ) */
#define  T_START 8      /* ^ */
#define  T_END  9       /* $ */
#define  T_ANY  10      /* . */
#define  T_CLASS 11     /* starts with [ */
#define  T_SLASH 12     /*  \  */
#define  T_CHAR  13     /* all the rest */
#define  T_STR   14
#define  T_U     15

/*  precedences and error codes  */
#define  L   0
#define  EQ  1
#define  G   2
#define  E1  (-1)
#define  E2  (-2)
#define  E3  (-3)
#define  E4  (-4)
#define  E5  (-5)
#define  E6  (-6)
#define  E7  (-7)

#define  MEMORY_FAILURE      5

#define  ison(b,x)  ((b)[((unsigned char)(x))>>3] & (1<<((x)&7)))

/* struct for the run time stack */
typedef struct {
STATE *m ;   /*   save the machine ptr */
int    u ;   /*   save the u_flag */
char  *s ;   /*   save the active string ptr */
char  *ss ;  /*   save the match start -- only used by REmatch */
} RT_STATE ;   /* run time state */

/*  error  trap   */
extern int REerrno ;
void   PROTO(RE_error_trap, (int) ) ;


MACHINE   PROTO( RE_u, (void) ) ;
MACHINE   PROTO( RE_start, (void) ) ;
MACHINE   PROTO( RE_end, (void) ) ;
MACHINE   PROTO( RE_any, (void) ) ;
MACHINE   PROTO( RE_str, (char *, unsigned) ) ;
MACHINE   PROTO( RE_class, (BV *) ) ;
void      PROTO( RE_cat, (MACHINE *, MACHINE *) ) ;
void      PROTO( RE_or, (MACHINE *, MACHINE *) ) ;
void      PROTO( RE_close, (MACHINE *) ) ;
void      PROTO( RE_poscl, (MACHINE *) ) ;
void      PROTO( RE_01, (MACHINE *) ) ;
void      PROTO( RE_panic, (char *) ) ;
char     *PROTO( str_str, (char *, char *, unsigned) ) ;

void      PROTO( RE_lex_init , (char *) ) ;
int       PROTO( RE_lex , (MACHINE *) ) ;
void      PROTO( RE_run_stack_init, (void) ) ;
RT_STATE *PROTO( RE_new_run_stack, (void) ) ;

#endif   /* REXP_H  */

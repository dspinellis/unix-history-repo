
/********************************************
mawk.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/*   $Log: mawk.h,v $
 * Revision 1.2  1992/06/02  05:07:35  rich
 * Ported to 386bsd.  Changes from vax BSD4.3 include usage of
 * fmod in libm.a, usage of void pointers, and usage of vfprintf
 * in libc.a.  Floating point exceptions are not raised when
 * they should be, which causes the last fpe test to fail.
 *
 * Revision 5.4  92/03/03  16:34:41  brennan
 * conditional around open() proto
 * 
 * Revision 5.3  92/01/09  08:46:58  brennan
 * cell destroy macro
 * 
 * Revision 5.2  92/01/06  08:08:56  brennan
 * binmode() proto for MSDOS
 * 
 * Revision 5.1  91/12/05  07:59:26  brennan
 * 1.1 pre-release
 * 
*/


/*  mawk.h  */

#ifndef  MAWK_H
#define  MAWK_H   

#include  "config.h"

#ifdef   DEBUG
#define  YYDEBUG  1
extern  int   yydebug ;  /* print parse if on */
extern  int   dump_RE ;
#endif
extern  int   dump_code ;
extern  int   posix_space_flag ; 

#include <stdio.h>

#if  HAVE_STRING_H
#include <string.h>
#else
char *strchr() ;
char *strcpy() ;
char *strrchr() ;
#endif

#if  HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "types.h"


/*----------------
 *  GLOBAL VARIABLES
 *----------------*/

/* a well known string */
extern STRING  null_str ;

#ifndef TEMPBUFF_GOES_HERE
#define EXTERN	extern
#else
#define EXTERN   /* empty */
#endif

/* a useful scratch area */
EXTERN  union {
STRING  *_split_buff[MAX_SPLIT] ;
char    _string_buff[MIN_SPRINTF] ;
} tempbuff ;

/* anonymous union */
#define  string_buff	tempbuff._string_buff
#define  split_buff	tempbuff._split_buff

#define  SPRINTF_SZ	sizeof(tempbuff)

/* help with casts */
extern int pow2[] ;


 /* these are used by the parser, scanner and error messages
    from the compile  */

extern  char *pfile_name ; /* program input file */
extern  int current_token ;
extern  unsigned  token_lineno ; /* lineno of current token */
extern  unsigned  compile_error_count ;
extern  int  paren_cnt, brace_cnt ;
extern  int  print_flag, getline_flag ;
extern  short mawk_state ;
#define EXECUTION       1  /* other state is 0 compiling */

/*---------*/

#ifndef MSDOS_MSC
extern  int  errno ;     
#endif
extern  char *progname ; /* for error messages */

/* macro to test the type of two adjacent cells */
#define TEST2(cp)  (pow2[(cp)->type]+pow2[((cp)+1)->type])

/* macro to get at the string part of a CELL */
#define string(cp) ((STRING *)(cp)->ptr)

#ifdef   DEBUG
#define cell_destroy(cp)  DB_cell_destroy(cp)
#else

#define cell_destroy(cp)   if ( (cp)->type >= C_STRING &&\
                           -- string(cp)->ref_cnt == 0 )\
                        zfree(string(cp),string(cp)->len+STRING_OH);else
#endif

/*  prototypes  */

void  PROTO( cast1_to_s, (CELL *) ) ;
void  PROTO( cast1_to_d, (CELL *) ) ;
void  PROTO( cast2_to_s, (CELL *) ) ;
void  PROTO( cast2_to_d, (CELL *) ) ;
void  PROTO( cast_to_RE, (CELL *) ) ;
void  PROTO( cast_for_split, (CELL *) ) ;
void  PROTO( check_strnum, (CELL *) ) ;
void  PROTO( cast_to_REPL, (CELL *) ) ;

int   PROTO( test, (CELL *) ) ; /* test for null non-null */
CELL *PROTO( cellcpy, (CELL *, CELL *) ) ;
CELL *PROTO( repl_cpy, (CELL *, CELL *) ) ;
void  PROTO( DB_cell_destroy, (CELL *) ) ;
void  PROTO( overflow, (char *, unsigned) ) ;
void  PROTO( rt_overflow, (char *, unsigned) ) ;
void  PROTO( rt_error, ( char *, ...) ) ;
void  PROTO( mawk_exit, (int) ) ;
void PROTO( da, (INST *, FILE *)) ;
char *PROTO( str_str, (char*, char*, unsigned) ) ;
char *PROTO( rm_escape, (char *) ) ;
int   PROTO( re_split, (char *, PTR) ) ;
char *PROTO( re_pos_match, (char *, PTR, unsigned *) ) ;
int   PROTO( binmode, (void)) ;

void  PROTO( exit, (int) ) ;

#ifdef THINK_C
#include <unix.h>
#else
int   PROTO( close, (int) ) ;

/* ANSI compilers won't like open() if they've ever seen open as
   int open(char *,int, ...).  If so remove it.
*/

#ifndef _IBMR2 /* AIX */
#ifndef __386BSD__
int   PROTO( open, (char *,int, int) ) ;
#endif
#endif

int   PROTO( read, (int , PTR, unsigned) ) ;
#endif

int  PROTO ( parse, (void) ) ;
int  PROTO ( yylex, (void) ) ;
int  PROTO( yyparse, (void) ) ;
void PROTO( yyerror, (char *) ) ;

void PROTO( bozo, (char *) ) ;
void PROTO( errmsg , (int, char*, ...) ) ;
void PROTO( compile_error, ( char *, ...) ) ;

void  PROTO( execute, (INST *, CELL *, CELL *) ) ;
char *PROTO( find_kw_str, (int) ) ;

#if ! HAVE_STDLIB_H
double strtod() ;
#endif

double fmod() ;

#endif  /* MAWK_H */

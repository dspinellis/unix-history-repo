
/********************************************
code.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	code.c,v $
 * Revision 5.1  91/12/05  07:55:43  brennan
 * 1.1 pre-release
 * 
*/

/*  code.c  */

#include "mawk.h"
#include "code.h"
#include "init.h"
#include "jmp.h"
#include "field.h"


#define   MAIN_CODE_SZ  (MAIN_PAGE_SZ*sizeof(INST))

INST *code_ptr  ;
INST *main_start , *main_code_ptr ;

#if 0
INST *begin_start , *begin_code_ptr ;
INST *end_start , *end_code_ptr ;
#endif 
unsigned main_size /*, begin_size, end_size*/ ;
    /* when the code is done executing its freed,
       that's why this is global */

struct be_code begin_code , end_code ;

void  PROTO(fdump, (void) ) ;

void  be_expand( p )
  struct be_code *p ;
{ int delta ;

  main_code_ptr = code_ptr ;

  if ( p->start )
  {
    delta = p->ptr - p->start ;
    p->start = (INST*) zrealloc(p->start, INST_BYTES(p->size),
			INST_BYTES(PAGE_SZ)) ;
  }
  else
  {
    delta = 0 ;
    p->start = (INST*) zmalloc(INST_BYTES(PAGE_SZ)) ;
  }

  p->size = PAGE_SZ ;
  code_ptr = p->start + delta ;
}

void  be_shrink(p)
  struct be_code *p ;
{
  int delta = code_ptr - p->start ;

  code_ptr = main_code_ptr ;

  if ( delta > p->size )
	overflow( p == & begin_code ?
		  "BEGIN code" : "END code" , p->size) ;

  p->start = (INST*) zrealloc(p->start, INST_BYTES(p->size) ,
			INST_BYTES(delta+2)) ;

  p->ptr = p->start + delta ;
  p->size = delta + 2 ;
}
    

void  code_init()
{ 
  code_ptr = main_code_ptr = main_start 
    = (INST *) zmalloc(MAIN_CODE_SZ) ;

  code1(_OMAIN) ; 
}

void code_cleanup()
{ int some_code_flag = 0 ; /* might only have functions */ 


  /* set the END code */
  if ( end_code.start )
  { 
    end_code.ptr++ -> op =  _EXIT0 ;
    end_code.ptr++ -> op =  _HALT  ;
    end_code.size = INST_BYTES(end_code.size) ;
  }

  /* set the main code */
  if ( end_code.start || code_ptr - main_start > 1 )
  { int gl_offset = code_ptr - main_start ;
    extern INST *next_label ;
    extern int NR_flag ;

    if ( NR_flag )  code1(OL_GL_NR) ;
    else code1(OL_GL) ;

    code1(_HALT) ;

    main_size = code_ptr - main_start ;
    if ( main_size > MAIN_PAGE_SZ )
           overflow("MAIN code" , MAIN_PAGE_SZ) ;
    main_size *= sizeof(INST) ;
    code_ptr = main_start =
               (INST*) zrealloc(main_start, MAIN_CODE_SZ, main_size) ;

    next_label = main_start+gl_offset ;
    some_code_flag = 1 ;
  }
  else  /* only BEGIN */
  {
    zfree(main_start, MAIN_CODE_SZ) ;
    main_start = (INST*) 0 ;
  }

  /* set the BEGIN code */
  if ( begin_code.start )
  { 
    some_code_flag = 1 ;
    begin_code.ptr++ -> op = main_start ? _JMAIN : _EXIT0 ;
    begin_code.ptr++ -> op = _HALT ;
    begin_code.size = INST_BYTES(begin_code.size) ;

    /* execution starts at code_ptr */
    code_ptr = begin_code.start ;
  }

#if ! SM_DOS
  if ( dump_code )
  {
    fdump() ; /* dumps all user functions */
    if ( begin_code.start )
    { fprintf(stderr, "BEGIN\n") ; da(begin_code.start, stderr) ; }
    if ( end_code.start )
    { fprintf(stderr, "END\n") ; da(end_code.start, stderr) ; }
    if ( main_start )
    { fprintf(stderr, "MAIN\n") ; da(main_start, stderr) ; }
  }
#endif

  if ( some_code_flag == 0 ) mawk_exit(0) ;
}



/********************************************
main.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	main.c,v $
 * Revision 5.1  91/12/05  07:56:14  brennan
 * 1.1 pre-release
 * 
*/



/*  main.c  */

#include "mawk.h"
#include "code.h"
#include "init.h"
#include "fin.h"
#include "bi_vars.h"
#include "field.h"
#include "files.h"
#include <stdio.h>

#if  MSDOS 
void  reargv(int *, char ***) ;
#endif

#if LM_DOS && __TURBOC__
extern unsigned  _stklen = 16 * 1024U ;
   /*  4K of stack is enough for a user function call 
       nesting depth of 75 so this is enough for 300 */
#endif



extern int program_fd ;
char *progname ;
short mawk_state ; /* 0 is compiling */
int  exit_code ;


main(argc , argv )
  int argc ; char **argv ;
{ 

#if   MSDOS
  progname = "mawk" ;
#if      HAVE_REARGV
  reargv(&argc, &argv) ;
#endif
#else	/* MSDOS */
#ifdef THINK_C
  progname = "MacMAWK";
#else	/* THINK_C */
  { char *strrchr() ;
    char *p = strrchr(argv[0], '/') ;
    progname = p ? p+1 : argv[0] ; }
#endif
#endif

  initialize(argc, argv) ;

  if ( parse() || compile_error_count )  mawk_exit(1) ;

  compile_cleanup() ;
  mawk_state = EXECUTION ;
  execute(code_ptr, eval_stack-1, 0) ;
  /* never returns */
  return 0 ;
}

void  mawk_exit(x)
  int x ;
{
#if  HAVE_REAL_PIPES
  close_out_pipes() ;  /* no effect, if no out pipes */
#else
#if  HAVE_FAKE_PIPES
  close_fake_pipes() ;
#endif
#endif

  exit(x) ;
}

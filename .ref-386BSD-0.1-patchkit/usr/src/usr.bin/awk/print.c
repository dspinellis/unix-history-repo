
/********************************************
print.c
copyright 1992, 1991.  Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	print.c,v $
 * Revision 5.2  92/02/24  10:52:16  brennan
 * printf and sprintf() can now have more args than % conversions
 * removed HAVE_PRINTF_HD -- it was too obscure
 * 
 * Revision 5.1  91/12/05  07:56:22  brennan
 * 1.1 pre-release
 * 
*/

#include "mawk.h"
#include "bi_vars.h"
#include "bi_funct.h"
#include "memory.h"
#include "field.h"
#include "scan.h"
#include "files.h"

static void  PROTO( print_cell, (CELL *, FILE *) ) ;
static STRING* PROTO( do_printf, (FILE *, char *, unsigned, CELL *) ) ;
static void  PROTO( bad_conversion, (int, char *, char *)) ;



/* this can be moved and enlarged  by -W sprintf=num  */
char *sprintf_buff = string_buff ;
char *sprintf_limit = string_buff + SPRINTF_SZ ;

static void print_cell(p, fp)
  register CELL *p ;
  register FILE *fp ;
{ register int len ;
  
  switch( p->type )
  {
    case C_NOINIT : break ;
    case C_MBSTRN :
    case C_STRING :
    case C_STRNUM :
        switch( len = string(p)->len )
        {
          case 0 :  break ;
          case 1 :
                    putc(string(p)->str[0],fp) ;
                    break ;

          default :
                    fwrite(string(p)->str, SIZE_T(1), SIZE_T(len), fp) ;
        }
        break ;

    case C_DOUBLE :
	if ( (double)(len = (int) p->dval) == p->dval )
	    fprintf(fp, "%d", len) ;
	else
        fprintf(fp, string(OFMT)->str, p->dval) ;
        break ;

    default :
        bozo("bad cell passed to print_cell") ;
  }
}

/* on entry to bi_print or bi_printf the stack is:

   sp[0] = an integer k
       if ( k < 0 )  output is to a file with name in sp[-1]
       { so open file and sp -= 2 }

   sp[0] = k >= 0 is the number of print args
   sp[-k]   holds the first argument 
*/

CELL *bi_print(sp)
  CELL *sp ; /* stack ptr passed in */
{ register CELL *p ;
  register int k ;
  FILE *fp ;

  if ( (k = sp->type) < 0 )
  { if ( (--sp)->type < C_STRING ) cast1_to_s(sp) ;
    fp = (FILE *) file_find( string(sp), k ) ;
    free_STRING(string(sp)) ;
    k = (--sp)->type ;
  }
  else  fp = stdout ;

  if ( k )  
  { p = sp - k ; /* clear k variables off the stack */
    sp = p - 1 ;
    while ( k-- > 1 ) 
    { print_cell(p,fp) ; print_cell(OFS,fp) ;
      cell_destroy(p) ; p++ ; }
    
    print_cell(p, fp) ;  cell_destroy(p) ;
  }
  else  
  { sp-- ;
    print_cell( &field[0], fp )  ; }

  print_cell(ORS , fp) ;
  return sp ;
}
  
/*---------- types and defs for doing printf and sprintf----*/
#define  PF_C		0
#define  PF_S		1
#define  PF_D		2   /* int conversion */
#define  PF_LD		3   /* long int */
#define  PF_F		4   /* float conversion */

/* for switch on number of '*' and type */
#define  AST(num,type)  (5*(num)+(type))

/* some picky ANSI compilers go berserk without this */
#if HAVE_PROTOS
typedef int (*PRINTER)(PTR,char *,...) ;
#else
typedef int (*PRINTER)() ;
#endif

/*-------------------------------------------------------*/

static void bad_conversion(cnt, who, format)
  int cnt ; 
  char *who , *format ;
{
  rt_error( "improper conversion(number %d) in %s(\"%s\")", 
	     cnt, who, format ) ;
}

/* the contents of format are preserved,
   caller does CELL cleanup 

   This routine does both printf and sprintf (if fp==0)
*/
static STRING *do_printf( fp, format, argcnt, cp)
  FILE *fp ;
  char *format ; 
  CELL *cp ;  /* ptr to an array of arguments ( on the eval stack) */
  unsigned argcnt ;  /* number of args on eval stack */
{ 
  char  save ;
  char *p ;
  register char *q = format ;
  register char *target ;
  int l_flag , h_flag ;  /* seen %ld or %hd  */
  int ast_cnt ;
  int ast[2] ;
  long lval ;
  int ival ;  /* caters to MSDOS */
  int num_conversion = 0 ; /* for error messages */
  char *who ; /*ditto*/
  int pf_type ;  /* conversion type */
  PRINTER printer ; /* pts at fprintf() or sprintf() */

  if ( fp == (FILE *) 0 ) /* doing sprintf */
  {
    target = sprintf_buff ;
    printer = (PRINTER) sprintf ;
    who = "sprintf" ;
  }
  else /* doing printf */
  {
    target = (char *) fp ; /* will never change */
    printer = (PRINTER) fprintf ;
    who = "printf" ;
  }

  while ( 1 )
  { 
    if ( fp )  /* printf */
    {
      while ( *q != '%' )
	if ( *q == 0 )  return (STRING *) 0 ;
	else
	{ putc(*q,fp) ; q++ ; }
    }
    else  /* sprintf */
    {
      while ( *q != '%' )
	if ( *q == 0 )
	{
		if ( target > sprintf_limit ) /* damaged */
		{
		  /* hope this works */
		  rt_overflow("sprintf buffer",
			  sprintf_limit - sprintf_buff) ;
		}
		else  /* really done */
		{
		  STRING *retval ;
		  int len = target - sprintf_buff ;

		  retval = new_STRING((char*)0, len) ;
		  (void)memcpy(retval->str, sprintf_buff, SIZE_T(len)) ;
		  return retval ;
		}
	}
	else  *target++ = *q++ ;
    }
       

    num_conversion++ ;
  
    if ( * ++q == '%' )   /* %% */
    {
	if ( fp )   putc(*q, fp) ; 
	else *target++ = *q ;

	q++ ; continue ;
    }

    /* mark the '%' with p */
    p = q-1 ;

    /* eat the flags */
    while ( *q == '-' || *q == '+' || *q == ' ' ||
            *q == '#' || *q == '0' )  q++ ;

    ast_cnt = 0 ;
    if ( *q == '*' )
    { 
      if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
      ast[ast_cnt++] = (int) cp++ ->dval ;
      argcnt-- ; q++ ;
    }
    else
    while ( scan_code[*(unsigned char *)q] == SC_DIGIT )  q++ ;
    /* width is done */

    if ( *q == '.' )  /* have precision */
    { q++ ;
      if ( *q == '*' )
      {
	if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
        ast[ast_cnt++] = (int) cp++ ->dval ;
        argcnt-- ; q++ ;
      }
      else
      while ( scan_code[*(unsigned char*)q] == SC_DIGIT ) q++ ; 
    }

    if ( argcnt <= 0 )  
        rt_error("not enough arguments passed to %s(\"%s\")",
		  who, format) ;

    l_flag = h_flag = 0 ;

    if ( *q == 'l' ) { q++ ; l_flag = 1 ; }
    else
    if ( *q == 'h' ) { q++ ; h_flag = 1 ; }

    switch( *q++ )
    {
      case 's' :
            if ( l_flag + h_flag ) 
		bad_conversion(num_conversion,who,format) ;
            if ( cp->type < C_STRING ) cast1_to_s(cp) ;
            pf_type = PF_S ;
            break ;

      case 'c' :
            if ( l_flag + h_flag )
		bad_conversion(num_conversion,who,format) ;

	    switch( cp->type )
	    {
	      case C_NOINIT :
		    ival = 0 ;
		    break ;

	      case C_STRNUM :
	      case C_DOUBLE :
		    ival = (int) cp->dval ;
		    break ;

	      case  C_STRING :
		    ival = string(cp)->str[0] ;
		    break ;

	      case  C_MBSTRN :
		    check_strnum(cp) ;
		    ival = cp->type == C_STRING ?
			string(cp)->str[0] : (int) cp->dval ;
		    break ;
	      
	      default :
		    bozo("printf %c") ;
	    }

            pf_type = PF_C ;
	    break ;

      case 'd' :
      case 'o' :
      case 'x' :
      case 'X' :
      case 'i' :
      case 'u' :
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            lval = (long) cp->dval ;
	    if ( h_flag ) lval &= 0xffff ;

            pf_type = l_flag ? PF_LD : PF_D ;
            break ;
    
      case 'e' :
      case 'g' :
      case 'f' :
      case 'E' :
      case 'G' :
            if ( h_flag + l_flag )
		bad_conversion(num_conversion,who,format) ;
            if ( cp->type != C_DOUBLE ) cast1_to_d(cp) ;
            pf_type = PF_F ;
            break ;

      default : bad_conversion(num_conversion,who,format) ;
    }

    save = *q ;
    *q = 0 ;

    /* ready to call printf() */
    switch( AST(ast_cnt, pf_type ) )
    {
      case AST(0, PF_C )  :
            (*printer)((PTR) target, p, ival) ;
            break ;

      case AST(1, PF_C ) :
            (*printer)((PTR) target, p, ast[0], ival) ;
            break ;

      case AST(2, PF_C ) :
            (*printer)((PTR) target, p, ast[0], ast[1], ival) ;
            break ;

      case AST(0, PF_S) :
            (*printer)((PTR) target, p, string(cp)->str) ;
            break ;

      case AST(1, PF_S) :
            (*printer)((PTR) target, p, ast[0],string(cp)->str) ;
            break ;

      case AST(2, PF_S) :
            (*printer)((PTR) target, p, ast[0], ast[1], string(cp)->str) ;
            break ;

      case AST(0, PF_D) :
            (*printer)((PTR) target, p, (int) lval) ;
            break ;

      case AST(1, PF_D) :
            (*printer)((PTR) target, p, ast[0], (int) lval) ;
            break ;

      case AST(2, PF_D) :
            (*printer)((PTR) target, p, ast[0], ast[1], (int) lval) ;
            break ;

      case AST(0, PF_LD) :
            (*printer)((PTR) target, p,  lval) ;
            break ;

      case AST(1, PF_LD) :
            (*printer)((PTR) target, p, ast[0],  lval) ;
            break ;

      case AST(2, PF_LD) :
            (*printer)((PTR) target, p, ast[0], ast[1],  lval) ;
            break ;

      case AST(0, PF_F) :
            (*printer)((PTR) target, p,  cp->dval) ;
            break ;

      case AST(1, PF_F) :
            (*printer)((PTR) target, p, ast[0],  cp->dval) ;
            break ;

      case AST(2, PF_F) :
            (*printer)((PTR) target, p, ast[0], ast[1],  cp->dval) ;
            break ;
    }
    if ( fp == (FILE *) 0 ) while ( *target ) target++ ;
    *q = save ; argcnt-- ; cp++ ;
  }
}

CELL *bi_printf(sp)
  register CELL *sp ;
{ register int k ;
  register CELL *p ;
  FILE *fp ;

  if ( (k = sp->type) < 0 )
  { if ( (--sp)->type < C_STRING ) cast1_to_s(sp) ;
    fp = (FILE *) file_find( string(sp), k ) ;
    free_STRING(string(sp)) ;
    k = (--sp)->type ;
  }
  else  fp = stdout ;

  sp -= k-- ; /* sp points at the format string */
  if ( sp->type < C_STRING )  cast1_to_s(sp) ;
  do_printf(fp, string(sp)->str, k, sp+1) ;

  free_STRING(string(sp)) ;
  for ( p = sp+1 ; k-- ; p++ )  cell_destroy(p) ;
  return --sp ;
}

CELL *bi_sprintf(sp)
  CELL *sp ;
{ CELL *p ;
  int argcnt = sp->type ;
  STRING *sval ;

  sp -= argcnt-- ; /* sp points at the format string */
  if ( sp->type != C_STRING )  cast1_to_s(sp) ;

  sval = do_printf((FILE *)0, string(sp)->str, argcnt, sp+1) ;
  free_STRING(string(sp)) ;
  sp->ptr = (PTR) sval ;

  for ( p = sp+1 ; argcnt-- ; p++ )  cell_destroy(p) ;

  return sp ;
}



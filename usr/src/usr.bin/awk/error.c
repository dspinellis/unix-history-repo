
/********************************************
error.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	error.c,v $
 * Revision 5.1  91/12/05  07:55:48  brennan
 * 1.1 pre-release
 * 
*/


#include  "mawk.h"
#include  "scan.h"
#include  "bi_vars.h"

#ifndef  EOF
#define  EOF  (-1)
#endif

/* statics */
static void  PROTO( rt_where, (void) ) ;
static void  PROTO( unexpected_char, (void) ) ;
static void  PROTO( missing, (int, char *, int) ) ;
static char *PROTO( type_to_str, (int) ) ;

extern int NR_flag ; /* on if tracking NR */


static struct token_str  {
short token ;
char *str ; }  token_str[] = {
EOF , "end of file" ,
NL , "end of line",
SEMI_COLON , ";" ,
LBRACE , "{" ,
RBRACE , "}" ,
SC_FAKE_SEMI_COLON, "}",
LPAREN , "(" ,
RPAREN , ")" ,
LBOX , "[",
RBOX , "]",
QMARK , "?",
COLON , ":",
OR, "||",
AND, "&&",
ASSIGN , "=" ,
ADD_ASG, "+=",
SUB_ASG, "-=",
MUL_ASG, "*=",
DIV_ASG, "/=",
MOD_ASG, "%=",
POW_ASG, "^=",
EQ  , "==" ,
NEQ , "!=",
LT, "<" ,
LTE, "<=" ,
GT, ">",
GTE, ">=" ,
MATCH, string_buff,
PLUS , "+" ,
MINUS, "-" ,
MUL , "*" ,
DIV, "/"  , 
MOD, "%" ,
POW, "^" ,
NOT, "!" ,
COMMA, "," ,
INC_or_DEC , string_buff ,
DOUBLE  , string_buff ,
STRING_  , string_buff ,
ID  , string_buff ,
FUNCT_ID  , string_buff ,
BUILTIN  , string_buff ,
IO_OUT , string_buff ,
IO_IN, "<" ,
PIPE, "|" ,
DOLLAR, "$" ,
FIELD, "$" ,
0, (char *) 0 } ;

/* if paren_cnt >0 and we see one of these, we are missing a ')' */
static int missing_rparen[] =
{ EOF, NL, SEMI_COLON, SC_FAKE_SEMI_COLON, RBRACE, 0 } ;

/* ditto for '}' */
static int missing_rbrace[] =
{ EOF, BEGIN, END , 0 } ;

static void missing( c, n , ln)
  int c ;
  char *n ;
  int ln ;
{ char *s0, *s1 ;

  if ( pfile_name )
  { s0 = pfile_name ; s1 = ": " ; }
  else s0 = s1 = "" ;

  errmsg(0, "%s%sline %u: missing %c near %s" ,s0, s1, ln, c, n) ; 
}  

void  yyerror(s)
  char *s ; /* we won't use s as input 
  (yacc and bison force this).
  We will use s for storage to keep lint or the compiler
  off our back */
{ struct token_str *p ;
  int *ip ;

  s = (char *) 0 ;

  for ( p = token_str ; p->token ; p++ )
      if ( current_token == p->token )
      { s = p->str ; break ; }

  if ( ! s )  /* search the keywords */
         s = find_kw_str(current_token) ;

  if ( s )
  {
    if ( paren_cnt )
        for( ip = missing_rparen ; *ip ; ip++)
          if ( *ip == current_token )
          { missing(')', s, token_lineno) ;
            paren_cnt = 0 ;
            goto done ;
          }

    if ( brace_cnt )
        for( ip = missing_rbrace ; *ip ; ip++)
          if ( *ip == current_token )
          { missing('}', s, token_lineno) ;
            brace_cnt = 0 ;
            goto done ;
          }

    compile_error("syntax error at or near %s", s) ;

  }
  else  /* special cases */
  switch ( current_token )
  {
    case UNEXPECTED :
            unexpected_char() ; 
            goto done ;

    case BAD_DECIMAL :
            compile_error(
              "syntax error in decimal constant %s",
              string_buff ) ;
            break ;

    case RE :
            compile_error(
            "syntax error at or near /%s/", 
            string_buff ) ;
            break ;

    default :
            compile_error("syntax error") ;
            break ;
  }
  return ;

done :
  if ( ++compile_error_count == MAX_COMPILE_ERRORS ) mawk_exit(1) ;
}

/* system provided errnos and messages */
#ifndef MSDOS_MSC       /* don't need the declarations */
#ifndef THINK_C         /* don't WANT the declarations */
extern int sys_nerr ;
extern char *sys_errlist[] ;
#endif
#endif

#if  HAVE_STDARG_H
#include <stdarg.h>

/* generic error message with a hook into the system error 
   messages if errnum > 0 */

void  errmsg(int errnum, char *format, ...)
{ va_list args ;

  fprintf(stderr, "%s: " , progname) ;
  va_start(args, format) ;
  (void) vfprintf(stderr, format, args) ;
  va_end(args) ;
#ifdef THINK_C
  if ( errnum > 0 )
    fprintf(stderr, " (%s)" , strerror(errnum) ) ;
#else
  if ( errnum > 0 && errnum < sys_nerr )
    fprintf(stderr, " (%s)" , sys_errlist[errnum]) ;
#endif
  fprintf( stderr, "\n") ;
}

void  compile_error(char *format, ...)
{ va_list args ;
  char *s0, *s1 ;

  /* with multiple program files put program name in
     error message */
  if ( pfile_name )
  { s0 = pfile_name ; s1 = ": " ; }
  else
  { s0 = s1 = "" ; }

  fprintf(stderr, "%s: %s%sline %u: " , progname, s0, s1,token_lineno) ;
  va_start(args, format) ;
  vfprintf(stderr, format, args) ;
  va_end(args) ;
  fprintf(stderr, "\n") ;
  if ( ++compile_error_count == MAX_COMPILE_ERRORS ) mawk_exit(1) ;
}

void  rt_error( char *format, ...)
{ va_list args ;

  fprintf(stderr, "%s: run time error: " , progname ) ;
  va_start(args, format) ;
  vfprintf(stderr, format, args) ;
  va_end(args) ;
  putc('\n',stderr) ;
  rt_where() ;
  mawk_exit(1) ;
}

#else

#include <varargs.h>

/*  void errmsg(errnum, format, ...) */

void  errmsg( va_alist)
  va_dcl
{ va_list ap ;
  int errnum ;
  char *format ;

  fprintf(stderr, "%s: " , progname) ;
  va_start(ap) ;
  errnum = va_arg(ap, int) ;
  format = va_arg(ap, char *) ;
  (void) vfprintf(stderr, format, ap) ;
#ifdef THINK_C
  if ( errnum > 0 )
    fprintf(stderr, " (%s)" , strerror(errnum) ) ;
#else
  if ( errnum > 0 && errnum < sys_nerr )
    fprintf(stderr, " (%s)" , sys_errlist[errnum]) ;
#endif
  fprintf( stderr, "\n") ;
}

void compile_error( va_alist )
  va_dcl
{ va_list args ;
  char *format ;
  char *s0, *s1 ;

  if ( pfile_name ) /* print program filename too */
  { s0 = pfile_name ; s1 = ": " ; }
  else s0 = s1 = "" ;

  fprintf(stderr, "%s: %s%sline %u: " , progname, s0, s1,token_lineno) ;
  va_start(args) ;
  format = va_arg(args, char *) ;
  vfprintf(stderr, format, args) ;
  va_end(args) ;
  fprintf(stderr, "\n") ;
  if ( ++compile_error_count == MAX_COMPILE_ERRORS ) mawk_exit(1) ;
}

void  rt_error( va_alist )
  va_dcl
{ va_list args ;
  char *format ;

  fprintf(stderr, "%s: run time error: " , progname ) ;
  va_start(args) ;
  format = va_arg(args, char *) ;
  vfprintf(stderr, format, args) ;
  va_end(args) ;
  putc('\n',stderr) ;
  rt_where() ;
  mawk_exit(1) ;
}

#endif

void bozo(s)
  char *s ;
{ errmsg(0, "bozo: %s" , s) ; mawk_exit(1) ; }

void overflow(s, size)
  char *s ; unsigned size ;
{ errmsg(0 , "program limit exceeded: %s size=%u", s, size) ;
  mawk_exit(1) ; }


/* print as much as we know about where a rt error occured */

static void rt_where()
{
  if ( FILENAME->type != C_STRING ) cast1_to_s(FILENAME) ;
  if ( TEST2(NR) != TWO_DOUBLES ) cast2_to_d(NR) ;

  fprintf(stderr, "\tFILENAME=\"%s\"", string(FILENAME)->str) ;
  if ( NR_flag ) 
      fprintf(stderr, " FNR=%g NR=%g" , FNR->dval, NR->dval) ;

  fprintf(stderr, "\n") ;
}

/* run time */
void rt_overflow(s, size)
  char *s ; unsigned size ;
{ 
  errmsg(0 , "program limit exceeded: %s size=%u", s, size) ;
  rt_where() ;
  mawk_exit(1) ;
}

static void unexpected_char()
{ int c = yylval.ival ;

  fprintf(stderr, "%s: %u: ", progname, token_lineno) ;
  if ( c > ' ')
      fprintf(stderr, "unexpected character '%c'\n" , c) ;
  else
      fprintf(stderr, "unexpected character 0x%02x\n" , c) ;
}

static char *type_to_str( type )
  int type ;
{ char *retval ;

  switch( type )
  {
    case  ST_VAR :  retval = "variable" ; break ;
    case  ST_ARRAY :  retval = "array" ; break ;
    case  ST_FUNCT :  retval = "function" ; break ;
    case  ST_LOCAL_VAR : retval = "local variable" ; break ;
    case  ST_LOCAL_ARRAY : retval = "local array" ; break ;
    default : bozo("type_to_str") ;
  }
  return retval ;
}

/* emit an error message about a type clash */
void type_error(p)
  SYMTAB *p ;
{ compile_error("illegal reference to %s %s", 
    type_to_str(p->type) , p->name) ;
}



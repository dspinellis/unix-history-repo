
/********************************************
scan.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/


/* $Log:	scan.c,v $
 * Revision 5.2  92/02/21  14:16:53  brennan
 * fix:  getline <=
 * 
 * Revision 5.1  91/12/05  07:56:27  brennan
 * 1.1 pre-release
 * 
*/


#include  "mawk.h"
#include  "sizes.h"
#include  "scan.h"
#include  "memory.h"
#include  "field.h"
#include  "init.h"
#include  "fin.h"
#include  "repl.h"
#include  "code.h"

#if HAVE_FCNTL_H
#include  <fcntl.h>
#endif

#include  "files.h"


/* static functions */
static void PROTO(scan_fillbuff, (void) ) ;
static void PROTO(scan_open, (void) ) ;
static int PROTO(slow_next, (void) ) ;
static void PROTO(eat_comment, (void) ) ;
static void PROTO(eat_semi_colon, (void) ) ;
static double PROTO(collect_decimal, (int, int *) ) ;
static int PROTO(collect_string, (void) ) ;
static int  PROTO(collect_RE, (void) ) ;


/*-----------------------------
  program file management
 *----------------------------*/

char *pfile_name ;
STRING  *program_string ;
PFILE *pfile_list ;
static  unsigned char *buffer ;
static  unsigned char *buffp ;  
    /* unsigned so it works with 8 bit chars */
static  int  program_fd   ; 
static  int  eof_flag ;

void  scan_init(cmdline_program)
  char * cmdline_program ;
{ 
  if ( cmdline_program )
  {
    program_fd = -1 ; /* command line program */
    program_string = new_STRING((char *)0, 
                strlen(cmdline_program) + 1 ) ;
    (void) strcpy(program_string->str, cmdline_program) ;
    /* simulate file termination */
    program_string->str[program_string->len-1] = '\n' ;
    buffp = (unsigned char *)  program_string->str ;
    eof_flag = 1 ;
  }
  else /* program from file[s] */
  {
    scan_open() ;
    buffp = buffer = (unsigned char *) zmalloc( BUFFSZ+1 ) ;
    scan_fillbuff() ;
  }

  eat_nl() ; /* scan to first token */
  if ( next() == 0 ) { errmsg(0, "no program") ; mawk_exit(1) ; }
  un_next() ;
  
}

static void  scan_open() /* open pfile_name */
{
  if ( pfile_name[0] == '-' && pfile_name[1] == 0 )
        program_fd = 0 ;
  else
  if ( (program_fd = open(pfile_name, O_RDONLY, 0)) == -1 )
  { errmsg( errno, "cannot open %s", pfile_name) ; mawk_exit(1) ; }
}

void scan_cleanup()
{ 
  if ( program_fd >= 0 ) zfree(buffer, BUFFSZ+1) ;
  else  free_STRING(program_string) ;

  if ( program_fd > 0 )  (void) close(program_fd) ;

  /* redefine SPACE as [ \t\n] */

  scan_code['\n'] = posix_space_flag && rs_shadow.type != SEP_MLR
		    ? SC_UNEXPECTED : SC_SPACE ;
  scan_code['\f'] = SC_UNEXPECTED ; /*value doesn't matter */
  scan_code['\013'] = SC_UNEXPECTED ; /* \v not space */
  scan_code['\r'] = SC_UNEXPECTED ;
}

/*--------------------------------
  global variables shared by yyparse() and yylex()
  and used for error messages too
 *-------------------------------*/

int  current_token = -1 ; 
unsigned  token_lineno ;
unsigned  compile_error_count ;
int   NR_flag ; /* are we tracking NR */
int   paren_cnt ;
int   brace_cnt ;
int   print_flag ;  /* changes meaning of '>' */
int   getline_flag ; /* changes meaning of '<' */

extern  YYSTYPE  yylval ;

/*----------------------------------------
 file reading functions
 next() and un_next(c) are macros in scan.h

 *---------------------*/

static  unsigned lineno = 1 ;


static  void scan_fillbuff()
{ unsigned r ;

  r = fillbuff(program_fd, (char *)buffer, BUFFSZ) ;
  if ( r < BUFFSZ ) 
  { eof_flag = 1 ;
    /* check eof is terminated */
    if ( r && buffer[r-1] != '\n' )
    { buffer[r] = '\n' ; buffer[r+1] = 0 ; }
  }
}

/* read one character -- slowly */
static int slow_next()
{ 
   
  while ( *buffp == 0 )
  {
    if ( !eof_flag ) 
    { buffp = buffer ; scan_fillbuff() ; }
    else
    if ( pfile_list /* open another program file */ )
    {
      PFILE *q ;

      if ( program_fd > 0 ) (void) close(program_fd) ;
      eof_flag = 0 ;
      pfile_name = pfile_list->fname ;
      q = pfile_list ;
      pfile_list = pfile_list->link ;
      ZFREE(q) ;
      scan_open() ;
      token_lineno = lineno = 1 ;
    }
    else  break /* real eof */ ;
  }

  return *buffp++ ; /* note can un_next() , eof which is zero */
}

static void eat_comment()
{ register int c ;

  while ( (c = next()) != '\n' && scan_code[c] ) ;
  un_next() ;
}

/* this is how we handle extra semi-colons that are
   now allowed to separate pattern-action blocks

   A proof that they are useless clutter to the language:
   we throw them away
*/

static  void  eat_semi_colon()
/* eat one semi-colon on the current line */
{ register int c ;

  while ( scan_code[c = next()] == SC_SPACE )  ;
  if ( c != ';' )  un_next() ;
}

void eat_nl() /* eat all space including newlines */
{
  while ( 1 )
    switch( scan_code[next()] )
    { 
      case SC_COMMENT : 
         eat_comment() ;
         break ;
         
      case  SC_NL  :   lineno++ ;
      /* fall thru  */
      case  SC_SPACE  :   break ;
      default :  
          un_next() ; return ;
    }
}

int yylex()
{ 
  register int c ;

  token_lineno = lineno ;

reswitch:

    switch( scan_code[c = next()] )
    {
      case  0  :  
          ct_ret(EOF) ;
          
      case  SC_SPACE  :   goto reswitch ;

      case  SC_COMMENT :
          eat_comment() ; goto reswitch ;

      case  SC_NL  : 
          lineno++ ; eat_nl() ;
          ct_ret(NL) ;

      case SC_ESCAPE :
          while ( scan_code[ c = next() ] == SC_SPACE ) ;
          if ( c == '\n')
          { token_lineno = ++lineno ; goto reswitch ; }
          if ( c == 0 )  ct_ret(EOF) ;
          un_next() ;
          yylval.ival = '\\' ;
          ct_ret(UNEXPECTED) ;


      case  SC_SEMI_COLON  : 
          eat_nl() ;
          ct_ret(SEMI_COLON) ;

      case  SC_LBRACE :  
          eat_nl() ; brace_cnt++ ;
          ct_ret(LBRACE) ;

      case  SC_PLUS  :
          switch( next() )
          {
            case '+' :  
                yylval.ival = '+' ;
                string_buff[0] = 
                     string_buff[1] = '+' ;
                string_buff[2] = 0 ;
                ct_ret(INC_or_DEC) ;

            case  '=' :
                ct_ret(ADD_ASG) ;

            default :  un_next() ; ct_ret(PLUS) ;
          }

      case  SC_MINUS :
          switch( next() )
          {
            case '-' :  
                yylval.ival = '-' ;
                string_buff[0] = 
                     string_buff[1] = '-' ;
                string_buff[2] = 0 ;
                ct_ret(INC_or_DEC) ;

            case  '=' :
                ct_ret(SUB_ASG) ;

            default :  un_next() ; ct_ret(MINUS) ;
          }

      case  SC_COMMA :  eat_nl() ; ct_ret(COMMA) ;

      case  SC_MUL  :  test1_ret('=', MUL_ASG, MUL) ;
      case  SC_DIV :   
          { static int can_precede_div[] =
	    { DOUBLE, STRING_, RPAREN, ID, D_ID, RE, RBOX, FIELD,
	      GETLINE, INC_or_DEC, -1 } ;
	      
	      int *p = can_precede_div ;

            do
                if ( *p == current_token )
		{
		  if ( *p != INC_or_DEC ) 
			test1_ret('=', DIV_ASG, DIV) ;

		  if ( next() == '=' )
		  { un_next() ; ct_ret( collect_RE() ) ;  }
		}
                 
            while ( * ++p != -1 ) ;

            ct_ret( collect_RE() ) ;
          }

      case  SC_MOD  :  test1_ret('=', MOD_ASG, MOD) ;
      case  SC_POW :   test1_ret('=' , POW_ASG, POW) ;
      case  SC_LPAREN : 
          paren_cnt++ ;
          ct_ret(LPAREN) ;

      case  SC_RPAREN : 
          if ( --paren_cnt < 0 )
          { compile_error( "extra ')'" ) ;
            paren_cnt = 0 ;
            goto reswitch ; }

          ct_ret(RPAREN) ;

      case  SC_LBOX   : ct_ret(LBOX) ;
      case  SC_RBOX   : ct_ret(RBOX) ;

      case  SC_MATCH  : 
	  string_buff[0] = '~' ; string_buff[0] = 0 ;
	  yylval.ival = 1 ;
	  ct_ret(MATCH) ;

      case  SC_EQUAL  :
          test1_ret( '=', EQ, ASSIGN ) ;

      case  SC_NOT : /* !  */
	  if ( (c = next()) == '~' )
	  {
	    string_buff[0] = '!' ;
	    string_buff[1] = '~' ;
	    string_buff[2] = 0 ;
	    yylval.ival = 0 ;
	    ct_ret(MATCH) ;
	  }
	  else
	  if ( c == '=' )  ct_ret(NEQ) ;

	  un_next() ;
	  ct_ret(NOT) ;


      case  SC_LT  :  /* '<' */
	  if ( next() == '=' ) ct_ret(LTE) ;
	  else  un_next() ;

          if ( getline_flag )
          { getline_flag = 0 ; ct_ret(IO_IN) ; }
          else  ct_ret(LT) ;

      case  SC_GT  :  /* '>' */
          if ( print_flag && paren_cnt == 0 )
          { print_flag = 0 ;
            /* there are 3 types of IO_OUT 
               -- build the error string in string_buff */
            string_buff[0] = '>' ;
            if ( next() == '>' ) 
            { 
              yylval.ival = F_APPEND ;
              string_buff[1] = '>' ;
              string_buff[2] =  0 ;
            }
            else
            { un_next() ; 
              yylval.ival = F_TRUNC ; 
              string_buff[1] = 0 ;
            }
            return current_token = IO_OUT ;
          }

          test1_ret('=', GTE, GT) ;

      case  SC_OR :
          if ( next() == '|' ) 
          { eat_nl() ; ct_ret(OR) ; }
          else 
          { un_next() ; 

            if ( print_flag && paren_cnt == 0 )
            { print_flag = 0 ; 
              yylval.ival = PIPE_OUT;
              string_buff[0] = '|' ;
              string_buff[1] = 0 ;
              ct_ret(IO_OUT) ;
            }
            else  ct_ret(PIPE) ;
          }

      case  SC_AND :
          if ( next() == '&' )  
          { eat_nl() ; ct_ret(AND) ; }
          else 
          { un_next() ; yylval.ival = '&' ; ct_ret(UNEXPECTED) ; }

      case  SC_QMARK  :  ct_ret(QMARK) ;
      case  SC_COLON  :  ct_ret(COLON) ;
      case  SC_RBRACE :
          if ( --brace_cnt < 0 )
          { compile_error("extra '}'" ) ;
            eat_semi_colon() ;
            brace_cnt = 0 ; goto reswitch ; }

          if ( (c = current_token) == NL || c == SEMI_COLON 
               || c == SC_FAKE_SEMI_COLON  || c == RBRACE  )
          { 
            /* if the brace_cnt is zero , we've completed
               a pattern action block. If the user insists
               on adding a semi-colon on the same line
               we will eat it.  Note what we do below:
               physical law -- conservation of semi-colons */

            if ( brace_cnt == 0 )  eat_semi_colon() ;
            eat_nl() ;
            ct_ret(RBRACE) ;
          }

          /* supply missing semi-colon to statement that
             precedes a '}' */
          brace_cnt++ ; un_next() ;
          current_token = SC_FAKE_SEMI_COLON ;
          return  SEMI_COLON ;

      case  SC_DIGIT  :
      case  SC_DOT    :
          { double d ;
            int flag ;
	    static double double_zero = 0.0 ;
	    static double double_one = 1.0 ;

            if ( (d = collect_decimal(c, &flag)) == 0.0 )
                if ( flag )  ct_ret(flag) ;
                else  yylval.ptr = (PTR) &double_zero ;
            else if ( d == 1.0 ) yylval.ptr = (PTR) &double_one ;
            else 
            { yylval.ptr = (PTR) ZMALLOC(double) ;
	      *(double*)yylval.ptr = d ;
            }
            ct_ret( DOUBLE ) ;
          }

      case  SC_DOLLAR :  /* '$' */
          { double d ;
            int flag ;

            while ( scan_code[c = next()] == SC_SPACE )  ;
            if ( scan_code[c] != SC_DIGIT &&
                 scan_code[c] != SC_DOT )
            { un_next() ; ct_ret(DOLLAR) ; }
            /* compute field address at compile time */
            if ( (d = collect_decimal(c, &flag)) == 0.0 )
                if ( flag )  ct_ret(flag) ; /* an error */
                else  yylval.cp = &field[0] ;
            else
            { int k = (int) d ;

              if ( k > MAX_FIELD )
              { compile_error(
                   "$%g exceeds maximum field(%d)" , d, MAX_FIELD) ;
                k = MAX_FIELD ;
              }
              yylval.cp = field_ptr(k) ;
            }

            ct_ret(FIELD) ;
          }

      case  SC_DQUOTE :
          return current_token = collect_string() ;

      case  SC_IDCHAR : /* collect an identifier */
            { unsigned char *p =
                    (unsigned char *)string_buff + 1 ;
              SYMTAB *stp ;

              string_buff[0] = c ;

              while ( 
                (c = scan_code[ *p++ = next()]) == SC_IDCHAR ||
                       c == SC_DIGIT )  ;
              
              un_next() ; * --p = 0 ;

              switch( (stp = find(string_buff))->type )
              { case ST_NONE :  
                  /* check for function call before defined */
                      if ( next() == '(' )
                      { stp->type = ST_FUNCT ;
                        stp->stval.fbp = (FBLOCK *)
                                zmalloc(sizeof(FBLOCK)) ;
                        stp->stval.fbp->name = stp->name ;
                        stp->stval.fbp->code = (INST *) 0 ;
                        yylval.fbp = stp->stval.fbp ;
                        current_token = FUNCT_ID ;
                      }
                      else
                      { yylval.stp = stp ;
                        current_token = 
                            current_token == DOLLAR ? D_ID : ID ;
                      }
                      un_next() ;
                      break ;

                case  ST_NR  :
		      NR_flag = 1 ;
                      stp->type = ST_VAR ;
                      /* fall thru */
                        
                case ST_VAR :
                case  ST_ARRAY :
                case  ST_LOCAL_NONE :
                case  ST_LOCAL_VAR :
                case  ST_LOCAL_ARRAY :

                      yylval.stp = stp ;
                      current_token =
                            current_token == DOLLAR ? D_ID : ID ;
                      break ;

		case ST_ENV :
		      stp->type = ST_ARRAY ;
		      stp->stval.array = new_ARRAY() ;
		      load_environ(stp->stval.array) ;
		      yylval.stp = stp ;
		      current_token =
			  current_token == DOLLAR ? D_ID : ID ;
		      break ;

                case ST_FUNCT :
                      yylval.fbp = stp->stval.fbp ;
                      current_token = FUNCT_ID ;
                      break ;

                case ST_KEYWORD :  
                      current_token = stp->stval.kw ;
                      break ;

                case  ST_BUILTIN :
                      yylval.bip = stp->stval.bip ;
                      current_token = BUILTIN ;
                      break ;

                case  ST_FIELD  :
                      yylval.cp = stp->stval.cp ;
                      current_token = FIELD ;
                      break ;

                default : 
                      bozo("find returned bad st type") ;
              }
              return  current_token  ;
            }


      case  SC_UNEXPECTED :
            yylval.ival = c & 0xff ;
            ct_ret(UNEXPECTED) ;
    }
    return  0 ; /* never get here make lint happy */
}

/* collect a decimal constant in temp_buff.
   Return the value and error conditions by reference */

static double collect_decimal(c, flag)
  int c ; int *flag ;
{ register unsigned char *p = (unsigned char*) string_buff + 1;
  unsigned char *endp ;
  double d ;

  *flag = 0 ;
  string_buff[0] = c ;

  if ( c == '.' )
  { if ( scan_code[*p++ = next()] != SC_DIGIT )
    { *flag = UNEXPECTED ; yylval.ival = '.' ;
      return 0.0 ; }
  }
  else
  {  while ( scan_code[*p++ = next()] == SC_DIGIT ) ;
     if ( p[-1] != '.' )
     { un_next() ; p-- ; }
  }
  /* get rest of digits after decimal point */
  while ( scan_code[*p++ = next()] == SC_DIGIT )  ;

  /* check for exponent */
  if ( p[-1] != 'e' && p[-1] != 'E' )
  { un_next() ; * --p = 0 ; }
  else  /* get the exponent */
    if ( scan_code[*p = next()] != SC_DIGIT &&
         *p != '-' && *p != '+' )
    { *++p = 0 ; *flag = BAD_DECIMAL ;
      return 0.0 ; }
    else  /* get the rest of the exponent */
    { p++ ;
      while ( scan_code[*p++ = next()] == SC_DIGIT )  ;
      un_next() ; * --p = 0 ;
    }

  errno = 0 ; /* check for overflow/underflow */
  d = strtod( string_buff, (char **)&endp ) ;

#ifndef  STRTOD_UNDERFLOW_ON_ZERO_BUG
  if ( errno )
      compile_error( "%s : decimal %sflow" , string_buff,
        d == 0.0 ? "under" : "over") ;
#else /* sun4 bug */
  if ( errno && d != 0.0 )
      compile_error( "%s : decimal overflow", string_buff) ;
#endif

  if ( endp < p )
  { *flag = BAD_DECIMAL ; return 0.0 ; }
  return d ;
}

/*----------  process escape characters ---------------*/

static char hex_val['f' - 'A' + 1] = {
10,11,12,13,14,15, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0,
 0, 0, 0, 0, 0, 0, 0, 0,
10,11,12,13,14,15 } ;

#define isoctal(x)  ((x)>='0'&&(x)<='7')

#define  hex_value(x)   hex_val[(x)-'A']

#define ishex(x) (scan_code[x] == SC_DIGIT ||\
                  'A' <= (x) && (x) <= 'f' && hex_value(x))

static int PROTO(octal, (char **)) ;
static int PROTO(hex, (char **)) ;

/* process one , two or three octal digits
   moving a pointer forward by reference */
static int octal( start_p )
  char **start_p ;
{ register char *p = *start_p ;
  register unsigned x ;

  x = *p++ - '0' ;
  if ( isoctal(*p) )
  {
    x = (x<<3) + *p++ - '0' ;
    if ( isoctal(*p) )   x = (x<<3) + *p++ - '0' ;
  }
  *start_p = p ;
  return  x & 0xff ;
}

/* process one or two hex digits
   moving a pointer forward by reference */

static int  hex( start_p )
  char **start_p ;
{ register unsigned char *p = (unsigned char*) *start_p ;
  register unsigned x ;
  unsigned t ;

  if ( scan_code[*p] == SC_DIGIT )
        x = *p++ - '0' ;
  else  x = hex_value(*p++) ;

  if ( scan_code[*p] == SC_DIGIT )
        x = (x<<4) + *p++ - '0' ;
  else
  if ( 'A' <= *p && *p <= 'f' && (t = hex_value(*p)) )
  { x = (x<<4) + t ; p++ ; }

  *start_p = (char *) p ;
  return x ;
}

#define  ET_END     9

static struct { char in , out ; } escape_test[ET_END+1] = {
'n' , '\n',
't' , '\t',
'f' , '\f',
'b' , '\b',
'r' , '\r',
'a' , '\07',
'v' , '\013',
'\\', '\\',
'\"', '\"',
0 , 0 } ;


/* process the escape characters in a string, in place . */

char *rm_escape(s)
  char *s ;
{ register char *p, *q ;
  char *t ;
  int i ;

  q = p = s ;

  while ( *p )
      if ( *p == '\\' )
      { 
        escape_test[ET_END].in = * ++p ; /* sentinal */
        i = 0 ;
        while ( escape_test[i].in != *p )  i++ ;

        if ( i != ET_END )  /* in table */
        { 
          p++ ; *q++ = escape_test[i].out ;
        }
        else
        if ( isoctal(*p) ) 
        {
          t = p ;  *q++ = octal(&t) ; p = t ;
        }
        else
        if ( *p == 'x' && ishex(*(unsigned char*)(p+1)) )
        {
          t = p+1 ; *q++ = hex(&t) ; p = t ;
        }
        else
        if ( *p == 0 ) /* can only happen with command line assign */
            *q++ = '\\' ;
        else  /* not an escape sequence */
        { 
          *q++ = '\\' ; *q++ = *p++ ;
        }
      }
      else  *q++ = *p++ ;

  *q = 0 ;
  return s ;
}

static  int  collect_string()
{ register unsigned char *p = (unsigned char *)string_buff ;
  int c ;
  int e_flag = 0 ; /* on if have an escape char */

  while ( 1 )
      switch( scan_code[ *p++ = next() ] )
      { case  SC_DQUOTE : /* done */
              * --p = 0 ;  goto out ;

        case  SC_NL :
              p[-1] = 0 ;
              /* fall thru */

        case  0 :   /* unterminated string */
              compile_error(
              "runaway string constant \"%.10s ..." ,
              string_buff, token_lineno ) ;
              mawk_exit(1) ;

        case SC_ESCAPE :
              if ( (c = next()) == '\n' )
              { p-- ; lineno++ ; }
              else  
                if ( c == 0 )  un_next() ;   
                else 
                { *p++ = c ; e_flag = 1 ; }

              break ;

        default : break ;
      }

out:
    yylval.ptr = (PTR) new_STRING(
         e_flag ? rm_escape( string_buff ) 
                : string_buff ) ;
    return  STRING_ ;
}


static  int  collect_RE()
{ register unsigned char *p = (unsigned char*) string_buff ;
  int c ;
  STRING *sval ;

  while ( 1 )
      switch( scan_code[ *p++ = next() ] )
      { case  SC_DIV : /* done */
              * --p = 0 ;  goto out ;

        case  SC_NL :
              p[-1] = 0 ;
              /* fall thru */

        case  0 :   /* unterminated re */
              compile_error(
              "runaway regular expression /%.10s ..." ,
              string_buff, token_lineno ) ;
              mawk_exit(1) ;

        case SC_ESCAPE :
              switch( c = next() )
              { case '/' :  
                      p[-1] = '/' ; break ;

                case '\n' :
                      p-- ;  break ;

                case  0   :
                      un_next() ;  break ;

                default :
                      *p++ = c ; break ;
              }
              break ;
      }

out:
  /* now we've got the RE, so compile it */
  sval = new_STRING( string_buff ) ;
  yylval.ptr = re_compile(sval) ;
  free_STRING(sval) ;
  return RE ;
}


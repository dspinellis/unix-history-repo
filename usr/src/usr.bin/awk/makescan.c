
/********************************************
makescan.c
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/*$Log:	makescan.c,v $
 * Revision 5.1  91/12/05  07:56:16  brennan
 * 1.1 pre-release
 * 
*/

/* source for makescan.exe which builds the scancode[]
   via:   makescan.exe > scancode.c
*/

#ifdef THINK_C
#include <stdio.h>
#include <console.h>
#include <string.h>
#define SIZE_T(x) (size_t)(x)
#endif

#ifndef SIZE_T
#define SIZE_T(x) (x)
#endif

#define  MAKESCAN

#include  "scan.h"

char   scan_code[256] ;

void  scan_init()
{ 
  register char *p ;

  (void) memset(scan_code, SC_UNEXPECTED, SIZE_T(sizeof(scan_code))) ;
  for( p = scan_code + '0' ; p <= scan_code + '9' ; p++ )
       *p = SC_DIGIT ;
  scan_code[0] = 0 ;
  scan_code[ ' ' ] = scan_code['\t'] = scan_code['\f'] = SC_SPACE ;
  scan_code[ '\r'] = scan_code['\013'] = SC_SPACE ;

  scan_code[';'] = SC_SEMI_COLON ;
  scan_code['\n'] = SC_NL ;
  scan_code['{'] = SC_LBRACE ;
  scan_code[ '}'] = SC_RBRACE ;
  scan_code['+'] = SC_PLUS ;
  scan_code['-'] = SC_MINUS ;
  scan_code['*'] = SC_MUL ;
  scan_code['/'] = SC_DIV ;
  scan_code['%'] = SC_MOD ;
  scan_code['^'] = SC_POW ;
  scan_code['('] = SC_LPAREN ;
  scan_code[')'] = SC_RPAREN ;
  scan_code['_'] = SC_IDCHAR ;
  scan_code['='] = SC_EQUAL ;
  scan_code['#'] = SC_COMMENT ;
  scan_code['\"'] = SC_DQUOTE ;
  scan_code[','] = SC_COMMA ;
  scan_code['!'] = SC_NOT ;
  scan_code['<'] = SC_LT ;
  scan_code['>'] = SC_GT ;
  scan_code['|'] = SC_OR ;
  scan_code['&'] = SC_AND ;
  scan_code['?'] = SC_QMARK ;
  scan_code[':'] = SC_COLON ;
  scan_code['['] = SC_LBOX ;
  scan_code[']'] = SC_RBOX ;
  scan_code['\\'] = SC_ESCAPE ;
  scan_code['.'] = SC_DOT ;
  scan_code['~'] = SC_MATCH ;
  scan_code['$'] = SC_DOLLAR ;

  for( p = scan_code + 'A' ; p <= scan_code + 'Z' ; p++ )
       *p = *(p + 'a' - 'A') = SC_IDCHAR ;

}

void scan_print()
{ register char *p = scan_code ;
  register int c ; /* column */
  register int r ; /* row */

  printf("\n\n/* scancode.c */\n\n\n") ;
  printf( "char scan_code[256] = {\n" ) ;

  for( r = 1 ; r <= 16 ; r++)
  {
    for( c = 1 ; c <= 16 ; c++)
    {
      printf("%2d" , *p++) ;
      if ( r != 16 || c != 16 )  putchar(',') ;
    }
    putchar('\n') ;
  }

  printf("} ;\n") ;
}


int main(argc,argv)
int argc;
char **argv;
{
#ifdef THINK_C
fprintf(stderr, "MAKESCAN for MacMAWK\n");
SetWTitle( FrontWindow(), "\pPC-KIMMO");
argc = ccommand(&argv);
#endif
  scan_init() ; scan_print() ;
  return 0 ;
}

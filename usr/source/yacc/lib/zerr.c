extern int yyline;  /* input line number */

yyerror(s) char *s; {
  extern int yychar;
  extern char *yysterm[];
  printf("\n%s", s );
  if( yyline ) printf(", line %d,", yyline );
  printf(" on input: ");
  if( yychar >= 0400 ) printf("%s\n", yysterm[yychar-0400] );
  else switch ( yychar ) {
    case '\t': printf( "\\t\n" ); return;
    case '\n': printf( "\\n\n" ); return;
    case '\0': printf( "$end\n" ); return;
    default: printf( "%c\n" , yychar ); return;
    }
  }

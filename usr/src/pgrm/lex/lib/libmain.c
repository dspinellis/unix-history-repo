/* libmain - flex run-time support library "main" function */

/* $Header: /usr/fsys/odin/a/vern/flex/RCS/libmain.c,v 1.2 90/05/26 16:50:08 vern Exp $ */

extern int yylex();

int main( argc, argv )
int argc;
char *argv[];

    {
    return yylex();
    }

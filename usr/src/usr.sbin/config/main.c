/*
 * main.c	1.1	81/02/24
 * Config
 *	Do system configuration for VAX/UNIX
 *		1) Build system data structures
 *		2) Build makefile
 *		3) Create header files for devices
 *	Michael Toy -- Berkeley -- 1981
 */

#include <stdio.h>
#include <ctype.h>
#include "y.tab.h"
#include "config.h"

main(argc, argv)
int argc;
char **argv;
{
    if (argc != 2)
    {
	fprintf(stderr, "usage: config <sysname>\n");
	exit(1);
    }
    if (freopen(argv[1], "r", stdin) == NULL)
    {
	perror(argv[1]);
	exit(2);
    }
    dtab = NULL;
    if (yyparse())
	exit(3);
    else
    {
	ioconf();			/* Print ioconf.c */
	ubglue();			/* Create ubglue.s */
	makefile();			/* build Makefile */
	headers();			/* make a lot of .h files */
    }
}

/*
 * get_word
 *	returns EOF on end of file
 *	NULL on end of line
 *	pointer to the word otherwise
 */

char *get_word(fp)
register FILE *fp;
{
    static char line[80];
    register int ch;
    register char *cp;

    while((ch = getc(fp)) != EOF)
	if (ch != ' ' && ch != '\t')
	    break;
    if (ch == EOF)
	return EOF;
    if (ch == '\n')
	return NULL;
    cp = line;
    *cp++ = ch;
    while((ch = getc(fp)) != EOF)
    {
	if (isspace(ch))
	    break;
	*cp++ = ch;
    }
    *cp = '\0';
    if (ch == EOF)
	return EOF;
    ungetc(ch, fp);
    return line;
}

/*	main.c	1.6	82/10/24	*/

#include <stdio.h>
#include <ctype.h>
#include "y.tab.h"
#include "config.h"

/*
 * Config builds a set of files for building a UNIX
 * system given a description of the desired system.
 */
main(argc, argv)
	int argc;
	char **argv;
{

	if (argc != 2) {
		fprintf(stderr, "usage: config sysname\n");
		exit(1);
	}
	PREFIX = argv[1];
	if (freopen(argv[1], "r", stdin) == NULL) {
		perror(argv[1]);
		exit(2);
	}
	dtab = NULL;
	if (yyparse())
		exit(3);
	switch (machine) {

	case MACHINE_VAX:
		vax_ioconf();		/* Print ioconf.c */
		ubglue();		/* Create ubglue.s */
		break;

	case MACHINE_SUN:
		sun_ioconf();
		break;

	default:
		printf("Specify machine type, e.g. ``machine vax''\n");
		exit(1);
	}
	makefile();			/* build Makefile */
	headers();			/* make a lot of .h files */
	printf("Don't forget to run \"make depend\"\n");
}

/*
 * get_word
 *	returns EOF on end of file
 *	NULL on end of line
 *	pointer to the word otherwise
 */
char *
get_word(fp)
	register FILE *fp;
{
	static char line[80];
	register int ch;
	register char *cp;

	while ((ch = getc(fp)) != EOF)
		if (ch != ' ' && ch != '\t')
			break;
	if (ch == EOF)
		return (EOF);
	if (ch == '\n')
		return (NULL);
	cp = line;
	*cp++ = ch;
	while ((ch = getc(fp)) != EOF) {
		if (isspace(ch))
			break;
		*cp++ = ch;
	}
	*cp = 0;
	if (ch == EOF)
		return (EOF);
	ungetc(ch, fp);
	return (line);
}

/*
 * prepend the path to a filename
 */
path(file)
	char *file;
{
	register char *cp;

	cp = malloc(strlen(PREFIX)+strlen(file)+5);
	strcpy(cp, "../");
	strcat(cp, PREFIX);
	strcat(cp, "/");
	strcat(cp, file);
	return (cp);
}

/* @(#)findaed.c	1.3	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 * This file contains a single routine which will find the correct
 * AED display to use from the current terminal.  
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 */

#include "gremlin.h"

/* Imports from library routines: */

extern FILE *fopen();
extern char *ttyname();

/* imports from config.c */

extern char GDisplays[];

char *
FindAED()

/*---------------------------------------------------------
 *	This routine reads out the name of the terminal associated
 *	with the standard input, then selects an AED display to be
 *	used, based on that terminal name.
 *
 *	Results:
 *	The return value is a pointer to a string that is the device
 *	name of the AED display to use.  This name is found by searching
 *	in the file "displays" in the current search path.  The file
 *	consists of pairs of strings.  If the first string of a pair
 *	matches the name of our input terminal, then the second string
 *	of the pair is used as the AED device name.  If no match occurs
 *	then /dev/null is used.
 *
 *	Side Effects:	None.
 *---------------------------------------------------------
 */

{
    char name1[100], *tty;
    static char name2[100];
    FILE *f;
    tty = ttyname(fileno(stderr));
    if (tty == NULL) return NULL;
    f = fopen(GDisplays, "r");
    if (f == NULL) return NULL;
    while (TRUE)
    {
	if (fscanf(f, "%99s %99s", name1, name2) < 2)
	{
	    (void) fclose(f);
	    return NULL;
	}
	if (strcmp(name1, tty) == 0)
	{
	    (void) fclose(f);
	    return name2;
	}
	while (getc (f) > 10);		/* ignore extra characters on a line */
    }
}

/* gdump.c	1.1	83/06/17 	by David Slattengren
 *
 *      This file contains a program for printing gprint raster files.
 *   Gprint puts out a generic file, and gdump changes the size to fit
 *   the proper device that's being used.
 */

#include <stdio.h>
#include <signal.h>
#include "gprint.h"

#define LPR	"/usr/ucb/lpr"

extern char *mktemp();

char *picture	= "/usr/tmp/rastAXXXXXX";	/* output file name */
int  temp;					/* output file number */
char *lpargs[9]	= { "lpr", "-Pvarian", "-v",
		   "-s", "-r", "-J", "gdump" };
int  outwidth	= Vbytperlin;		/* number of chars per line to output*/
int  outlength	= Vylen;		/* number of lines to output */

int  infile	= 0;		/* input file (default = stdin) */
int  FileFound	= 0;		/* flag for filename on input */
char *arg;			/* intermediary command line argument */
char *file;			/* input file name */
char buf [Wbytperlin];		/* intermediary raster line buffer */


cleanup()		/* Called if program stopped, or ... */
{
    unlink (picture);
    exit (1);
}


main (argc, argv)
int argc;
char *argv[];
{
    lpargs [7] = picture;	/* set file for lpr to read */
    lpargs [8] = 0;

    while (--argc > 0)		/* Parse the command line. */
    {
        arg = *++argv;
        if (arg[0] != '-') {
	    if (FileFound) {
		fprintf (stderr, "gdump: Only one file may be printed\n");
		exit(1);
	    }
            lpargs [6] = file = arg;	/* set filename (and to lpr) */
            FileFound = 1;
        } else {
            switch (*++arg) {
		case 'W':		/* Print to wide (versatec) device */
			outwidth = Wbytperlin;
			outlength = Wylen;
			lpargs[1] = "-Pversatec";
			break;
		case 'V':		/* Print to narrow (varian) device */
			outwidth = Vbytperlin;
			outlength = Vylen;
			lpargs[1] = "-Pvarian";
			break;
		default:
			printf ("unknown switch %c", *arg);
			exit (1);
			break;
            }
        }
    }

    if (FileFound) {		/* open input file, if one exists */
	fclose (stdin);
	infile = open (file, 0);
	if (infile == -1) {
            fprintf (stderr, "can't open %s", file);
	    exit(1);
	}
    }
					/* clear out line buffer */
    for (FileFound = 0; FileFound < Wbytperlin; buf [FileFound++] = 0)
	;

    mktemp (picture);			/* make up file name */

    signal (SIGTERM, cleanup);			/* prepare to be killed */
    if (signal (SIGINT, SIG_IGN) != SIG_IGN)	/*    or interrupted */
	signal (SIGINT, cleanup);

    if ((temp = creat (picture, 0666)) == -1) {
	fprintf (stderr, "gdump: can't create %s\n", picture);
	cleanup ();
    }

/****** transfer the raster file from input to output, fixing line
	lengths, if necessary, and truncating to one page length *********/

    while ((read (infile, buf, Vbytperlin) == Vbytperlin) && outlength--) {
	if (write (temp, buf, outwidth) != outwidth) {
	    fprintf (stderr, "gdump: error writing file %s\n", picture);
	    cleanup();
	}
    }
    close (infile);				/* eat the rest of input */
    while (read (infile, buf, Vbytperlin) > 0)	/* if there is any */
	;

    execv (LPR, lpargs);
    fprintf (stderr, "gdump: can't exec %s\n", LPR);
    cleanup();
}

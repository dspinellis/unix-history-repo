/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"trek.h"
# include	<stdio.h>
# include	<sgtty.h>
# define	PRIO		00	/* default priority */

int	Mother	= 51 + (51 << 8);

/*
**	 ####  #####    #    ####          #####  ####   #####  #   #
**	#        #     # #   #   #           #    #   #  #      #  #
**	 ###     #    #####  ####            #    ####   ###    ###
**	    #    #    #   #  #  #            #    #  #   #      #  #
**	####     #    #   #  #   #           #    #   #  #####  #   #
**
**	C version by Eric P. Allman 5/76 (U.C. Berkeley) with help
**		from Jeff Poskanzer and Pete Rubinstein.
**
**	I also want to thank everyone here at Berkeley who
**	where crazy enough to play the undebugged game.  I want to
**	particularly thank Nick Whyte, who made considerable
**	suggestions regarding the content of the game.  Why, I'll
**	never forget the time he suggested the name for the
**	"capture" command.
**
**	Please send comments, questions, and suggestions about this
**		game to:
**			Eric P. Allman
**			Project INGRES
**			Electronics Research Laboratory
**			Cory Hall
**			University of California
**			Berkeley, California  94720
**
**	If you make ANY changes in the game, I sure would like to
**	know about them.  It is sort of an ongoing project for me,
**	and I very much want to put in any bug fixes and improvements
**	that you might come up with.
**
**	FORTRASH version by Kay R. Fisher (DEC) "and countless others".
**	That was adapted from the "original BASIC program" (ha!) by
**		Mike Mayfield (Centerline Engineering).
**
**	Additional inspiration taken from FORTRAN version by
**		David Matuszek and Paul Reynolds which runs on the CDC
**		7600 at Lawrence Berkeley Lab, maintained there by
**		Andy Davidson.  This version is also available at LLL
**		and at LMSC.  In all fairness, this version was the
**		major inspiration for this version of the game (trans-
**		lation:  I ripped off a whole lot of code).
**
**	Minor other input from the "Battelle Version 7A" by Joe Miller
**		(Graphics Systems Group, Battelle-Columbus Labs) and
**		Ross Pavlac (Systems Programmer, Battelle Memorial
**		Institute).  That version was written in December '74
**		and extensively modified June '75.  It was adapted
**		from the FTN version by Ron Williams of CDC Sunnyvale,
**		which was adapted from the Basic version distributed
**		by DEC.  It also had "neat stuff swiped" from T. T.
**		Terry and Jim Korp (University of Texas), Hicks (Penn
**		U.), and Rick Maus (Georgia Tech).  Unfortunately, it
**		was not as readable as it could have been and so the
**		translation effort was severely hampered.  None the
**		less, I got the idea of inhabited starsystems from this
**		version.
**
**	Permission is given for use, copying, and modification of
**		all or part of this program and related documentation,
**		provided that all reference to the authors are maintained.
**
**
**********************************************************************
**
**  NOTES TO THE MAINTAINER:
**
**	There is a compilation option xTRACE which must be set for any
**	trace information to be generated.  It is probably defined in
**	the version that you get.  It can be removed, however, if you
**	have trouble finding room in core.
**
**	Many things in trek are not as clear as they might be, but are
**	done to reduce space.  I compile with the -f and -O flags.  I
**	am constrained to running with non-seperated I/D space, since
**	we don't have doubleing point hardware here; even if we did, I
**	would like trek to be available to the large number of people
**	who either have an 11/40 or do not have FP hardware.  I also
**	found it desirable to make the code run reentrant, so this
**	added even more space constraints.
**
**	I use the portable C library to do my I/O.  This is done be-
**	cause I wanted the game easily transportable to other C
**	implementations, and because I was too lazy to do the doubleing
**	point input myself.  Little did I know.  The portable C library
**	released by Bell Labs has more bugs than you would believe, so
**	I ended up rewriting the whole blessed thing.  Trek excercises
**	many of the bugs in it, as well as bugs in some of the section
**	III UNIX routines.  We have fixed them here.  One main problem
**	was a bug in alloc() that caused it to always ask for a large
**	hunk of memory, which worked fine unless you were almost out,
**	which I inevitably was.  If you want the code for all of this
**	stuff, it is also available through me.
**
***********************************************************************
*/

main(argc, argv)
int	argc;
char	**argv;
{
	long			vect;
	/* extern FILE		*f_log; */
	register char		opencode;
	int			prio;
	register int		ac;
	register char		**av;
	struct	sgttyb		argp;
	int			been_here = 0;

	av = argv;
	ac = argc;
	av++;
	time(&vect);
	srand(vect);
	opencode = 'w';
	prio = PRIO;
	if (gtty(1, &argp) == 0)
	{
		if ((argp.sg_ispeed ) < B1200)
			Etc.fast++;
	}
	while (ac > 1 && av[0][0] == '-')
	{
		switch (av[0][1])
		{
		  case 'a':	/* append to log file */
			opencode = 'a';
			break;

		  case 'f':	/* set fast mode */
			Etc.fast++;
			break;

		  case 's':	/* set slow mode */
			Etc.fast = 0;
			break;

#		ifdef xTRACE
		  case 't':	/* trace */
			if (getuid() != Mother)
				goto badflag;
			Trace++;
			break;
#		endif

		  case 'p':	/* set priority */
			if (getuid() != Mother)
				goto badflag;
			if (scanf(-1, &av[0][2], "%d", &prio) > 0)
				break;

		  default:
		  badflag:
			printf("Invalid option: %s\n", av[0]);

		}
		ac--;
		av++;
	}
	if (ac > 2)
		syserr(0, "arg count");
		/*
	if (ac > 1)
		f_log = fopen(av[0], opencode);
		*/

	printf("\n   * * *   S T A R   T R E K   * * *\n\nPress return to continue.\n");

	setexit();
	if ( been_here == 1 )
	{
		if ( !getynpar("Another game") )
			exit(0);
	}
	been_here = 1;
	do
	{
		setup();
		play();
	} while (getynpar("Another game"));

	fflush(stdout);
}

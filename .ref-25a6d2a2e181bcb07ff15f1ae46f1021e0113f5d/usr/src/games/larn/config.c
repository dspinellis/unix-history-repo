/*
 *	config.c	--	This defines the installation dependent variables.
 *                  Some strings are modified later.  ANSI C would
 *                  allow compile time string concatenation, we must
 *                  do runtime concatenation, in main.
 *
 *		Larn is copyrighted 1986 by Noah Morgan.
 */
#include "header.h"

#ifndef LARNHOME
#define LARNHOME "/usr/games/lib/larn/"		/* normally supplied by a Makefile */
#endif

#ifndef WIZID
#define WIZID	0
#endif

/*
 *	All these strings will be appended to in main() to be complete filenames
 */

		/* the game save filename   */
char savefilename[SAVEFILENAMESIZE] = 					LARNHOME;

		/* the score file	    	*/
char scorefile[sizeof(LARNHOME)+sizeof(SCORENAME)] =	LARNHOME;

		/* the logging file     	*/
char logfile[sizeof(LARNHOME)+sizeof(LOGFNAME)]  =		LARNHOME;

		/* the help text file		*/
char helpfile[sizeof(LARNHOME)+sizeof(HELPNAME)] = 		LARNHOME;

		/* the maze data file		*/
char larnlevels[sizeof(LARNHOME)+sizeof(LEVELSNAME)] = 	LARNHOME;

		/* the fortune data file	*/
char fortfile[sizeof(LARNHOME)+sizeof(FORTSNAME)] =		LARNHOME;

		/* the .larnopts filename */
char optsfile[128] ="/.larnopts";				/* the option file			*/

		/* the player id datafile name */
char playerids[sizeof(LARNHOME)+sizeof(PLAYERIDS)] =	LARNHOME;

		/* the holiday datafile */
char holifile[sizeof(LARNHOME)+sizeof(HOLIFILE)] =		LARNHOME;

char diagfile[] ="Diagfile";					/* the diagnostic filename	*/
char ckpfile[] ="Larn12.0.ckp";					/* the checkpoint filename	*/
char *password ="pvnert(x)";					/* the wizards password <=32*/
#if WIZID == -1
int wisid=0;			/* the user id of the only person who can be wizard */
#else
int wisid=WIZID;		/* the user id of the only person who can be wizard */
#endif
char psname[PSNAMESIZE]="larn";						/* the process name		*/

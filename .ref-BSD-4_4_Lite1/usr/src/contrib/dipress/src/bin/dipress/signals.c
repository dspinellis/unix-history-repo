/*******************************************************************************
 *
 *	signals	--	signal handling for dipress
 *
 *	William LeFebvre
 *	with modifications by John Mellor-Crummey
 *
 *	Copyright (c) 1985 Xerox Corporation
 ******************************************************************************/

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>

#include "defs.h"	/* constant and macro definitions */
#include "externs.h"	/* declarations for global variables */

int abortProcess();

/*-----------------------------------------------------------------------------
 *	signalHandler	--	insure files are cleaned up properly upon exit
 *				caused by an external signal
 *---------------------------------------------------------------------------*/
signalHandler()
{
	int goodbye();

	if ( signal(SIGINT, abortProcess) == (int (*)()) SIG_IGN)
	{
		/* if interrupts are turned off, 
		 * disable other externally generated 
		 * harmful signals 
		 */
		(void) signal(SIGINT, (int (*)()) SIG_IGN);
		(void) signal(SIGQUIT, (int (*)()) SIG_IGN);
		(void) signal(SIGHUP, (int (*)()) SIG_IGN);
	}
	else
	{
		(void) signal(SIGQUIT, goodbye);
		(void) signal(SIGHUP, goodbye);
	}
	(void) signal(SIGTERM, abortProcess);
}

/*-----------------------------------------------------------------------------
 * abortProcess	--	signal handler that is called upon serious error
 *---------------------------------------------------------------------------*/
abortProcess()			
{
	(void) close(pagebodyfile);
	(void) unlink(tempfilename);

	(void) close(outputfile);
	tempfilename[strlen(tempfilename) - 1] = '\0';
	if (outputfile != fileno(stdout))
		(void) unlink(tempfilename);
	exit(1);
}


/*-----------------------------------------------------------------------------
 * goodbye	--	function called for friendly cleanup
 *---------------------------------------------------------------------------*/
goodbye()
{
	if (outputfile == -1)
		exit(1);
	resetDevice();
	exit(0);
}


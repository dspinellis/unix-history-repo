/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define NOEXTERNS

#include "tune.h"

/* these are variables that can be set with the set command, so they are
   allocated more memory than they actually need for the defaults */
char	TmpFilePath[64] = ".",
	Shell[64] = "command",
	ShFlags[16] = "-c",
	CmdDb[64] = "c:/unix/cmds.doc";

/* these guys are not settable */
char	*d_tempfile = "joveXXXXXX",	/* buffer lines go here */
	*Joverc = "jove.rc";


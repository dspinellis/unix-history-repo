#
/*
 *	this file contains code and declarations that are common to
 *	all main lines. This file sould be included INSIDE the 
 *	main block. It contains variables that we want on the stack
 *	and initializes some pointers to them. 
 *
 *	We also take care of the problem of some systems not honoring
 *	the setuid bit when root runs a program.
 *
 *	Ray Essick	May 7, 1982
 */

msk = umask(NOTESUMASK);
globuid = getuid () & UIDMASK;			/* set this */
if (globuid == 0) 				/* root ? */ 
    setuid (NOTESUID);				/* take care of that */

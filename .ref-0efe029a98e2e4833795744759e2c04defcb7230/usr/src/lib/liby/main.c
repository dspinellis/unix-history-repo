/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.2 (Berkeley) %G%";
#endif /* not lint */

main()
{
	extern yyparse();

	(void)yyparse();
	return(0);
}

/* whatnow.c - the MH WhatNow? shell */
#ifndef	lint
static char ident[] = "@(#)$Id: whatnow.c,v 1.3 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#ifdef LOCALE
#include	<locale.h>
#endif

main (argc, argv)
int	argc;
char  **argv;
{
#ifdef LOCALE
    setlocale(LC_ALL, "");
#endif
    WhatNow (argc, argv);
}

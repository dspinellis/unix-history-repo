/* mhl.c - the MH message listing program */
#ifndef	lint
static char ident[] = "@(#)$Id: mhl.c,v 1.3 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */
#ifdef LOCALE
#include	<locale.h>
#endif

main (argc, argv)
int     argc;
char  **argv;
{
#ifdef LOCALE
    setlocale(LC_ALL, "");
#endif
    done (mhl (argc, argv));
}

/*  */

/* Cheat:  we are loaded with adrparse, which wants a routine called
   OfficialName().  We call adrparse:getm() with the correct arguments
   to prevent OfficialName() from being called.  Hence, the following
   is to keep the loader happy.
 */

char   *OfficialName (name)
register char  *name;
{
    return name;
}

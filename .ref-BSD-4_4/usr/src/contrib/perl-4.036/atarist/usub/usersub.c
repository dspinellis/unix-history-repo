/* $RCSfile: usersub.c,v $$Revision: 4.0.1.1 $$Date: 92/06/08 11:54:52 $
 *
 * $Log:	usersub.c,v $
 * Revision 4.0.1.1  92/06/08  11:54:52  lwall
 * Initial revision
 * 
 * Revision 4.0.1.1  91/11/05  19:07:24  lwall
 * patch11: there are now subroutines for calling back from C into Perl
 * 
 * Revision 4.0  91/03/20  01:56:34  lwall
 * 4.0 baseline.
 * 
 * Revision 3.0.1.1  90/08/09  04:06:10  lwall
 * patch19: Initial revision
 * 
 */

#include "EXTERN.h"
#include "perl.h"

int
userinit()
{
    install_null();	/* install device /dev/null or NUL: */
    init_curses();
    return 0;
}

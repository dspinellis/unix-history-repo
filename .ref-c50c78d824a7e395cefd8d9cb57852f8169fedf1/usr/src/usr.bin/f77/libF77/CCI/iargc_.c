/*
 *	@(#)iargc_.c	5.1 (Berkeley) %G%
 */

long int iargc_()
{
extern int xargc;
return ( xargc - 1 );
}

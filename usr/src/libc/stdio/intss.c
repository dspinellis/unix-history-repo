/* @(#)intss.c	4.2 (Berkeley) 7/1/81 */

#include <stdio.h>
/*
 * Name refers to ``in TSS'', i.e. in the
 * TSS operating system and thus that input is a terminal.
 */
intss()
{

	return (isatty(fileno(stdin)));
}

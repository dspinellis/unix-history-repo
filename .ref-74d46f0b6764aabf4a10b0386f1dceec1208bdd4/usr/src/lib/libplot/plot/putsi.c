#ifndef lint
static char sccsid[] = "@(#)putsi.c	4.1 (Berkeley) %G%";
#endif

#include <stdio.h>
putsi(a){
	putc((char)a,stdout);
	putc((char)(a>>8),stdout);
}

#ifndef lint
static char sccsid[] = "@(#)misc.c	1.2 (CWI) 85/10/02";
#endif lint

#include "defs.h"
#include "ext.h"

/*
 * Number register alocation
 *
 * This array must have at least 3*qcol entries or
 * illegal register names will result. (bwk)
 */

 /*
  * This will counts for the restriction on MAXCOL
  */

char *nregs[] = {
	"40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
	"50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
	"60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
	"70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
	"80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
	"90", "91", "92", "93", "94", "95", "96", "97", "4q", "4r",
	"4s", "4t", "4u", "4v", "4w", "4x", "4y", "4z", "4;", "4.",
	"4a", "4b", "4c", "4d", "4e", "4f", "4g", "4h", "4i", "4j",
	"4k", "4l", "4m", "4n", "4o", "4p", "5a", "5b", "5c", "5d",
	"5e", "5f", "5g", "5h", "5i", "5j", "5k", "5l", "5m", "5n",
	"5o", "5p", "5q", "5r", "5s", "5t", "5u", "5v", "5w", "5x",
	0
};

char *
reg(col, place)
{
	register int i;

	i = sizeof(nregs);

	if( i < 2 * 3 * qcol)
		error("Too many columns for registers");
	if( i < qcol * place + col)
		error("Out of registers");
	return(nregs[qcol * place + col]);
}

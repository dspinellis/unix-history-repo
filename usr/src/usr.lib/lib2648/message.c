/*	message.c	4.1	83/03/09	*/
/*
 * message: print str on the screen in the message area.
 */

#include "2648.h"

message(str)
char *str;
{
	dispmsg(str, 4, 4, 100);
}

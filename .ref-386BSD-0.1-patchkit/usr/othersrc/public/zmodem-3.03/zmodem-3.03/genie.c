/*
 *
 *  Rev 5-08-89
 *  This file contains GEnie specific code for setting terminal modes,
 *  very little is specific to ZMODEM or YMODEM per se (that code is in
 *  sz.c and rz.c).  The CRC-16 routines used by XMODEM, YMODEM, and ZMODEM
 *  are also in this file, a fast table driven macro version
 *
 *   This file is #included so the main file can set parameters such as HOWMANY.
 *   See the main file rz.c for compile instructions.
 */

#include <string.h>
#include <fcntl.h>

#define XARGSFILE "/"
#define XX

STATIC char Myattn[] = { 0335, 0336, 0336, 0,
	 24,24,24,24,24,24,24,24,24,24,13
};
#define ALTCANOFF 4

/*
extern unsigned int _fmode = O_BINARY;
*/

/*
 * return 1 iff stdout and stderr are different devices
 *  indicating this program operating with a modem on a
 *  different line
 */
int Fromcu;		/* Were called from cu or yam */
from_cu()
{
	return 0;
}
cucheck()
{
}


int Twostop;		/* Use two stop bits */




/*
 * mode(n)
 *  3: save old tty stat, set raw mode with flow control
 *  2: set XON/XOFF for sb/sz with ZMODEM or YMODEM-g
 *  1: save old tty stat, set raw mode 
 *  0: restore original tty mode
 */
mode(n)
{
	static did0 = FALSE;

	vfile("mode:%d", n);
	switch(n) {
	case 2:		/* Un-raw mode used by sz, sb when -g detected */
	case 1:
	case 3:
		did0 = TRUE;
		system("set x on");
		system("set e off");
		system("set t13,10");
/*
		system("set b3");
*/
		reset(1);
		binary(1); 
		/* Assume fd 1 is stdout (not documented in GEnie) */
		fcntl(1, F_SETFL,
		  ((fcntl(1, F_GETFL, 0)|O_BINARY)& ~O_POST_BREAK));
		return OK;
	case 0:
		if(!did0)
			return ERROR;

		return OK;
	default:
		return ERROR;
	}
}

sendbrk()
{
}
/*
 * readline(timeout) reads character(s) from file descriptor 0
 * timeout is in tenths of seconds
 */
readline(timeout)
{
	static char byt[1];

	fflush(stdout);
	read(0, byt, 1);
	return (byt[0]&0377);
}

flushmo()
{
	fflush(stdout);
}

purgeline() {}


/* End of genie.c */

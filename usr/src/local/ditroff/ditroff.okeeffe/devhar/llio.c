/*
 * lowest level io
 *
 * The Harris has a weird protocol, which wil be maintained by
 * a different programme (now called harprot, will be changed into
 * a typesetter spooler in near future).
 *
 * Due to thenature of the harris is a slight protocol to harprot
 * necessary as well:
 *	Operator messages are ruined if they aren't at the end of a buffer
 *
 * So to harprot we send a buffer with a header containing a bufferlenght
 * and (for histrorical reasons, the amount of paper used.)
 * The paper use will have to be counted by harprot in future.
 *
 */

#include <stdio.h>
#include "hcodes.h"
#include "llio.h"

char	obuf[BUFSIZE];
char	*obufp = obuf;
char	*eobufp = &obuf[BUFSIZE-1];
int	typeset;	/* if set, we are really typesetting */

extern int	fcut;	/* if set, we have just cut the paper */
extern int	papuse;	/* used paper in feed */
extern int	tf;
extern char	harcode;
extern int	eflag;


oput( c )
char	c;
{
	typeset = 1;

	if( fcut ) {		/* See harris manual, appendix D */
		fcut = 0;
		oput(VMV);
		oput(0);
		oput(0);
	}
	if( obufp > eobufp)
		flusho();
	*obufp++ = c & BMASK;
}

flusho()
{	int length, i;
	if ( length = (int )(obufp - obuf )) {
		if ( !papuse )
			papuse++;	/* account always at least 1 foot */
		/* for testing only */
		/*papuse = 1;*/
		if ( write( tf, (char *)&length, 2) != 2 ||
		     write( tf, (char *)&papuse, 2) != 2 ||
		     (i = write( tf, obuf, length)) <= 0) {
			printf("dhar: write error\n");
			exit(1);
		}
		obufp = obuf;
	}
}

xflusho(nbytes)
int nbytes;
{
	if ( obufp > &obuf[BUFSIZE - nbytes])
		flusho();
}

flushchar()
{
	if( harcode ) {
		oput(harcode);
		oput(0); oput(0);
		harcode = 0;
	}
}

ex()
{	if(!typeset)
		return;
	if(!fcut)
		cut();
	fcut = 0;
	oput(EOT);
	oput(STP);
	operator("End of job");
	typeset = 0;
	if(eflag) {
		fprintf( stderr, "Don't forget to bring the machine");
		fprintf( stderr, " back in a normal state\n");
		eflag--;
	}
}

operator( s )
char	*s;
{	register int i, j, n;
	char buf[PANEL_SIZE];	
	n = 0;
	while ( *s ) {		/* ascii from harris, stupid fools */
		if( *s >= 'a' && *s <= 'z')
			*s += 'A' - 'a';
		if(*s == '{')
			*s = 0136;
		if(*s == '}')
			*s = 0137;
		if( *s < 040 || *s > 0137) {
			error( !FATAL, "illegal char %o to display", *s);
			s++;
			continue;
		}
		buf[n++] = *s++;
		if( n >= PANEL_SIZE)
			break;
	}
	/*
	 * now center the message and send it away
	 */
	if( n <= PANEL_SIZE && n > 0) {
		flusho(PANEL_SIZE + 1);
			/*
			 * always flushing the buffer seems to be better
			 */
		oput(OPR);
		i = ( PANEL_SIZE - n ) / 2;
		for( j = 0; j < i; j++)
			oput(' ');
		for( j = 0; j < n; j++)
			oput(buf[j]);
		for( j = i + n; j < PANEL_SIZE; j++)
			oput(' ');
		flusho();
	}
}

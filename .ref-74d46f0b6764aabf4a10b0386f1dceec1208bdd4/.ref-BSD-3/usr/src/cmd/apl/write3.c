#include "apl.h"

dofix() {
	register unsigned char cha;
	extern TERMtype;
	register unsigned char	*fixline;
	extern unsigned char *iline;
	extern	unsigned char changeinput[];

	fixline = iline;
	while ( *fixline != '\0' ) {
		cha = *fixline;
		*fixline++ = changeinput[( int )cha];
	}
}

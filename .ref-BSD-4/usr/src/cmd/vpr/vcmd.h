/*	vcmd.h	3.1	4/9/80	*/

#define VPRINT		0100
#define	VPLOT		0200
#define VPRINTPLOT	0400

#define VGETSTATE	(('v'<<8)|0)
#define VSETSTATE	(('v'<<8)|1)

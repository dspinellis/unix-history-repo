/*	pagsiz.h	4.1	83/05/03	*/

#define	NBPG	512
#define	PGOFSET	511
#define	CLSIZE	2
#define	CLOFSET	1023
#define	PAGSIZ	(NBPG*CLSIZE)
#define	PAGRND	((PAGSIZ)-1)

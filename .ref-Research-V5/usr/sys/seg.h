/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 * KT-11 registers
 */

#define	UISD	0177600
#define	UISA	0177640
#define	RO	02
#define	RW	06
#define	WO	04
#define	ED	010
struct {
	int	r[8];
};
int	*ka6;

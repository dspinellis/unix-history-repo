#
/*
 * rp03 disk driver
 */

#include <sys/param.h>
#include <sys/inode.h>
#include "saio.h"

struct device {
	int	rpds;
	int	rper;
	union {
		int	w;
		char	c;
	} rpcs;
	int	rpwc;
	char	*rpba;
	int	rpca;
	int	rpda;
};

#define RPADDR ((struct device *) 0176710)

#define	GO	01
#define	DONE	0200
#define	RESET	0
#define	HSEEK	014

#define	IENABLE	0100
#define	READY	0200
#define	RCOM	4
#define	WCOM	2

#define	SUFU	01000
#define	SUSU	02000
#define	SUSI	04000
#define	HNF	010000



rpstrategy(io, func)
register struct iob *io;
{
	int com,cn,tn,sn;


/*
	dn = unit>>3;
	bn = bp->b_blkno;
	cn = bn/(20*10) + rp_sizes[unit&07].cyloff;
*/
	cn = io->i_bn/(20*10);
	sn = io->i_bn%(20*10);
	tn = sn/10;
	sn = sn%10;
	RPADDR->rpcs.w = (io->i_unit<<8);
	RPADDR->rpda = (tn<<8) | sn;
	RPADDR->rpca = cn;
	RPADDR->rpba = io->i_ma;
	RPADDR->rpwc = -(io->i_cc>>1);
	com = (segflag<<4)|GO;
	if (func == READ)
		com |= RCOM; else
		com |= WCOM;
	
	RPADDR->rpcs.w |= com;
	while ((RPADDR->rpcs.w&DONE)==0)
		;
	if (RPADDR->rpcs.w < 0) {	/* error bit */
		printf("disk error: cyl=%d track=%d sect=%d er=%o ds=%o\n",
		    cn, tn, sn, RPADDR->rper, RPADDR->rpds);
		return(-1);
	}
	return(io->i_cc);
}

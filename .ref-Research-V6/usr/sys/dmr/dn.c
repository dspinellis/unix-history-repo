#
/*
 */

/*
 * DN-11 ACU interface
 */

#include "../param.h"
#include "../conf.h"
#include "../user.h"

struct dn {
	struct {
		char	dn_stat;
		char	dn_reg;
	} dn11[3];
}

#define	DNADDR	0175200

#define	PWI	00200
#define	ACR	00100
#define	DLO	0020
#define	DONE	0200
#define	IENABLE	0100
#define	DSS	040
#define	PND	020
#define	MENABLE	04
#define	DPR	02
#define	CRQ	01

#define	DNPRI	5

dnopen(dev, flag)
{
	register struct dn *dp;
	register int rdev;

	rdev = dev.d_minor;
	dp = &DNADDR->dn11[rdev];
	if (dp->dn_reg&(PWI|DLO))
		u.u_error = ENXIO;
	else {
		DNADDR->dn11[0].dn_stat =| MENABLE;
		dp->dn_stat = IENABLE|MENABLE|CRQ;
	}
}

dnclose(dev)
{
	DNADDR->dn11[dev.d_minor].dn_stat =& MENABLE;
}

dnwrite(dev)
{
	register struct dn *dp;
	register c;
	extern lbolt;

	dp = &DNADDR->dn11[dev.d_minor];
	for(;;) {
		while ((dp->dn_stat&DONE)==0)
			sleep(DNADDR, DNPRI);
		dp->dn_stat =& ~DONE;
	    contin:
		if (dp->dn_reg&(PWI|ACR)) {
			u.u_error = EIO;
			return;
		}
		if (dp->dn_stat&DSS)
			return;
		c = 0;
		if (u.u_count==0 || (dp->dn_stat&PND)==0 || (c=cpass())<0)
			continue;
		if (c=='-') {
			sleep(&lbolt, DNPRI);
			sleep(&lbolt, DNPRI);
			goto contin;
		}
		dp->dn_reg = c-'0';
		dp->dn_stat =| DPR;
	}
}

dnint(dev)
{
	wakeup(DNADDR);
}

#
/*
 * DN-11 ACU interface
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"

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
	struct dn *dp;
	register struct dn *rdp;
	int c;

	dp = &DNADDR->dn11[dev.d_minor];
	for(;;) {
		while (((rdp = dp)->dn_stat&DONE)==0)
			sleep(DNADDR, DNPRI);
		rdp->dn_stat =& ~DONE;
		if (rdp->dn_reg&(PWI|ACR)) {
			u.u_error = EIO;
			return;
		}
		if (rdp->dn_stat&DSS)
			return;
		if (u.u_count==0 || (rdp->dn_stat&PND)==0 || cpass(&c)<0)
			continue;
		rdp = dp;
		rdp->dn_reg = c-'0';
		rdp->dn_stat =| DPR;
	}
}

dnint(dev)
{
	wakeup(DNADDR);
}

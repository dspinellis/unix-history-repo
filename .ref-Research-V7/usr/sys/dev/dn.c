/*
 * DN-11 ACU interface
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"

struct device {
	int	dn_reg[4];
};

struct device *dn_addr[] = { (struct device *)0175200 };
#define	NDN	4

#define	PWI	0100000
#define	ACR	040000
#define	DLO	010000
#define	DONE	0200
#define	IENABLE	0100
#define	DSS	040
#define	PND	020
#define	MENABLE	04
#define	DPR	02
#define	CRQ	01

#define	DNPRI	(PZERO+5)

dnopen(dev)
register dev;
{
	register struct device *dp;

	dev = minor(dev);
	if (dev >= NDN ||
	   (dp = dn_addr[dev>>2])->dn_reg[dev&03]&(PWI|DLO|CRQ))
		u.u_error = ENXIO;
	else {
		dp->dn_reg[0] |= MENABLE;
		dp->dn_reg[dev&03] = IENABLE|MENABLE|CRQ;
	}
}

dnclose(dev)
{
	dev = minor(dev);
	dn_addr[dev>>2]->dn_reg[dev&03] = MENABLE;
}

dnwrite(dev)
{
	register c;
	register *dp;
	extern lbolt;

	dev = minor(dev);
	dp = &(dn_addr[dev>>2]->dn_reg[dev&03]);
	while ((*dp & (PWI|ACR|DSS)) == 0) {
		spl4();
		if ((*dp&PND) == 0 || u.u_count == 0 || (c=cpass()) < 0)
			sleep((caddr_t)dp, DNPRI);
		else if (c == '-') {
			sleep((caddr_t)&lbolt, DNPRI);
			sleep((caddr_t)&lbolt, DNPRI);
		} else {
			*dp = (c<<8)|IENABLE|MENABLE|DPR|CRQ;
			sleep((caddr_t)dp, DNPRI);
		}
		spl0();
	}
	if (*dp&(PWI|ACR))
		u.u_error = EIO;
}

/*
 * interrupt-- "dev" applies to
 * system unit number, not minor device
 */
dnint(dev)
{
	register *dp,*ep;

	dp = &(dn_addr[dev]->dn_reg[0]);
	*dp &= ~MENABLE;
	for (ep=dp; ep<dp+4; ep++)
		if (*ep&DONE) {
			*ep &= ~DONE;
			wakeup((caddr_t)ep);
		}
	*dp |= MENABLE;
}

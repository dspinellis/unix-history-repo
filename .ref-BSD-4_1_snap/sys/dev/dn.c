/*	dn.c	4.4	82/01/12	*/

#include "dn.h"
#if NDN > 0
/*
 * DN-11 ACU interface
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/ubavar.h"
#include "../h/conf.h"
#include "../h/ioctl.h"

struct dndevice {
	u_short	dn_reg[4];
};

struct uba_device *dninfo[NDN];
int dnprobe(), dnattach();
u_short dnstd[] = { 0175200 };
struct uba_driver dndriver =
	{ dnprobe, 0, dnattach, 0, dnstd, "dn", dninfo };

#define	CRQ	0x001		/* call request */
#define	DPR	0x002		/* digit present */
#define	MENABLE	0x004		/* master enable */
#define MAINT	0x008		/* maintenance mode */
#define	PND	0x010		/* present next digit */
#define	DSS	0x020		/* data set status */
#define	IENABLE	0x040		/* interrupt enable */
#define	DONE	0x080		/* operation complete */
#define	DLO	0x1000		/* data line occupied */
#define	ACR	0x4000		/* abandon call and retry */
#define	PWI	0x8000		/* power indicate */

#define	DNPRI	(PZERO+5)
#define DNUNIT(dev)	(minor(dev)>>2)
#define DNREG(dev)	((dev)&03)

#define OBUFSIZ	40		/* largest phone # dialer can handle */

/*
 * There's no good way to determine the correct number of dialers attached
 * to a single device (especially when dialers such as Vadic-821 MACS
 * exist which can address four chassis, each with its own dialer).
 */
dnprobe(reg)
	caddr_t reg;
{
	register int br, cvec;	/* value-result, must be r11, r10 */
	register struct dndevice *dnaddr = (struct dndevice *)reg;

	/*
	 * If there's at least one dialer out there it better be
	 *  at chassis 0.
	 */
	dnaddr->dn_reg[0] = MENABLE|IENABLE|DONE;
	DELAY(5);
	dnaddr->dn_reg[0] = 0;
}

dnattach(ui)
	struct uba_device *ui;
{}

/*ARGSUSED*/
dnopen(dev, flag)
	dev_t dev;
{
	register struct dndevice *dp;
	register u_short unit, *dnreg;
	register struct uba_device *ui;
	register short dialer;

	if ((unit = DNUNIT(dev)) >= NDN || (ui = dninfo[unit]) == 0 ||
	    ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	dialer = DNREG(dev);
	dp = (struct dndevice *)ui->ui_addr;
	if (dp->dn_reg[dialer] & PWI) {
		u.u_error = ENXIO;
		return;
	}
	dnreg = &(dp->dn_reg[dialer]);
	if (*dnreg&(DLO|CRQ)) {
		u.u_error = EBUSY;
		return;
	}
	dp->dn_reg[0] |= MENABLE;
	*dnreg = IENABLE|MENABLE|CRQ;
}

/*ARGSUSED*/
dnclose(dev, flag)
	dev_t dev;
{
	register struct dndevice *dp;

	dp = (struct dndevice *)dninfo[DNUNIT(dev)]->ui_addr;
	dp->dn_reg[DNREG(dev)] = MENABLE;
}

dnwrite(dev)
	dev_t dev;
{
	register u_short *dnreg;
	register int cc;
	register struct dndevice *dp;
	char buf[OBUFSIZ];
	register char *cp;
	extern lbolt;

	dp = (struct dndevice *)dninfo[DNUNIT(dev)]->ui_addr;
	dnreg = &(dp->dn_reg[DNREG(dev)]);
	cc = MIN(u.u_count, OBUFSIZ);
	cp = buf;
	iomove(cp, (unsigned)cc, B_WRITE);
	if (u.u_error)
		return;
	while ((*dnreg & (PWI|ACR|DSS)) == 0 && cc >= 0) {
		spl4();
		if ((*dnreg & PND) == 0 || cc == 0)
			sleep((caddr_t)dnreg, DNPRI);
		else switch(*cp) {
		
		case '-':
			sleep((caddr_t)&lbolt, DNPRI);
			sleep((caddr_t)&lbolt, DNPRI);
			break;

		case 'f':
			*dnreg &= ~CRQ;
			sleep((caddr_t)&lbolt, DNPRI);
			*dnreg |= CRQ;
			break;

		case '*': case ':':
			*cp = 012;
			goto dial;

		case '#': case ';':
			*cp = 013;
			goto dial;

		case 'e': case '<':
			*cp = 014;
			goto dial;

		case 'w': case '=':
			*cp = 015;
			goto dial;

		default:
			if (*cp < '0' || *cp > '9')
				break;
		dial:
			*dnreg = (*cp << 8) | (IENABLE|MENABLE|DPR|CRQ);
			sleep((caddr_t)dnreg, DNPRI);
		}
		cp++, cc--;
		spl0();
	}
	if (*dnreg & (PWI|ACR))
		u.u_error = EIO;
}

dnintr(dev)
	dev_t dev;
{
	register u_short *basereg, *dnreg;

	basereg = (u_short *)dninfo[dev]->ui_addr;
	*basereg &= ~MENABLE;
	for (dnreg = basereg; dnreg < basereg + 4; dnreg++)
		if (*dnreg & DONE) {
			*dnreg &= ~(DONE|DPR);
			wakeup((caddr_t)dnreg);
		}
	*basereg |= MENABLE;
}
#endif

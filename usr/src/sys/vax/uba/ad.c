/*	ad.c	4.2	82/06/26	*/

#include "ad.h"
#if NAD > 0
/*
 * Data translation AD converter interface -- Bill Reeves
 */
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/adreg.h"

#define ADBUSY 01
#define ADWAITPRI (PZERO+1)

int adprobe(), adattach();
struct uba_device *addinfo[NAD];
u_short adstd[] = { 0770400, 0000000, 0 };
struct uba_driver addriver =
	{ adprobe, 0, adattach, 0, adstd, "ad", addinfo, 0, 0 };

struct ad {
	char	ad_open;
	short int ad_uid;
	short int ad_state;
	short int ad_softcsr;
	short int ad_softdata;
	short int ad_chan;
	int	ad_icnt;
	int	ad_loop;
} ad[NAD];

#define ADUNIT(dev) (minor(dev))

adprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct addevice *adaddr = (struct addevice *) reg;

	adaddr->ad_csr = AD_IENABLE | AD_START;
	DELAY(40000);
	adaddr->ad_csr = 0;
}

adattach(ui)
	struct uba_device *ui;
{
}

adopen(dev)
	dev_t dev;
{
	register struct ad *adp;
	register struct uba_device *ui;

	if(ADUNIT(dev) >= NAD || (adp = &ad[ADUNIT(dev)])->ad_open ||
		(ui = addinfo[ADUNIT(dev)]) == 0 || ui->ui_alive == 0) {
			u.u_error = ENXIO;
			return;
	}
	adp->ad_open = 1;
	adp->ad_icnt = 0;
	adp->ad_state = 0;
	adp->ad_uid = u.u_uid;
}

adclose(dev)
	dev_t dev;
{

	ad[ADUNIT(dev)].ad_open = 0;
	ad[ADUNIT(dev)].ad_state = 0;
}

/*ARGSUSED*/
adioctl(dev, cmd, addr, flag)
	dev_t dev;
	register caddr_t addr;
{
	register struct addevice *adaddr =
			(struct addevice *) addinfo[ADUNIT(dev)]->ui_addr;
	register struct uba_device *ui = addinfo[ADUNIT(dev)];
	register struct ad *adp;
	register int i;
	short int chan;

	switch (cmd) {
	case AD_CHAN:
		adp = &ad[ADUNIT(dev)];
		chan  = fuword(addr);
		if(chan == -1)
			u.u_error = EFAULT;
		else
			adp->ad_chan = chan<<8;
		break;
	case AD_READ:
		adp = &ad[ADUNIT(dev)];
		spl6();
		adaddr->ad_csr = adp->ad_chan;
		i = 1000;
		while(i-- > 0 && (adaddr->ad_csr&037400) != adp->ad_chan) {
			adp->ad_loop++;
			adaddr->ad_csr = adp->ad_chan;
		}
		adp->ad_state |= ADBUSY;
		adaddr->ad_csr |= AD_IENABLE|AD_START;
		while(adp->ad_state&ADBUSY)
			sleep((caddr_t)adp, ADWAITPRI);
		spl0();
		(void) suword(addr, adp->ad_softdata);
		break;
	default:
		u.u_error = ENOTTY;	/* Not a legal ioctl cmd. */
	}
}

/*ARGSUSED*/
adintr(dev)
	dev_t dev;
{
	register struct addevice *adaddr =
			(struct addevice *) addinfo[ADUNIT(dev)]->ui_addr;
	register struct ad *adp = &ad[ADUNIT(dev)];

	adp->ad_icnt++;
	adp->ad_softcsr = adaddr->ad_csr;
	adp->ad_softdata = adaddr->ad_data;
	if(adp->ad_state&ADBUSY) {
		adp->ad_state &= ~ADBUSY;
		wakeup((caddr_t)adp);
	}
}

adreset(uban)
	int uban;
{
	register int i;
	register struct uba_device *ui;
	register struct ad *adp = ad;
	register struct addevice *adaddr;

	for(i = 0; i < NAD; i++, adp++) {
		if((ui = addinfo[i]) == 0 || ui->ui_alive == 0 ||
				ui->ui_ubanum != uban || adp->ad_open == 0)
			continue;
		printf(" ad%d", i);
		if(adp->ad_state&ADBUSY == 0)
			continue;
		adaddr = (struct addevice *) ui->ui_addr;
		adaddr->ad_csr = 0;
		adaddr->ad_csr = adp->ad_chan|AD_IENABLE|AD_START;
	}
}
#endif

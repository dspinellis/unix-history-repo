#ifndef __DKCTLR_H			/* TUB */
#define __DKCTLR_H
#define MAXCHNO		256

struct dkctlr {
	struct device	*addr;
	struct ptr	*ptr;		/* controller dependent */
	unsigned	maxchno;
	struct dkmodule	*module;
	struct chan	*txq, *txqe;
	struct chan	*curchan;
	mblk_t		*rxbp;
	unsigned char	state[MAXCHNO];
	struct chan {
		unsigned char	exclusive;
		unsigned char	txbusy;
		unsigned char	buffer;
		unsigned	chno;
		queue_t		*q;
		struct dkctlr	*ctlr;
		struct chan	*txql;
		mblk_t		*curmp;
	} chan[MAXCHNO];
};

struct dkctlrstat {			/* should be in dkctlr */
	unsigned	rintr;
	unsigned	extrab;
	unsigned	blocked;
	unsigned	noallocb;
	unsigned	noqueue;
	unsigned	xintr;
	unsigned	spin[8];
};
#endif __DKCTLR_H

/*
 * datakit status
 */

struct dkstat {
	long	input;
	long	output;
	int	markflt;		/* mark byte expected */
	int	markparity;		/* parity error on mark byte */
	int	closepack;		/* packet on closed channel */
	int	pack0;			/* packet on channel 0 */
	int	packstrange;		/* packet on strange channel */
	int	shortpack;		/* short packet */
	int	parity;			/* parity error */
	int	chgstrange;		/* T_CHG on strange channel */
	int	notclosed;		/* packets on hung-up chans */
	int	isclosed;		/* "isclosed" packet on open chan */
	int	dkprxmit;		/* retransmit rejected dkp msg */
	int	dkprjtrs;		/* reject, trailer size */
	int	dkprjpks;		/* reject, packet size */
	int	dkprjseq;		/* reject, sequence number */
};

#define crc	shortpack
#define oflow	parity

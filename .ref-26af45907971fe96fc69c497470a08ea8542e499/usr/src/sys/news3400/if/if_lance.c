/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: if_lance.c,v 4.300 91/06/09 06:25:58 root Rel41 $ SONY
 *
 *	@(#)if_lance.c	7.4 (Berkeley) %G%
 */

/*
 * if_lance: Am7990 LANCE driver
 *
 * This driver is available only for single CPU machine.
 */

#define	LANCE_LED

#include "en.h"

#if NEN > 0

#include <machine/adrsmap.h>

#include <sys/param.h>

#include <news3400/if/lancereg.h>
#include <news3400/if/if_lance.h>

#ifdef mips
#define	VOLATILE	volatile
#else
#define	VOLATILE
#endif

#ifdef LANCE_LED
#ifdef news3400
#define	LED_ON	{ \
	VOLATILE u_char *p = (u_char *)DEBUG_PORT; \
	*p = DP_WRITE | (*p & ~DP_LED2); \
}
#define	LED_OFF	{ \
	VOLATILE u_char *p = (u_char *)DEBUG_PORT; \
	*p = DP_WRITE | (*p | DP_LED2); \
}
#else /* news3400 */
#define	LED_ON
#define	LED_OFF
#endif /* news3400 */
#else /* LANCE_LED */
#define	LED_ON
#define	LED_OFF
#endif /* LANCE_LED */

/*
 *	LANCE memory configuration
 */
#define	INIT_BLOCK		0x000000
#define	RECV_MSG_DESC		0x000100
#define	XMIT_MSG_DESC		0x000200
#ifdef mips
#define	RECV_BUFFER		(0x000300 + 2)
				/* for data alignment to long word */
#else /* mips */
#define	RECV_BUFFER		0x000300
#endif /* mips */
#define	XMIT_BUFFER		(RECV_BUFFER+(RECV_BUFFER_SIZE*RECV_BUFFERS))

#define	RECV_RING_LEN		3	/* log2(8) */
#define	XMIT_RING_LEN		1	/* log2(2) */

#define	RECV_BUFFER_SIZE	0x600
#define	XMIT_BUFFER_SIZE	0x600

#define	RECV_BUFFERS		(1 << RECV_RING_LEN)
#define	XMIT_BUFFERS		(1 << XMIT_RING_LEN)

/*
 *	Initialization block
 */
struct init_block init_block = {
	0,
	0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0,
	RECV_MSG_DESC & 0xffff,
	(RECV_RING_LEN << 13) | (RECV_MSG_DESC >> 16),
	XMIT_MSG_DESC & 0xffff,
	(XMIT_RING_LEN << 13) | (XMIT_MSG_DESC >> 16)
};

/*
 *	LANCE control block
 */
Lance_chan lancesw[NEN] = {
      {	(Lance_reg *)LANCE_PORT,
	(caddr_t)LANCE_MEMORY,
	(caddr_t)ETHER_ID },
#if NEN > 1
      {	(Lance_reg *)LANCE_PORT1,
	(caddr_t)LANCE_MEMORY1,
	(caddr_t)ETHER_ID1 },
#endif
#if NEN > 2
      {	(Lance_reg *)LANCE_PORT2,
	(caddr_t)LANCE_MEMORY2,
	(caddr_t)ETHER_ID2 },
#endif
};

lance_probe(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];
	VOLATILE int *p = (VOLATILE int *)lance->lance_memory;

	if (badaddr(lance->lance_addr, 1))
		return (0);

	*p = 0x12345678;

	return (*p == 0x12345678);
}

lance_open(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];
	register recv_msg_desc *rmd;
	register xmit_msg_desc *tmd;
	register struct init_block *ib;
	register int buffer, i;

	if ((lance->lance_flags & LANCE_ACTIVE) == 0) {
		if (lance->lance_addr == (Lance_reg *)0)
			return (-1);

		lance_write_reg(chan, CSR0, CSR_STOP);

		rmd = (recv_msg_desc *)
		    (RECV_MSG_DESC + lance->lance_memory);
		lance->lance_last_rmd =
		    (lance->lance_rmd = rmd) + RECV_BUFFERS - 1;
		buffer = RECV_BUFFER;
		for (i = 0; i < RECV_BUFFERS; i++) {
			rmd->rmd_ladr = buffer & 0xffff;
			rmd->rmd_stat = RMD_OWN | (buffer >> 16);
			rmd->rmd_bcnt = -RECV_BUFFER_SIZE;
			rmd->rmd_mcnt = 0;
			rmd++;
			buffer += RECV_BUFFER_SIZE;
		}

		tmd = (xmit_msg_desc *)
		    (XMIT_MSG_DESC + lance->lance_memory);
		lance->lance_last_tmd =
			(lance->lance_tmd = tmd) + XMIT_BUFFERS - 1;
		buffer = XMIT_BUFFER;
		for (i = 0; i < XMIT_BUFFERS; i++) {
			tmd->tmd_ladr = buffer & 0xffff;
			tmd->tmd_stat = buffer >> 16;
			tmd->tmd_bcnt = 0;
			tmd++;
			buffer += XMIT_BUFFER_SIZE;
		}
		get_hard_addr(chan, lance->lance_stats.ens_addr);

		ib = (struct init_block *)(INIT_BLOCK + lance->lance_memory);
		lance->lance_ib = ib;
		*ib = init_block;
		ib->ib_padr[0] = lance->lance_stats.ens_addr[1];
		ib->ib_padr[1] = lance->lance_stats.ens_addr[0];
		ib->ib_padr[2] = lance->lance_stats.ens_addr[3];
		ib->ib_padr[3] = lance->lance_stats.ens_addr[2];
		ib->ib_padr[4] = lance->lance_stats.ens_addr[5];
		ib->ib_padr[5] = lance->lance_stats.ens_addr[4];

		if (lance->lance_flags & LANCE_PROM)
			ib->ib_mode |= IB_PROM;

		lance->lance_flags |= LANCE_ACTIVE;
		lance_init(chan);

		if (lance_read_reg(chan, CSR0) !=
		    (CSR_INEA|CSR_RXON|CSR_TXON|CSR_STRT|CSR_INIT)) {
			lance->lance_flags &= ~LANCE_ACTIVE;
			return (-1);
		}

	}

	return (0);
}

lance_close(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];

	lance_write_reg(chan, CSR0, CSR_STOP);
	lance->lance_flags &= ~LANCE_ACTIVE;

}

#define	RECEIVE(lance, rmd)	{ \
	register int i; \
	(rmd) = (lance)->lance_last_rmd + 1; \
	if ((rmd) >= (lance)->lance_rmd + RECV_BUFFERS) \
		(rmd) = (lance)->lance_rmd; \
	if (((rmd)->rmd_stat & RMD_OWN) == 0) \
		(lance)->lance_last_rmd = (rmd); \
	else \
		(rmd) = NULL; \
}
#define	RECV_BUF(lance, rmd)	(char *)((rmd)->rmd_ladr \
					+ (((rmd)->rmd_stat & RMD_HADR) << 16) \
					+ (lance)->lance_memory)
#define	RECV_CNT(rmd)		((rmd)->rmd_mcnt - 4)
#define	RECV_ERR(rmd)		((rmd)->rmd_stat & RMD_ERR)
#define	RELEASE_RECV_BUF(rmd)	{ \
	(rmd)->rmd_mcnt = 0; \
	(rmd)->rmd_stat = ((rmd)->rmd_stat & RMD_HADR) | RMD_OWN; \
}


caddr_t
get_recv_buffer(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];
	register recv_msg_desc *rmd;

next:
	RECEIVE(lance, rmd);
	if (rmd == NULL)
		return (NULL);

	if (RECV_ERR(rmd)) {
		recv_error(lance, rmd);
		RELEASE_RECV_BUF(rmd);
		goto next;
	}

	return (RECV_BUF(lance, rmd));
}

get_recv_length(chan)
	int chan;
{

	return (RECV_CNT(lancesw[chan].lance_last_rmd));
}

free_recv_buffer(chan)
	int chan;
{
	register recv_msg_desc *rmd = lancesw[chan].lance_last_rmd;

	RELEASE_RECV_BUF(rmd);
}

#define	GET_XMIT_BUF(lance, tmd)	{ \
	(tmd) = (lance)->lance_last_tmd + 1; \
	if ((tmd) >= (lance)->lance_tmd + XMIT_BUFFERS) \
		(tmd) = (lance)->lance_tmd; \
	if ((tmd)->tmd_stat & TMD_OWN) \
		(tmd) = NULL; \
	else \
		(lance)->lance_last_tmd = (tmd); \
}
#define	XMIT_BUF(lance, tmd)	(char *)((tmd)->tmd_ladr \
					+ (((tmd)->tmd_stat & TMD_HADR) << 16) \
					+ (lance)->lance_memory)
#define	XMIT_ERR(tmd)		((tmd)->tmd_stat & TMD_ERR)
#define	TRANSMIT(lance, tmd, count)	{ \
	(tmd)->tmd_bcnt = -(count); \
	(tmd)->tmd_error = 0; \
	(tmd)->tmd_stat = ((tmd)->tmd_stat & TMD_HADR) | (TMD_OWN|TMD_STP|TMD_ENP); \
	(lance)->lance_addr->rap = CSR0; \
	(lance)->lance_addr->rdp = (CSR_INEA|CSR_TDMD); \
}

caddr_t
get_xmit_buffer(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];
	register xmit_msg_desc *tmd;

	GET_XMIT_BUF(lance, tmd);
	if (tmd == NULL)
		return (NULL);
	return (XMIT_BUF(lance, tmd));
}

lance_transmit(chan, count)
	int chan;
	int count;
{
	register Lance_chan *lance = &lancesw[chan];
	register xmit_msg_desc *tmd;

	tmd = lance->lance_last_tmd;
	TRANSMIT(lance, tmd, count);
}

lance_xmit_error(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];
	register xmit_msg_desc *tmd;

	tmd = lance->lance_last_tmd;
	if (XMIT_ERR(tmd)) {
		xmit_error(lance, tmd);
		return (1);
	}

	return (0);
}

lance_collision(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];

	if (lance->lance_last_tmd->tmd_stat & (TMD_MORE|TMD_ONE)) {
		lance->lance_stats.ens_collis++;
		return (1);
	}

	return (0);
}

lance_get_addr(chan, addr)
	int chan;
	caddr_t addr;
{
	register Lance_chan *lance = &lancesw[chan];

	bcopy(lance->lance_stats.ens_addr, addr,
		sizeof(lance->lance_stats.ens_addr));
}

lance_prom_mode(chan, cmd)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];

	lance_close(chan);
	if (cmd)
		lance->lance_flags |= LANCE_PROM;
	else
		lance->lance_flags &= ~LANCE_PROM;
	lance_open(chan);
}

lance_get_status(chan, addr)
	int chan;
	caddr_t addr;
{
	register Lance_chan *lance = &lancesw[chan];
	register int s;

	s = splimp();
	bcopy(&lance->lance_stats.ens_frames, addr,
	    sizeof(lance->lance_stats) - sizeof (lance->lance_stats.ens_addr));
	bzero(&lance->lance_stats.ens_frames,
	    sizeof(lance->lance_stats) - sizeof (lance->lance_stats.ens_addr));
	(void) splx(s);
}

lance_intr()
{
	register Lance_chan *lance;
	register Lance_reg *reg;
	register int stat, chan;
	int retval = 0;

	LED_ON;

	for (chan = 0, lance = lancesw; chan < NEN ; lance++, chan++) {
		if ((lance->lance_flags & LANCE_ACTIVE) == 0)
			continue;

		reg = lance->lance_addr;
		reg->rap = CSR0;
		stat = reg->rdp & ~CSR_INEA;
		if ((stat & CSR_INTR) == 0)
			continue;

		retval = 1;
		reg->rdp = stat;
		reg->rdp = CSR_INEA;

		if (stat & CSR_ERR) {
			if (stat & CSR_BABL)
				printf("lance %d error: babl\n", chan);
			if (stat & CSR_MISS)
				lance->lance_stats.ens_lost++;
			if (stat & CSR_MERR)
				printf("lance %d error: merr\n", chan);
		}
		if (stat & CSR_RINT) {
			lance->lance_stats.ens_frames++;
			enrint(chan);
		}
		if (stat & CSR_TINT) {
			lance->lance_stats.ens_xmit++;
			enxint(chan);
		}
		if (stat & CSR_IDON)
			lance->lance_flags |= LANCE_IDON;
	}

	LED_OFF;

	return (retval);
}

lance_init(chan)
	int chan;
{
	register Lance_chan *lance = &lancesw[chan];
	register int s;

	s = splimp();
	lance_write_reg(chan, CSR1, INIT_BLOCK & 0xffff);
	lance_write_reg(chan, CSR2, INIT_BLOCK >> 16);
	lance_write_reg(chan, CSR3, CSR_BSWP|CSR_BCON);

	lance_write_reg(chan, CSR0, CSR_INEA|CSR_STRT|CSR_INIT);
	(void) splx(s);

	while ((lance->lance_flags & LANCE_IDON) == 0)
		;
}

recv_error(lance, rmd)
	register Lance_chan *lance;
	register recv_msg_desc *rmd;
{
	register int status = rmd->rmd_stat;
	register int chan = lance - lancesw;

	if (status & RMD_FRAM)
		lance->lance_stats.ens_align++;
	if (status & RMD_OFLO)
		printf("lance %d recv error: overflow\n", chan);
	if (status & RMD_CRC)
		lance->lance_stats.ens_crc++;
	if (status & RMD_BUFF)
		printf("lance %d:recv error: buffer\n", chan);
}

xmit_error(lance, tmd)
	register Lance_chan *lance;
	register xmit_msg_desc *tmd;
{
	register int status = tmd->tmd_error;
	register int chan = lance - lancesw;

	if (status & TMD_BUFF)
		printf("lance %d: xmit error: buffer\n", chan);
	if (status & TMD_UFLO)
		printf("lance %d: xmit error: underflow\n", chan);
	if (status & TMD_LCOL) {
		printf("lance %d: xmit error: late collision\n", chan);
		lance->lance_stats.ens_owcollis++;
	}
	if (status & TMD_LCAR)
		printf("lance %d: xmit error: loss of carrier\n", chan);
	if (status & TMD_RTRY) {
		printf("lance %d: xmit error: retry tdr=%d\n",
		    chan, status & TMD_TDR);
		lance->lance_stats.ens_xcollis++;
	}
}

lance_write_reg(chan, reg, data)
	int chan, reg, data;
{
	register Lance_reg *lance = lancesw[chan].lance_addr;
	register int s;

	s = spl7();
	lance->rap = reg;
	lance->rdp = data;
	(void) splx(s);
}

lance_read_reg(chan, reg)
	int chan, reg;
{
	register Lance_reg *lance = lancesw[chan].lance_addr;
	register int s, d;

	s = spl7();
	lance->rap = reg;
	d = lance->rdp;
	(void) splx(s);

	return (d);
}

get_hard_addr(chan, addr)
	int chan;
	u_short *addr;
{
	register unsigned char *p, *q;
	register int i;
	register Lance_chan *lance = &lancesw[chan];
	unsigned char hard_addr[6];

	p = (unsigned char *)lance->lance_rom + 16;
	q = hard_addr;
	for (i = 0; i < 6; i++) {
		*q = (*p++ & 0xf) << 4;
		*q++ |= *p++ & 0xf;
	}
	
	bcopy(hard_addr, (char *)addr, 6);
}

#if defined(mips) && defined(CPU_SINGLE)
bxcopy(s, d, n)
	caddr_t s, d;
	int n;
{

	if (n <= 0)
		return;
	switch ((((int)s & 03) << 2) + ((int)d & 03)) {

	case 0x0:
		blcopy((long *)s, (long *)d, n);
		return;

	case 0x5:
		*(char *)d = *(char *)s;
		blcopy((long *)(s + 1), (long *)(d + 1), n - 1);
		return;

	case 0xa:
		switch (n) {

		case 1:
			*(char *)d = *(char *)s;
			return;

		case 2:
			*(short *)d = *(short *)s;
			return;

		default:
			*(short *)d = *(short *)s;
			blcopy((long *)(s + 2), (long *)(d + 2), n - 2);
			return;
		}

	case 0xf:
		switch (n) {

		case 1:
			*(char *)d = *(char *)s;
			return;

		case 2:
			*(char *)d = *(char *)s;
			*(char *)(d + 1) = *(char *)(s + 1);
			return;

		case 3:
			*(char *)d = *(char *)s;
			*(short *)(d + 1) = *(short *)(s + 1);
			return;

		default:
			*(char *)d = *(char *)s;
			*(short *)(d + 1) = *(short *)(s + 1);
			blcopy((long *)(s + 3), (long *)(d + 3), n - 3);
			return;
		}

	case 0x7:
	case 0xd:
		switch (n) {

		case 1:
			*(char *)d = *(char *)s;
			return;

		case 2:
			*(char *)d = *(char *)s;
			*(char *)(d + 1) = *(char *)(s + 1);
			return;

		default:
			*(char *)d = *(char *)s;
			bwcopy((short *)(s + 1), (short *)(d + 1), n);
			return;
		}

	case 0x2:
	case 0x8:
		bwcopy((short *)s, (short *)d, n);
		return;

	default:
		bbcopy((char *)s, (char *)d, n);
		return;
	}
}

#define	COPY(s, d, n, t) \
	while ((n) >= 8 * sizeof (t)) { \
		int t0, t1, t2, t3, t4, t5, t6, t7; \
		t0 = (s)[0]; \
		t1 = (s)[1]; \
		t2 = (s)[2]; \
		t3 = (s)[3]; \
		t4 = (s)[4]; \
		t5 = (s)[5]; \
		t6 = (s)[6]; \
		t7 = (s)[7]; \
		(d)[0] = t0; \
		(d)[1] = t1; \
		(d)[2] = t2; \
		(d)[3] = t3; \
		(d)[4] = t4; \
		(d)[5] = t5; \
		(d)[6] = t6; \
		(d)[7] = t7; \
		(s) += 8; \
		(d) += 8; \
		(n) -= 8 * sizeof (t); \
	} \
	while ((n) >= sizeof (t)) { \
		(d)[0] = (s)[0]; \
		(s)++; \
		(d)++; \
		(n) -= sizeof (t); \
	}

blcopy(s, d, n)
	long *s, *d;
	int n;
{

	COPY(s, d, n, long);
	switch (n) {

	case 0:
		return;

	case 1:
		*(char *)d = *(char *)s;
		return;

	case 2:
		*(short *)d = *(short *)s;
		return;

	case 3:
		*(short *)d = *(short *)s;
		*((char *)d + 2) = *((char *)s + 2);
		return;
	}
}

bwcopy(s, d, n)
	short *s, *d;
	int n;
{

	COPY(s, d, n, short);
	if (n == 1)
		*(char *)d = *(char *)s;
}

bbcopy(s, d, n)
	char *s, *d;
	int n;
{

	COPY(s, d, n, char);
}
#endif /* defined(mips) && defined(CPU_SINGLE) */

#endif /* NEN > 0 */

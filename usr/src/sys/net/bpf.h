/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that: (1) source code distributions
 * retain the above copyright notice and this paragraph in its entirety, (2)
 * distributions including binary code include the above copyright notice and
 * this paragraph in its entirety in the documentation or other materials
 * provided with the distribution, and (3) all advertising materials mentioning
 * features or use of this software display the following acknowledgement:
 * ``This product includes software developed by the University of California,
 * Lawrence Berkeley Laboratory and its contributors.'' Neither the name of
 * the University nor the names of its contributors may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * @(#) $Header: bpf.h,v 1.19 91/01/30 18:20:21 mccanne Exp $ (LBL)
 *
 * This code is derived from the Stanford/CMU enet packet filter,
 * (net/enet.h) distributed with 4.3BSD Unix.
 */

/*
 * Alignment macros.  BPF_WORDALIGN rounds up to the next 
 * even multiple of BPF_ALIGNMENT. 
 */
#define BPF_ALIGNMENT sizeof(long)
#define BPF_WORDALIGN(x) (((x)+(BPF_ALIGNMENT-1))&~(BPF_ALIGNMENT-1))

/*
 *  Structure for BIOCSETF.
 */
struct bpf_program {
	u_int bf_len;
	struct bpf_insn *bf_insns;
};
 
/*
 * Struct returned by BIOCGSTATS.
 */
struct bpf_stat {
	u_int bs_recv;		/* number of packets received */
	u_int bs_drop;		/* number of packets dropped */
};

/*
 * BPF ioctls
 *
 * The first set is for compatibility with Sun's pcc style
 * header files.  If your using gcc, we assume that you
 * have run fixincludes so the latter set should work.
 */
#if defined(sun) && !defined(__GNUC__)
#define	BIOCGFLEN	_IOR(B,101, u_int)
#define	BIOCGBLEN	_IOR(B,102, u_int)
#define	BIOCSETF	_IOW(B,103, struct bpf_program)
#define	BIOCFLUSH	_IO(B,104)
#define BIOCPROMISC	_IO(B,105)
#define	BIOCDEVP	_IOR(B,106, struct bpf_devp)
#define BIOCGETIF	_IOR(B,107, struct ifreq)
#define BIOCSETIF	_IOW(B,108, struct ifreq)
#define BIOCSRTIMEOUT	_IOW(B,109, struct timeval)
#define BIOCGRTIMEOUT	_IOR(B,110, struct timeval)
#define BIOCGSTATS	_IOR(B,111, struct bpf_stat)
#define BIOCIMMEDIATE	_IOW(B,112, u_int)
#else
#define	BIOCGFLEN	_IOR('B',101, u_int)
#define	BIOCGBLEN	_IOR('B',102, u_int)
#define	BIOCSETF	_IOW('B',103, struct bpf_program)
#define	BIOCFLUSH	_IO('B',104)
#define BIOCPROMISC	_IO('B',105)
#define	BIOCDEVP	_IOR('B',106, struct bpf_devp)
#define BIOCGETIF	_IOR('B',107, struct ifreq)
#define BIOCSETIF	_IOW('B',108, struct ifreq)
#define BIOCSRTIMEOUT	_IOW('B',109, struct timeval)
#define BIOCGRTIMEOUT	_IOR('B',110, struct timeval)
#define BIOCGSTATS	_IOR('B',111, struct bpf_stat)
#define BIOCIMMEDIATE	_IOW('B',112, u_int)
#endif

/*
 * The device parameters of a network interface.
 */
struct bpf_devp {
	u_short	bdev_type;	/* data link layer type, codes below */
	u_short	bdev_hdrlen;	/* length of a hardware packet header */
};

/*
 * Structure prepended to each packet.
 */
struct bpf_hdr {
	struct timeval	bh_tstamp;	/* time stamp */
	u_long		bh_caplen;	/* length of captured portion */
	u_long		bh_datalen;	/* original length of packet */
	u_short		bh_hdrlen;	/* length of bpf header (this struct
					   plus alignment padding) */
};
/*
 * Because the structure above is not a multiple of 4 bytes, some compilers
 * will insist on inserting padding; hence, sizeof(struct bpf_hdr) won't work.
 * Only the kernel needs to know about it; applications use bh_hdrlen.
 */
#ifdef KERNEL
#define SIZEOF_BPF_HDR 18
#endif

/*
 * Data-link level type codes.
 * Currently, only ENDT_10MB and DLT_SLIP are supported.
 */
#define DLT_EN10MB	1	/* Ethernet (10Mb) */
#define DLT_EN3MB	2	/* Experimental Ethernet (3Mb) */
#define DLT_AX25	3	/* Amateur Radio AX.25 */
#define DLT_PRONET	4	/* Proteon ProNET Token Ring */
#define DLT_CHAOS	5	/* Chaos */
#define DLT_IEEE802	6	/* IEEE 802 Networks */
#define DLT_ARCNET	7	/* ARCNET */
#define DLT_SLIP	8	/* Serial Line IP */
#define DLT_PPP		9	/* Point-to-point Protocol */
#define DLT_FDDI	10	/* FDDI */

/*
 * The opcodes are defined as an enumeration.  However, they are stored
 * explicitly in the code array as 'u_short'.
 */
enum bpf_code {
#define OPDEF(opcode, opstr) opcode ,
#include <net/bpfcodes.h>
#undef OPDEF
	/* this idea is borrowed from gcc */
	LAST_AND_UNUSED_ENUM
};

#define BPF_NCODES ((unsigned)LAST_AND_UNUSED_ENUM)
#define BPF_VALIDCODE(code) ((unsigned)(code) < BPF_NCODES)

/*
 * The instruction data structure.
 */
struct bpf_insn {
	u_short	code;
	u_char 	jt;
	u_char 	jf;
	long	k;
};

/*
 * Macros for array initializers.
 */
#define BPF_STMT(code, k) { (u_short)code, 0, 0, k }
#define BPF_JUMP(code, k, jt, jf) { (u_short)code, jt, jf, k }

#ifdef KERNEL
extern u_int bpf_filter();
extern void bpfattach();
extern void bpf_tap();
extern void bpf_mtap();
#endif

/*
 * These two macros are sensitive to the order in which the
 * opcodes appear in bpfcodes.h.
 */
#define BPF_ISJUMP(code) ((unsigned)(code) <= (unsigned)EQOp)
#define BPF_ISLEAF(code) ((unsigned)(code) >= (unsigned)RetOp)

/*
 * Number of scratch memory words.
 */
#define BPF_MEMWORDS 16


/*
 * Hyperchannel Log Printer
 *
 * Copyright (c) 1983, Tektronix Inc.
 * All Rights Reserved
 *
 */


char _rcsid[] = "$Header$$Locker$";

#define ok(x) (((int)(x)) & 0x7fffffff)
#define HYELOG

#include <stdio.h>
#include <nlist.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>
#include <vaxif/if_hyreg.h>
#include <vaxif/if_hy.h>

struct nlist nl[2] = {
	{ "_hy_elog" },
	{ 0 }
};

u_long hy_elog[HYE_SIZE];

char *hye_code[] = {
	"message code 0 - INVALID",		/* 00 */
	"adapter rcv data from trunk",		/* 01 */
	"bad message length",			/* 02 */
	"no space for message",			/* 03 */
	"to port marked down",			/* 04 */
	"no space for assoc data",		/* 05 */
	"assoc data too big",			/* 06 */
	"illegal function code",		/* 07 */
	"message code 8 - INVALID",		/* 08 */
	"message code 9 - INVALID",		/* 09 */
	"not enough parameters",		/* 0A */
	"too many parameters",			/* 0B */
	"not enough data send to adapter",	/* 0C */
	"too much data sent to adapter",	/* 0D */
	"not all data read from buffer",	/* 0E */
	"host tried to read too much data",	/* 0F */
	"no messages queued for this port",	/* 10 */
	"host master cleared during xfer",	/* 11 */
	"host quit early",			/* 12 */
	"parity error from arapter to host",	/* 13 */
	"trunk transmission aborted",		/* 14 */
	"device reserved to this adapter",	/* 15 */
	"trunk xmit retry counter expired",	/* 16 */
	"other adapter didn't send assoc data",	/* 17 */
	"message code out of range"		/* 18 */
};


main()
{
	register unsigned char *p, *ep;
	register unsigned i;
	int mem;

	nlist("/vmunix", nl);
	if (nl[0].n_type == 0)
		done("No namelist\n");
	if ((mem = open("/dev/kmem", 0)) < 0)
		done("Can't oper /dev/kmem\n");
	lseek(mem, (long)nl[0].n_value, 0);
	read(mem, hy_elog, sizeof(hy_elog));

	printf("Error	Other	XMITMSG	LSTDATA	LOCMSG	Error Description\n");
	for (i = 0; i <= HYE_MAX; i++) {
		printf("%02x	%6d	%6d	%6d	%6d - %s\n", i,
			hy_elog[i],
			hy_elog[i + (HYE_MAX+1)],
			hy_elog[i + 2*(HYE_MAX+1)],
			hy_elog[i + 3*(HYE_MAX+1)],
			hye_code[i]);
	}
}

done(s, p)
	char *s;
	int p;
{
	fprintf(stderr, s, &p);
	exit(1);
}

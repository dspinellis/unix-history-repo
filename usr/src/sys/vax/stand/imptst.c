/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)imptst.c	6.2 (Berkeley) %G%
 */

#include "../h/param.h"
#include "../h/inode.h"

#include "../vaxuba/ubareg.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#define	IMPLEADERS
#include "../netimp/if_imp.h"
#include "../vaxif/if_acc.h"

#include "saio.h"
#include "savax.h"

#define min(a,b) (a<b ? a : b)
#define BUFSIZ 512

char	input[132];
struct	imp_leader imps, *ip = &imps;
char	inbuf[BUFSIZ];
int	writeflg = 0;       

main()
{
	register error = 0, i, len;
	short type, host, impno, link;
	register struct accdevice *addr =
		(struct accdevice *)ubamem(0, 0767700);

	printf("imp interface diagnostic\n");
	printf("read or write test(r or w)? ");
	gets(input);
	while (*input != 'r' && *input != 'w') {
		printf("reply r or w: ");
		gets(input);
	}
	if (*input == 'w') {
		writeflg++;
		printf("enter destination host number: ");
		gets(input);
		while ((host = (short)atol(input)) < 0 || host > 255) {
			printf("range [0, 255], re-enter: ");
			gets(input);
		}
		printf("imp number: ");
		gets(input);
		while ((impno = (short)atol(input)) < 0 || impno > 32767) {
			printf("range [0, 32767], re-enter: ");
			gets(input);
		}
		printf("link number: ");
		gets(input);
		while ((link = (short)atol(input)) < 0 || link > 255) {
			printf("range [0, 255], re-enter: ");
			gets(input);
		}
	}
	printf("initialization starting...\n");
	impinit();
	/* send 3 noops and init imp leader buffer */
	impnoops((struct control_leader *)ip);
	printf("initialization complete\n");
	if (writeflg) {
		printf("starting write test...\n");
		ip->il_host = host;
		ip->il_imp = htons((u_short)impno);
		ip->il_link = link;
		while (!error)
			error = impwrite(ip, sizeof (*ip));
        	printf("imp write error, ocsr=%b\n", (short)error,
			ACC_OUTBITS);
	} else {
		printf("starting read test...\n");
		while (!error) {
printf("impread(%d)\n", sizeof (*ip));
			error = impread(ip, sizeof (*ip));
printf("impread, error=%b\n", error, ACC_INBITS);
			printleader(ip);
			len = ntohs(ip->il_length);
printf("length=%d\n", len);
			/* read any data */
			while ((error & IN_EOM) == 0 &&
			  (error & ~IN_EOM) == 0 && len > 0) {
				i = min(len, BUFSIZ);
printf("impread(%d)\n", i);
				error = impread(inbuf, i);
				len -= i;
printf("error=%b, len=%d\n", error, ACC_INBITS, len);
			}
			error &= ~IN_EOM;
			if (error == 0 && (len > 0 || addr->iwc))
				printf("imp input length mismatch\n");
		}
		printf("imp read error, icsr=%b\n", (short)error, ACC_INBITS);
	}
	printf("...imptest exiting\n");
}

impnoops(cp)
	register struct control_leader *cp;
{
	register i, error;

	bzero((caddr_t)cp, sizeof (struct control_leader));
	cp->dl_format = IMP_NFF;
	cp->dl_mtype = IMPTYPE_NOOP;
	for (i = 0; i < IMP_DROPCNT + 1; i++ ) { 
                cp->dl_link = i;
		if ((error = impwrite(ip, sizeof (*ip))) != 0) {
			printf("imp init error, ocsr=%b\n", (short)error,
				ACC_OUTBITS);
			_stop();
		}
	}
}

impwrite(buf, len)
	register struct imp *buf;
	register len;
{
	register uba, error;
	struct iob io;
	register struct accdevice *addr =
	    (struct accdevice *)ubamem(0, 0767600);

	/* set up uba mapping */
	io.i_ma = (caddr_t)buf;
	io.i_cc = len;
	uba = ubasetup(&io, 0);

	/* set regs and perform i/o */
	addr->oba = (u_short)uba;
	addr->owc = -((io.i_cc + 1) >> 1);
	addr->ocsr = ((short) ((uba & 0x30000) >> 12) | OUT_ENLB | ACC_GO);
	while ((addr->ocsr & ACC_RDY) == 0)
		;
	error = addr->ocsr & (ACC_NXM|ACC_ERR);
	ubafree(uba);
	return(error);
}

impread(buf, len)
	register struct imp *buf;
	register len;
{
	register uba, error;
	struct iob io;
	register struct accdevice *addr =
	    (struct accdevice *)ubamem(0, 0767600);

	/* set up uba mapping */
	io.i_ma = (caddr_t)buf;
	io.i_cc = len;
	uba = ubasetup(&io, 0);
	/* set regs and perform i/o */
	addr->iba = (u_short)uba;
	addr->iwc = -(io.i_cc >> 1);
	addr->icsr = IN_MRDY | IN_WEN | ((uba & 0x30000) >> 12) | ACC_GO;
	while ((addr->icsr & ACC_RDY) == 0)
		;
	error = addr->icsr & (IN_EOM|ACC_ERR|IN_RMR|ACC_NXM);
	ubafree(uba);
	return(error);
}

impinit()
{	
	register struct accdevice *addr = 
		(struct accdevice *)ubamem(0, 0767600);
	register int i;

	/*
	 * Reset the imp interface;
	 * the delays are pure guesswork.
	 */
	addr->icsr = ACC_RESET; DELAY(5000);
        addr->ocsr = ACC_RESET; DELAY(5000);
	addr->ocsr = OUT_BBACK;	DELAY(5000);	/* reset host master ready */
	addr->ocsr = 0;
	addr->icsr = IN_MRDY | IN_WEN;		/* close the relay */
	DELAY(10000);
	/* YECH!!! */
	for (i = 0; i < 500; i++) {
		if ((addr->icsr & IN_HRDY) &&
		    (addr->icsr & (IN_RMR | IN_IMPBSY)) == 0)
			return;
		addr->icsr = IN_MRDY | IN_WEN; DELAY(10000);
		/* keep turning IN_RMR off */
	}
	printf("imp doesn't respond, icsr=%b, ocsr=%b\n",
		addr->icsr, ACC_INBITS, addr->ocsr, ACC_OUTBITS);
}

/*
 *  Convert null-terminated ascii string to binary
 *  and return value.
 *  1st char in string :
 *	0 -> octal
 *	x -> hex
 *	else decimal
 */
atol(as)
	register char *as;
{
	register value = 0;
	register base = 10;
	register sign = 1;
	register digit = 0;
 
aloop :
	if ((digit = (*as++)) == 0)
		return(value) ; /* null */
	if (digit == '-') {
		sign = -sign;
		goto aloop ;
	}
	if (digit == '0')
		base = 8 ;
	else if (digit == 'x')
		base = 16 ;
	else
		value = digit - '0';
	while (digit = (*as++)) {
		if (digit < '0')
			return(0);
		switch (base) {

		case 8 :
			if (digit > '7')
				return(0);
			digit -= '0';
			break;

		case 10 :
			if (digit > '9')
				return(0);
			digit -= '0';
			break;

		case 16 :
			if (digit <= '9') {
				digit -= 060 ;
				break;
			}
			if ((digit >= 'A') && (digit <= 'F')) {
				digit -= 'A' + 10;
				break;
			}
			if ((digit >= 'a') && (digit <= 'f')) {
				digit -= 'a' + 10 ;
				break;
			}
			return(0);
		}
		value = (value * base) + digit;
	}
	return (value * sign);
}

printleader(ip)
	register struct imp_leader *ip;
{
	printbyte((char *)ip, 12);
	printf("<fmt=%x,net=%x,flags=%x,mtype=", ip->il_format, ip->il_network,
		ip->il_flags);
	if (ip->il_mtype <= IMPTYPE_READY)
		printf("%s,", impleaders[ip->il_mtype]);
	else
		printf("%x,", ip->il_mtype);
	printf("htype=%x,host=%x,imp=%x,link=", ip->il_htype, ip->il_host,
		ntohs(ip->il_imp));
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%x,", ip->il_link);
	printf("subtype=%x,len=%x>\n",ip->il_subtype,ntohs(ip->il_length)>>3);
}

printbyte(cp, n)
	register char *cp;
	int n;
{
	register i, j, c;

	for (i=0; i<n; i++) {
		c = *cp++;
		for (j=0; j<2; j++)
			putchar("0123456789abcdef"[(c>>((1-j)*4))&0xf]);
		putchar(' ');
	}
	putchar('\n');
}

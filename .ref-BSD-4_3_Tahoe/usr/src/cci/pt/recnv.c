/************************************************************************
*                                                                       *
*	reccnv.c converts a standard PT hexfile to a larger record for  *
*  	format.  Options allows input filename to be defined output	*
*	fle is always hexout.  Checksums are set to "00" instead of	*
*	trying to calculate them.					*
*	This program assumes that each record is 44 chars long it then  *
*	combines two records affectively into one.			*
* 88 is the standard 152 the second pass and   is the third pass	*
*                                                                       *
************************************************************************/

#include	<stdio.h>
#define	 mode 0777
#define  TRUE 1;
#define  FALSE 0;
static	int    lastflg;	/* says working on last records */

main(argc, argv)
int argc;
char **argv;

{
static  char	record[1024];	/* actual data */
static  char	srecord[1024];	/* actual data */
register int	fs, ss, rs;		/* record size */
static  char	*ttrp, *ttrp2;
FILE	*in;
int	outf;
int	cs, i, pr;
char	*name = "hexout"; /* default filename for output */
char	*name1 = "DEFAULT_LD_FL"; /* default filename */
char	 *rp;	/* record pointer */
int	l=88;	/*length of record to fix */
int	dl;	/* length of data segment */
int	fl=9;	/* header length always the same */
char	*dlc;	/* data length character */
int	aoff;	/* address offset for checking */

if(argc < 2)
	printf("\n using default read file and record size 88\n");
else if(argc == 2)
	{
	name1 = *(++argv);
	}
else if(argc == 3)
	{
	name1 = *(++argv);
	l = atoi(*(++argv));
	}	

/* fix the record parameters */
switch(l)
	{
	case 3:
		l = 280; /* length of two records to be combined */
		dlc = "8";	/* new data length character */
		dl = 256;	/* new data field length */
		aoff = 0x40;	/* address offset of two sequ. rec's. */
		break;
	case 2:
		l = 152;
		dlc = "4";
		dl = 128;
		aoff = 0x20;
		break;
	default :		/* 88 two normal rows */
		l = 88;
		dlc = "2";
		dl = 64;
		aoff = 0x10;
		break;
	}

rp = record;	/* setup pointer to array to dump info */
ttrp2 = srecord;	/* setup pointer to array to dump info */
if((in = fopen(name1, "r")) == (FILE *)NULL )
	printf("can't fopen input file\n");
if((outf = creat(name, mode)) < 0)
	printf("can't create hexout\n");
if((open(name, 2)) < 0)
	printf("can't open hexout\n");

/* read and convert the records */
while (((fs = getrec(in, srecord )) + (ss = getrec(in, record))) > 0)
	{
	rs = fs + ss;
	if(rs == l)
		{
		ttrp = rp;
		/* check to see if the two records have sequential addresses */
		if (pr = acnv(record, srecord, aoff)) /* addreses sequential ?? */
			{
			*(ttrp + 1) = *dlc;	/* double the data amount */
			ttrp = rp + ((dl/2)+fl);   /* where to add new data */

			/* copy from data field */
			for(i=1, ttrp2 = srecord+fl ; i<((dl/2) +1);i++)
				*ttrp++ = *ttrp2++; /* copy data */
			*ttrp++ = '\0';
			*ttrp++ = '\0';
			*ttrp++ = '\n';
			write(outf, record, (dl+fl+3));
			}
		else if(pr == 0) /* not sequential */
/* they don't ave sequential addresses so just write the record as is */
			{
		if(ss >0)
			write(outf, record, ss);
		if(fs >0)
			write(outf, srecord, fs);
			continue;
			}
	}	/*end of large file if */
	else if(rs < l) /* if less than expected read dump as is */
		{
		/*  printf("< if\n");
		*/
		if(ss >0)
			write(outf, record, ss);
		if(fs >0)
			write(outf, srecord, fs);
		continue;
		}
	} /* end of while loop */
exit(0);
}

/************************************************************************
*                                                                       *
*  converts address field and compares to see if they're sequential 	*
*                                                                       *
*                                                                       *
************************************************************************/

int
acnv(a,b,aoff)
char *a, *b;	/* pointers to the two records */
int	aoff;	/* address offset */

{
	char g[5], h[5];
	int i, ch, cg;

	a=a+3; b=b+3;
	for(i=0; i<4; i++,a++,b++)
		{
		g[i] = *a;
		h[i] = *b;
		}
	g[4] = '\0';
	h[4] = '\0';
	cg = cnv(g);
	ch = cnv(h) - aoff;
	if( ch == cg)
		{
		/* printf("out of order \n");
		*/
		return 1;
		}
	else if(ch != cg) 
		return 0;

}

/************************************************************************
*	cnv - converts four ascii digits to an integer			*
*         returns - the integer value					*
*                                                                       *
************************************************************************/
int
cnv(sp)
char *sp;

{
register int c, d, i, b;

b = c = 0;

for(i=0 ; i<4; i++, sp++)
	{
	if (*sp >= '0' && *sp <= '9')
		b = *sp - '0';
	else if (*sp >= 'A' && *sp <= 'F')
		b = *sp - 'A' + 10;
	else if (*sp >= 'a' && *sp <= 'f')
		b = *sp - 'a' + 10;
	switch (i)
		{
		case 3:
			c = b + c;
		case 2:
			c = (b * 16) + c;
			break;
		case 1:
			c = (b * 256) + c;
			break;
		case 0:
			c = (b * 4096) + c;
			break;
		}
	}
return c;
}

/************************************************************************
 *									*
 *	getrec	- Get a record to transmit.				*
 *									*
 *	Returns:							*
 *		-1	Error in file format.				*
 *		0	End of file reached.				*
 *		> 0	Size of record to transmit.			*
 *									*
 ************************************************************************/

int
getrec (fd, rp)

FILE *		fd;
char *		rp;

{

	int		c;
	int		nc;		/* number of characters		*/
	int		l;		/* length of record bytes	*/
	int		cc;		/* checksum			*/
	int		i;		/* counter			*/
	int		a;		/* address piece		*/

	while ((c = getc (fd)) != ':')	/* Look for start of record	*/
		if (c == EOF)
			return 0;	/* End of file			*/

	/*****	Start record off.					*/

/*	printf("At record start\n"); */
	*rp++ = ':';
	nc = 1;

	/*****	Get number of hex bytes in record.			*/

	if ((l = getxb (fd, rp)) < 0)
		return -1;

	if(l == 1)
		{
		/* printf("01 record \n");
		*/
		rp += 2;
		for(i=1; i < 11; i += 2, rp +=2)
			{
			/* printf(" %u\n", i);
			*/
			getxb(fd,rp);
			}
		*(rp) = '\n';
		return 14;
		}	

	else if(l == 0)
		{
		printf("00 record \n");
		rp += 2;
		for(i=1; i < 8; i += 2, rp += 2)
			{
			printf(" %u\n",i);
			getxb(fd,rp);
			}
		*++rp = '\n';
		return 10;
		}	

	else
		{
		/* printf("normal record \n");
		printf("%d data bytes\n", l);
		*/
		cc = l;			/* Start checksum		*/
		rp += 2;		/* Past two characters		*/
		nc += 2;		/* Two more characters		*/

		/*****	Get address portion of data record.		*/
	
		for (i = 2 ; i > 0 ; i--)	/* Two hex bytes	*/
			{
			if ((a = getxb (fd, rp)) < 0)
			return -1;

			cc += a;	/* Figure address into checksum	*/
			rp += 2;	/* Past two characters		*/
			nc += 2;	/* Two more characters		*/
			}

		/*****	Read type byte, which should be zero.		*/

		if ((i = getxb (fd, rp)) != 0)
			return -1;

		cc += i;		/* Figure type into checksum	*/
		rp += 2;		/* Past two characters		*/
		nc += 2;		/* Two more characters		*/

		/*****	Read in the data record.			*/

		while (l-- > 0)		/* Count down			*/
			{
			if ((i = getxb (fd, rp)) < 0)
				return -1;

		/*	printf ("Read %d\n", i);	*/
			cc += i;	/* Checksum			*/
			rp += 2;	/* Pointer			*/
			nc += 2;	/* Number of characters		*/
			}

		cc = (-cc) & 0377;	/* Two's complement byte	*/

		/*	printf ("Checksum = %x\n", cc);	*/
		*rp = cc >> 4;
		*rp += *rp < 10 ? '0' : 'A' - 10;
		*++rp = cc & 0xF;
		*rp += *rp < 10 ? '0' : 'A' - 10;

		/*	printf ("record size = %d\n", nc + 2);	*/
		*++rp = '\n';
		return nc + 3;

		} /* normal records */
}

/************************************************************************
 *									*
 *	getxb	- Get a hex byte.					*
 *									*
 *	Returns:							*
 *		-1	Error in file format or end of file reached.	*
 *		>= 0	Integer value of byte.				*
 *									*
 ************************************************************************/

int
getxb (fd, sp)

FILE *		fd;
char *		sp;

{

	register int	i, j;

	if ((i = getxn (fd, sp++)) < 0 || (j = getxn (fd, sp)) < 0)
		return -1;

	return (i << 4) | j;

}

/************************************************************************
 *									*
 *	getxn	- Get a hex nibble.					*
 *									*
 *	Returns:							*
 *		-1	Error in file format or end of file reached.	*
 *		>= 0	Integer value of nibble.			*
 *									*
 ************************************************************************/

int
getxn (fd, sp)

FILE *		fd;
char *		sp;

{

	register int	c;

	*sp = c = getc (fd);

	if (c >= '0' && c <= '9')
		return c - '0';
	else if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	else if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;

	return -1;

}



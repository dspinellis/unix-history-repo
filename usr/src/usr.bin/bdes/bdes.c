/*
 * BDES -- DES encryption package for Berkeley Software Distribution 4.4
 * options:
 *	-a	key is in ASCII
 *	-c	use CBC (cipher block chaining) mode
 *	-e	use ECB (electronic code book) mode
 *	-f b	use b-bit CFB (cipher feedback) mode
 *	-F b	use b-bit CFB (cipher feedback) alternative mode
 *	-i	invert (decrypt) input
 *	-m b	generate a MAC of length b
 *	-o b	use b-bit OFB (output feedback) mode
 *	-p	don't reset the parity bit
 *	-v v	use v as the initialization vector (ignored for ECB)
 * note: the last character of the last block is the integer indicating
 * how many characters of that block are to be output
 *
 * Author: Matt Bishop
 *	   Department of Mathematics and Computer Science
 *	   Dartmouth College
 *	   Hanover, NH  03755
 * Email:  Matt.Bishop@dartmouth.edu
 *	   ...!decvax!dartvax!Matt.Bishop
 *
 * This is derived from a program written as part of work done for grant
 * NAG 2-680 from the National Aeronautics and Space Administration to
 * Dartmouth College.  It is freely distributable provided:
 * (1) the name and address of the author and the credit to NASA and
 *     Dartmouth are not altered or removed; and
 * (2) any changes made are noted in the leading comments, and the date
 *     and changer are also noted; and
 * (3) all bugs are promptly reported to the author at the above address.
 * Also, as stated in the manual page, "there is no warranty of merchant-
 * ability nor any warranty of fitness for a particular puurpose not any
 * other warranty, either express or implied, as to the accuracy of the
 * enclosed materials or as to their suitability for any particular pur-
 * pose.  Accordingly, the user assumes full responsibility for their 
 * use.  Further, the author assumes no obligation to furnish any assis-
 * tance of any kind whatsoever, or to furnish any additional information
 * or documentation."
 *
 * See Technical Report PCS-TR91-158, Department of Mathematics and Computer
 * Science, Dartmouth College, for a detailed description of the implemen-
 * tation and differences between it and Sun's.  The DES is described in
 * FIPS PUB 46, and the modes in FIPS PUB 81 (see either the manual page
 * or the technical report for a complete reference).
 *
 * 4/1/91 -- bug fix by Matt Bishop
 *	There was an error in the MAC computation if you asked for a
 *	MAC of length not a multiple of 8; you got the first bit from
 *	the first char, the second from the second char, and so on.
 *	Found by inspection of code; fixed.
 */
#include <ctype.h>
#include <stdio.h>
#define C_NULL	((char *) NULL)

/*
 * BSD and System V systems offer special library calls that do
 * block moves and fills, so if possible we take advantage of them
 */
#define	MEMCPY(dest,src,len)	bcopy((src),(dest),(len))
#define	MEMZERO(dest,len)	bzero((dest),(len))

/*
 * these "hide" the calls to the primitive encryption routines
 */
#define	DES_KEY(buf)	{						\
				char bits1[64];	/* bits of key */	\
				expand(buf, bits1);			\
				setkey(bits1);				\
			}
#define DES_XFORM(buf)	{						\
				char bits1[64];	/* bits of message */	\
				expand(buf, bits1);			\
				encrypt(bits1, inverse);		\
				compress(bits1, buf);			\
			}

/*
 * this does an error-checking write
 */
#define	READ(buf, n)	fread(buf, sizeof(char), n, stdin)
#define WRITE(buf,n)						\
		if (fwrite(buf, sizeof(char), n, stdout) != n)	\
			err(1, bn, C_NULL);

/*
 * some things to make references easier
 */
typedef char Desbuf[8];
#define	CHAR(x,i)	(x[i])
#define	UCHAR(x,i)	(x[i])
#define	BUFFER(x)	(x)
#define	UBUFFER(x)	(x)

/*
 * global variables and related macros
 */
#define KEY_DEFAULT		0	/* interpret radix of key from key */
#define KEY_ASCII		1	/* key is in ASCII characters */
int keybase = KEY_DEFAULT;	/* how to interpret the key */

#define MODE_ENCRYPT		0x01	/* encrypt */
#define MODE_DECRYPT		0x02	/* decrypt */
#define MODE_AUTHENTICATE	0x04	/* authenticate */
#define GET_DIRECTION		((mode)&0xf)
#define ISSET_MODE_DIRECTION	(GET_DIRECTION != 0)
#define MODE_ECB		0x10	/* ECB mode */
#define MODE_CBC		0x20	/* CBC mode */
#define	MODE_CFB		0x30	/* cipher feedback mode */
#define MODE_OFB		0x40	/* output feedback mode */
#define	MODE_CFBA		0x50	/* alternative cipher feedback mode */
#define GET_ALGORITHM		((mode)&0xf0)
#define ISSET_MODE_ALGORITHM	(GET_ALGORITHM != 0)
int mode = 0;			/* how to run */

char *keyrep = "*********";	/* replaces command-line key */
Desbuf ivec;			/* initialization vector */
char bits[] = { '\200', '\100',	/* used to extract bits from a char */
		'\040', '\020', '\010', '\004', '\002', '\001' };
int inverse = 0;		/* 0 ti encrypt, 1 to decrypt */
char *progname = "des program";	/* program name */
int macbits = -1;		/* number of bits in authentication */
int fbbits = -1;		/* number of feedback bits */
int pflag = 0;			/* 1 to preserve parity bits */
char *dummyargs[] = { "*****", NULL };	/* argument list to be printed */

/*
 * library hooks
 */
				/* see getopt(3) */
extern int optind;		/* option (argument) number */
extern char *optarg;		/* argument to option if any */

/*
 * library functions
 */
#ifdef notdef
char *sprintf();		/* in core formatted print */
#endif
char *getpass();		/* get a password from a terminal */

main(argc, argv)
int argc;
char **argv;
{
	register int i;		/* counter in a for loop */
	register char *p;	/* used to obtain the key */
	int n;			/* number of command-line errors */
	Desbuf msgbuf;		/* I/O buffer */
	int nargs;		/* internal number of arguments */
	char **arglist;		/* internal argument list */

	/*
	 * hide the arguments
	 */
	nargs = argc;
	argc = 1;
	arglist = argv;
	argv = dummyargs;

	/*
	 * initialize the initialization vctor
	 */
	for(i = 0; i < 8; i++)
		UCHAR(ivec, i) = 0x00;

	/*
	 * process the argument list
	 */
	progname = arglist[0];
	n = 0;
	while ((i = getopt(nargs, arglist, "aceF:f:im:o:pv:")) != EOF)
		switch(i){
		case 'a':		/* key is ASCII */
			keybase = KEY_ASCII;
			break;
		case 'c':		/* use CBC mode */
			if (ISSET_MODE_ALGORITHM)
				err(1, -1, "two modes of operation specified");
			mode |= MODE_CBC;
			break;
		case 'e':		/* use ECB mode */
			if (ISSET_MODE_ALGORITHM)
				err(1, -1, "two modes of operation specified");
			mode |= MODE_ECB;
			break;
		case 'F':		/* use alternative CFB mode */
			if (ISSET_MODE_ALGORITHM)
				err(1, -1, "two modes of operation specified");
			mode |= MODE_CFBA;
			if ((fbbits = setbits(optarg, 7)) > 56 || fbbits == 0)
			err(1, -1, "-F: number must be 1-56 inclusive");
			else if (fbbits == -1)
			err(1, -1, "-F: number must be a multiple of 7");
			break;
		case 'f':		/* use CFB mode */
			if (ISSET_MODE_ALGORITHM)
				err(1, -1, "two modes of operation specified");
			mode |= MODE_CFB;
			if ((fbbits = setbits(optarg, 8)) > 64 || fbbits == 0)
			err(1, -1, "-f: number must be 1-64 inclusive");
			else if (fbbits == -1)
			err(1, -1, "-f: number must be a multiple of 8");
			break;
		case 'i':		/* decrypt */
			if (ISSET_MODE_DIRECTION)
				err(1, -1, "only one of -i and -m allowed");
			mode |= MODE_DECRYPT;
			break;
		case 'm':		/* number of bits for MACing */
			if (ISSET_MODE_DIRECTION)
				err(1, -1, "only one of -i and -m allowed");
			mode |= MODE_AUTHENTICATE;
			if ((macbits = setbits(optarg, 1)) > 64)
			err(1, -1, "-m: number must be 0-64 inclusive");
			break;
		case 'o':		/* use OFB mode */
			if (ISSET_MODE_ALGORITHM)
				err(1, -1, "two modes of operation specified");
			mode |= MODE_OFB;
			if ((fbbits = setbits(optarg, 8)) > 64 || fbbits == 0)
			err(1, -1, "-o: number must be 1-64 inclusive");
			else if (fbbits == -1)
			err(1, -1, "-o: number must be a multiple of 8");
			break;
			break;
		case 'p':		/* preserve parity bits */
			pflag++;
			break;
		case 'v':		/* set initialization vector */
			cvtkey(BUFFER(ivec), optarg);
			break;
		default:		/* error */
			n++;
			break;
		}
	/*
	 * on error, quit
	 */
	if (n > 0)
		exit(1);
	/*
	 * if no direction set, default to encryption
	 */
	if (!ISSET_MODE_DIRECTION)
		mode |= MODE_ENCRYPT;
	if (!ISSET_MODE_ALGORITHM)
		mode |= MODE_CBC;

	/*
	 * pick up the key
	 * -- if there are no more arguments, prompt for it
	 * -- if there is 1 more argument, use it as the key
	 * -- if there are 2 or more arguments, error
	 */
	if (optind == nargs){
		/*
		 * if the key's not ASCII, assume it is
		 */
		keybase = KEY_ASCII;
		/*
		 * get the key
		 */
		if ((p = getpass("Enter key: ")) == NULL)
			err(1, -1, "no key given");
		/*
		 * copy it, nul-padded, into the key area
		 */
		strncpy(BUFFER(msgbuf), p, 8);
	}
	else if (optind + 1 == nargs){
		/*
		 * obtain the bit form of the key
		 * and hide it from a "ps"
		 */
		cvtkey(BUFFER(msgbuf), arglist[optind]);
		arglist[optind] = keyrep;
	}
	else{
		/*
		 * extra arguments -- bomb
		 */
		err(1, -1, "extraneous arguments");
	}


	/*
	 * main loop
	 */
	switch(mode){
	case MODE_ECB|MODE_ENCRYPT:		/* encrypt using ECB mode */
				inverse = 0;
				makekey(msgbuf);
				ecbenc();
				break;
	case MODE_ECB|MODE_DECRYPT:		/* decrypt using ECB mode */
				inverse = 1;
				makekey(msgbuf);
				ecbdec();
				break;
	case MODE_ECB|MODE_AUTHENTICATE:	/* authenticate using ECB */
				err(1, -1, "can't authenticate with ECB mode");
				break;
	case MODE_CBC|MODE_ENCRYPT:		/* encrypt using CBC mode */
				inverse = 0;
				makekey(msgbuf);
				cbcenc();
				break;
	case MODE_CBC|MODE_DECRYPT:		/* decrypt using CBC mode */
				inverse = 1;
				makekey(msgbuf);
				cbcdec();
				break;
	case MODE_CBC|MODE_AUTHENTICATE:	/* authenticate using CBC */
				inverse = 0;
				makekey(msgbuf);
				cbcauth();
				break;
	case MODE_CFB|MODE_ENCRYPT:		/* encrypt using CFB mode */
				inverse = 0;
				makekey(msgbuf);
				cfbenc();
				break;
	case MODE_CFB|MODE_DECRYPT:		/* decrypt using CFB mode */
				inverse = 0;
				makekey(msgbuf);
				cfbdec();
				break;
	case MODE_CFB|MODE_AUTHENTICATE:	/* authenticate using CFB */
				inverse = 0;
				makekey(msgbuf);
				cfbauth();
				break;
	case MODE_CFBA|MODE_ENCRYPT:		/* alternative CFB mode */
				inverse = 0;
				makekey(msgbuf);
				cfbaenc();
				break;
	case MODE_CFBA|MODE_DECRYPT:		/* alternative CFB mode */
				inverse = 0;
				makekey(msgbuf);
				cfbadec();
				break;
	case MODE_OFB|MODE_ENCRYPT:		/* encrypt using OFB mode */
				inverse = 0;
				makekey(msgbuf);
				ofbenc();
				break;
	case MODE_OFB|MODE_DECRYPT:		/* decrypt using OFB mode */
				inverse = 0;
				makekey(msgbuf);
				ofbdec();
				break;
	default:			/* unimplemented */
				err(1, -1, "can't handle that yet");
				break;
	}
	exit(0);

}

/*
 * print a warning message and, possibly, terminate
 */
err(f, n, s)
int f;			/* >0 if fatal (status code), 0 if not */
int n;			/* offending block number */
char *s;		/* the message */
{
	char tbuf[BUFSIZ];

	if (n > 0)
		(void) sprintf(tbuf, "%s (block %d)", progname, n);
	else
		(void) sprintf(tbuf, "%s", progname);
	if (s == C_NULL)
		perror(tbuf);
	else
		fprintf(stderr, "%s: %s\n", tbuf, s);
	if (f > 0)
		exit(f);
}

/*
 * map a hex character to an integer
 */
int tobinhex(c, radix)
char c;
int radix;
{
	switch(c){
	case '0':		return(0x0);
	case '1':		return(0x1);
	case '2':		return(radix > 2 ? 0x2 : -1);
	case '3':		return(radix > 3 ? 0x3 : -1);
	case '4':		return(radix > 4 ? 0x4 : -1);
	case '5':		return(radix > 5 ? 0x5 : -1);
	case '6':		return(radix > 6 ? 0x6 : -1);
	case '7':		return(radix > 7 ? 0x7 : -1);
	case '8':		return(radix > 8 ? 0x8 : -1);
	case '9':		return(radix > 9 ? 0x9 : -1);
	case 'A': case 'a':	return(radix > 10 ? 0xa : -1);
	case 'B': case 'b':	return(radix > 11 ? 0xb : -1);
	case 'C': case 'c':	return(radix > 12 ? 0xc : -1);
	case 'D': case 'd':	return(radix > 13 ? 0xd : -1);
	case 'E': case 'e':	return(radix > 14 ? 0xe : -1);
	case 'F': case 'f':	return(radix > 15 ? 0xf : -1);
	}
	/*
	 * invalid character
	 */
	return(-1);
}

/*
 * convert the key to a bit pattern
 */
cvtkey(obuf, ibuf)
char *obuf;
char *ibuf;
{
	register int i, j;			/* counter in a for loop */
	int nbuf[64];			/* used for hex/key translation */

	/*
	 * just switch on the key base
	 */
	switch(keybase){
	case KEY_ASCII:			/* ascii to integer */
		(void) strncpy(obuf, ibuf, 8);
		return;
	case KEY_DEFAULT:		/* tell from context */
		/*
		 * leading '0x' or '0X' == hex key
		 */
		if (ibuf[0] == '0' && (ibuf[1] == 'x' || ibuf[1] == 'X')){
			ibuf = &ibuf[2];
			/*
			 * now translate it, bombing on any illegal hex digit
			 */
			for(i = 0; ibuf[i] && i < 16; i++)
				if ((nbuf[i] = tobinhex(ibuf[i], 16)) == -1)
					err(1, -1, "bad hex digit in key");
			while(i < 16)
				nbuf[i++] = 0;
			for(i = 0; i < 8; i++)
				obuf[i] = ((nbuf[2*i]&0xf)<<4)|
							(nbuf[2*i+1]&0xf);
			/* preserve parity bits */
			pflag = 1;
			return;
		}
		/*
		 * leading '0b' or '0B' == binary key
		 */
		if (ibuf[0] == '0' && (ibuf[1] == 'b' || ibuf[1] == 'B')){
			ibuf = &ibuf[2];
			/*
			 * now translate it, bombing on any illegal binary digit
			 */
			for(i = 0; ibuf[i] && i < 16; i++)
				if ((nbuf[i] = tobinhex(ibuf[i], 2)) == -1)
					err(1, -1, "bad binary digit in key");
			while(i < 64)
				nbuf[i++] = 0;
			for(i = 0; i < 8; i++)
				for(j = 0; j < 8; j++)
					obuf[i] = (obuf[i]<<1)|nbuf[8*i+j];
			/* preserve parity bits */
			pflag = 1;
			return;
		}
		/*
		 * no special leader -- ASCII
		 */
		(void) strncpy(obuf, ibuf, 8);
	}
}

/*
 * convert an ASCII string into a decimal number:
 * 1. must be between 0 and 64 inclusive
 * 2. must be a valid decimal number
 * 3. must be a multiple of mult
 */
setbits(s, mult)
char *s;
int mult;
{
	register char *p;
	register int n = 0;

	/*
	 * skip white space
	 */
	while (isspace(*s))
		s++;
	/*
	 * get the integer
	 */
	for(p = s; *p; p++){
		if (isdigit(*p))
			n = n * 10 + *p - '0';
		else{
			err(1, -1, "bad decimal digit in MAC length");
		}
	}
	/*
	 * be sure it's a multiple of mult
	 */
	return((n % mult != 0) ? -1 : n);
}

/*****************
 * DES FUNCTIONS *
 *****************/
/*
 * This sets the DES key and (if you're using the deszip version)
 * the direction of the transformation.  This uses the Sun
 * to map the 64-bit key onto the 56 bits that the key schedule
 * generation routines use: the old way, which just uses the user-
 * supplied 64 bits as is, and the new way, which resets the parity
 * bit to be the same as the low-order bit in each character.  The
 * new way generates a greater variety of key schedules, since many
 * systems set the parity (high) bit of each character to 0, and the
 * DES ignores the low order bit of each character.
 */
makekey(buf)
Desbuf buf;			/* key block */
{
	register int i, j;			/* counter in a for loop */
	register int par;			/* parity counter */

	/*
	 * if the parity is not preserved, flip it
	 */
	if (!pflag){
		for(i = 0; i < 8; i++){
			par = 0;
			for(j = 1; j < 8; j++)
				if ((bits[j]&UCHAR(buf, i)) != 0)
					par++;
			if ((par&01) == 01)
				UCHAR(buf, i) = UCHAR(buf, i)&0177;
			else
				UCHAR(buf, i) = (UCHAR(buf, i)&0177)|0200;
		}
	}
	/*
	 * Make the key schedule
	 */
	DES_KEY(UBUFFER(buf));
}

/*
 * This encrypts using the Electronic Code Book mode of DES
 */
ecbenc()
{
	register int n;		/* number of bytes actually read */
	register int bn;	/* block number */
	Desbuf msgbuf;		/* I/O buffer */

	for(bn = 0; (n = READ(BUFFER(msgbuf),  8)) == 8; bn++){
		/*
		 * do the transformation
		 */
		DES_XFORM(UBUFFER(msgbuf));
		WRITE(BUFFER(msgbuf), 8);
	}
	/*
	 * at EOF or last block -- in either ase, the last byte contains
	 * the character representation of the number of bytes in it
	 */
	bn++;
	MEMZERO(&CHAR(msgbuf, n), 8 - n);
	CHAR(msgbuf, 7) = n;
	DES_XFORM(UBUFFER(msgbuf));
	WRITE(BUFFER(msgbuf), 8);

}

/*
 * This decrypts using the Electronic Code Book mode of DES
 */
ecbdec()
{
	register int n;		/* number of bytes actually read */
	register int c;		/* used to test for EOF */
	register int bn;	/* block number */
	Desbuf msgbuf;		/* I/O buffer */

	for(bn = 1; (n = READ(BUFFER(msgbuf), 8)) == 8; bn++){
		/*
		 * do the transformation
		 */
		DES_XFORM(UBUFFER(msgbuf));
		/*
		 * if the last one, handle it specially
		 */
		if ((c = getchar()) == EOF){
			n = CHAR(msgbuf, 7);
			if (n < 0 || n > 7)
				err(1, bn,
					"decryption failed (block corrupted)");
		}
		else
			(void) ungetc(c, stdin);
		WRITE(BUFFER(msgbuf), n);
	}
	if (n > 0)
		err(1, bn, "decryption failed (incomplete block)");
}

/*
 * This encrypts using the Cipher Block Chaining mode of DES
 */
cbcenc()
{
	register int n;		/* number of bytes actually read */
	register int bn;	/* block number */
	Desbuf msgbuf;		/* I/O buffer */

	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(BUFFER(msgbuf), 8)) == 8; bn++){
		for(n = 0; n < 8; n++)
			CHAR(msgbuf, n) ^= CHAR(ivec, n);
		DES_XFORM(UBUFFER(msgbuf));
		MEMCPY(BUFFER(ivec), BUFFER(msgbuf), 8);
		WRITE(BUFFER(msgbuf), 8);
	}
	/*
	 * at EOF or last block -- in either case, the last byte contains
	 * the character representation of the number of bytes in it
	 */
	bn++;
	MEMZERO(&CHAR(msgbuf, n), 8 - n);
	CHAR(msgbuf, 7) = n;
	for(n = 0; n < 8; n++)
		CHAR(msgbuf, n) ^= CHAR(ivec, n);
	DES_XFORM(UBUFFER(msgbuf));
	WRITE(BUFFER(msgbuf), 8);

}

/*
 * This decrypts using the Cipher Block Chaining mode of DES
 */
cbcdec()
{
	register int n;		/* number of bytes actually read */
	Desbuf msgbuf;		/* I/O buffer */
	Desbuf ibuf;		/* temp buffer for initialization vector */
	register int c;		/* used to test for EOF */
	register int bn;	/* block number */

	for(bn = 0; (n = READ(BUFFER(msgbuf), 8)) == 8; bn++){
		/*
		 * do the transformation
		 */
		MEMCPY(BUFFER(ibuf), BUFFER(msgbuf), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(c = 0; c < 8; c++)
			UCHAR(msgbuf, c) ^= UCHAR(ivec, c);
		MEMCPY(BUFFER(ivec), BUFFER(ibuf), 8);
		/*
		 * if the last one, handle it specially
		 */
		if ((c = getchar()) == EOF){
			n = CHAR(msgbuf, 7);
			if (n < 0 || n > 7)
				err(1, bn,
					"decryption failed (block corrupted)");
		}
		else
			(void) ungetc(c, stdin);
		WRITE(BUFFER(msgbuf), n);
	}
	if (n > 0)
		err(1, bn, "decryption failed (incomplete block)");
}

/*
 * This authenticates using the Cipher Block Chaining mode of DES
 */
cbcauth()
{
	register int n, j;		/* number of bytes actually read */
	Desbuf msgbuf;		/* I/O buffer */
	Desbuf encbuf;		/* encryption buffer */

	/*
	 * do the transformation
	 * note we DISCARD the encrypted block;
	 * we only care about the last one
	 */
	while ((n = READ(BUFFER(msgbuf), 8)) == 8){
		for(n = 0; n < 8; n++)
			CHAR(encbuf, n) = CHAR(msgbuf, n) ^ CHAR(ivec, n);
		DES_XFORM(UBUFFER(encbuf));
		MEMCPY(BUFFER(ivec), BUFFER(encbuf), 8);
	}
	/*
	 * now compute the last one, right padding with '\0' if need be
	 */
	if (n > 0){
		MEMZERO(&CHAR(msgbuf, n), 8 - n);
		for(n = 0; n < 8; n++)
			CHAR(encbuf, n) = CHAR(msgbuf, n) ^ CHAR(ivec, n);
		DES_XFORM(UBUFFER(encbuf));
	}
	/*
	 * drop the bits
	 * we write chars until fewer than 7 bits,
	 * and then pad the last one with 0 bits
	 */
	for(n = 0; macbits > 7; n++, macbits -= 8)
		putchar(CHAR(encbuf, n));
	if (macbits > 0){
		CHAR(msgbuf, 0) = 0x00;
		for(j = 0; j < macbits; j++)
			CHAR(msgbuf, 0) |= (CHAR(encbuf, n)&bits[j]);
		putchar(CHAR(msgbuf, 0));
	}
}

/*
 * This encrypts using the Cipher FeedBack mode of DES
 */
cfbenc()
{
	register int n;		/* number of bytes actually read */
	register int nbytes;	/* number of bytes to read */
	register int bn;	/* block number */
	char ibuf[8];		/* input buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 8;
	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(ibuf, nbytes)) == nbytes; bn++){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(n = 0; n < 8 - nbytes; n++)
			UCHAR(ivec, n) = UCHAR(ivec, n+nbytes);
		for(n = 0; n < nbytes; n++)
			UCHAR(ivec, 8-nbytes+n) = ibuf[n] ^ UCHAR(msgbuf, n);
		WRITE(&CHAR(ivec, 8-nbytes), nbytes);
	}
	/*
	 * at EOF or last block -- in either case, the last byte contains
	 * the character representation of the number of bytes in it
	 */
	bn++;
	MEMZERO(&ibuf[n], nbytes - n);
	ibuf[nbytes - 1] = n;
	MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
	DES_XFORM(UBUFFER(msgbuf));
	for(n = 0; n < nbytes; n++)
		ibuf[n] ^= UCHAR(msgbuf, n);
	WRITE(ibuf, nbytes);
}

/*
 * This decrypts using the Cipher Block Chaining mode of DES
 */
cfbdec()
{
	register int n;		/* number of bytes actually read */
	register int c;		/* used to test for EOF */
	register int nbytes;	/* number of bytes to read */
	register int bn;	/* block number */
	char ibuf[8];		/* input buffer */
	char obuf[8];		/* output buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 8;
	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(ibuf, nbytes)) == nbytes; bn++){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(c = 0; c < 8 - nbytes; c++)
			CHAR(ivec, c) = CHAR(ivec, c+nbytes);
		for(c = 0; c < nbytes; c++){
			CHAR(ivec, 8-nbytes+c) = ibuf[c];
			obuf[c] = ibuf[c] ^ UCHAR(msgbuf, c);
		}
		/*
		 * if the last one, handle it specially
		 */
		if ((c = getchar()) == EOF){
			n = obuf[nbytes-1];
			if (n < 0 || n > nbytes-1)
				err(1, bn,
					"decryption failed (block corrupted)");
		}
		else
			(void) ungetc(c, stdin);
		WRITE(obuf, n);
	}
	if (n > 0)
		err(1, bn, "decryption failed (incomplete block)");
}

/*
 * This encrypts using the alternative Cipher FeedBack mode of DES
 */
cfbaenc()
{
	register int n;		/* number of bytes actually read */
	register int nbytes;	/* number of bytes to read */
	register int bn;	/* block number */
	char ibuf[8];		/* input buffer */
	char obuf[8];		/* output buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 7;
	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(ibuf, nbytes)) == nbytes; bn++){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(n = 0; n < 8 - nbytes; n++)
			UCHAR(ivec, n) = UCHAR(ivec, n+nbytes);
		for(n = 0; n < nbytes; n++)
			UCHAR(ivec, 8-nbytes+n) = (ibuf[n] ^ UCHAR(msgbuf, n))
							|0200;
		for(n = 0; n < nbytes; n++)
			obuf[n] = CHAR(ivec, 8-nbytes+n)&0177;
		WRITE(obuf, nbytes);
	}
	/*
	 * at EOF or last block -- in either case, the last byte contains
	 * the character representation of the number of bytes in it
	 */
	bn++;
	MEMZERO(&ibuf[n], nbytes - n);
	ibuf[nbytes - 1] = ('0' + n)|0200;
	MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
	DES_XFORM(UBUFFER(msgbuf));
	for(n = 0; n < nbytes; n++)
		ibuf[n] ^= UCHAR(msgbuf, n);
	WRITE(ibuf, nbytes);
}

/*
 * This decrypts using the alternative Cipher Block Chaining mode of DES
 */
cfbadec()
{
	register int n;		/* number of bytes actually read */
	register int c;		/* used to test for EOF */
	register int nbytes;	/* number of bytes to read */
	register int bn;	/* block number */
	char ibuf[8];		/* input buffer */
	char obuf[8];		/* output buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 7;
	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(ibuf, nbytes)) == nbytes; bn++){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(c = 0; c < 8 - nbytes; c++)
			CHAR(ivec, c) = CHAR(ivec, c+nbytes);
		for(c = 0; c < nbytes; c++){
			CHAR(ivec, 8-nbytes+c) = ibuf[c]|0200;
			obuf[c] = (ibuf[c] ^ UCHAR(msgbuf, c))&0177;
		}
		/*
		 * if the last one, handle it specially
		 */
		if ((c = getchar()) == EOF){
			if ((n = (obuf[nbytes-1] - '0')) < 0
						|| n > nbytes-1)
				err(1, bn, 
					"decryption failed (block corrupted)");
		}
		else
			(void) ungetc(c, stdin);
		WRITE(obuf, n);
	}
	if (n > 0)
		err(1, bn, "decryption failed (incomplete block)");
}


/*
 * This encrypts using the Output FeedBack mode of DES
 */
ofbenc()
{
	register int n;		/* number of bytes actually read */
	register int c;		/* used to test for EOF */
	register int nbytes;	/* number of bytes to read */
	register int bn;	/* block number */
	char ibuf[8];		/* input buffer */
	char obuf[8];		/* output buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 8;
	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(ibuf, nbytes)) == nbytes; bn++){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(n = 0; n < 8 - nbytes; n++)
			UCHAR(ivec, n) = UCHAR(ivec, n+nbytes);
		for(n = 0; n < nbytes; n++){
			UCHAR(ivec, 8-nbytes+n) = UCHAR(msgbuf, n);
			obuf[n] = ibuf[n] ^ UCHAR(msgbuf, n);
		}
		WRITE(obuf, nbytes);
	}
	/*
	 * at EOF or last block -- in either case, the last byte contains
	 * the character representation of the number of bytes in it
	 */
	bn++;
	MEMZERO(&ibuf[n], nbytes - n);
	ibuf[nbytes - 1] = n;
	MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
	DES_XFORM(UBUFFER(msgbuf));
	for(c = 0; c < nbytes; c++)
		ibuf[c] ^= UCHAR(msgbuf, c);
	WRITE(ibuf, nbytes);
}

/*
 * This decrypts using the Output Block Chaining mode of DES
 */
ofbdec()
{
	register int n;		/* number of bytes actually read */
	register int c;		/* used to test for EOF */
	register int nbytes;	/* number of bytes to read */
	register int bn;	/* block number */
	char ibuf[8];		/* input buffer */
	char obuf[8];		/* output buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 8;
	/*
	 * do the transformation
	 */
	for(bn = 1; (n = READ(ibuf, nbytes)) == nbytes; bn++){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(c = 0; c < 8 - nbytes; c++)
			CHAR(ivec, c) = CHAR(ivec, c+nbytes);
		for(c = 0; c < nbytes; c++){
			CHAR(ivec, 8-nbytes+c) = UCHAR(msgbuf, c);
			obuf[c] = ibuf[c] ^ UCHAR(msgbuf, c);
		}
		/*
		 * if the last one, handle it specially
		 */
		if ((c = getchar()) == EOF){
			n = obuf[nbytes-1];
			if (n < 0 || n > nbytes-1)
				err(1, bn,
					"decryption failed (block corrupted)");
		}
		else
			(void) ungetc(c, stdin);
		/*
		 * dump it
		 */
		WRITE(obuf, n);
	}
	if (n > 0)
		err(1, bn, "decryption failed (incomplete block)");
}

/*
 * This authenticates using the Cipher FeedBack mode of DES
 */
cfbauth()
{
	register int n, j;	/* number of bytes actually read */
	register int nbytes;	/* number of bytes to read */
	char ibuf[8];		/* input buffer */
	Desbuf msgbuf;		/* encryption buffer */

	/*
	 * do things in bytes, not bits
	 */
	nbytes = fbbits / 8;
	/*
	 * do the transformation
	 */
	while((n = READ(ibuf, nbytes)) == nbytes){
		MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
		DES_XFORM(UBUFFER(msgbuf));
		for(n = 0; n < 8 - nbytes; n++)
			UCHAR(ivec, n) = UCHAR(ivec, n+nbytes);
		for(n = 0; n < nbytes; n++)
			UCHAR(ivec, 8-nbytes+n) = ibuf[n] ^ UCHAR(msgbuf, n);
	}
	/*
	 * at EOF or last block -- in either case, the last byte contains
	 * the character representation of the number of bytes in it
	 */
	MEMZERO(&ibuf[n], nbytes - n);
	ibuf[nbytes - 1] = '0' + n;
	MEMCPY(BUFFER(msgbuf), BUFFER(ivec), 8);
	DES_XFORM(UBUFFER(msgbuf));
	for(n = 0; n < nbytes; n++)
		ibuf[n] ^= UCHAR(msgbuf, n);
	/*
	 * drop the bits
	 * we write chars until fewer than 7 bits,
	 * and then pad the last one with 0 bits
	 */
	for(n = 0; macbits > 7; n++, macbits -= 8)
		putchar(CHAR(msgbuf, n));
	if (macbits > 0){
		CHAR(msgbuf, 0) = 0x00;
		for(j = 0; j < macbits; j++)
			CHAR(msgbuf, 0) |= (CHAR(msgbuf, n)&bits[j]);
		putchar(CHAR(msgbuf, 0));
	}
}

/*
 * change from 8 bits/Uchar to 1 bit/Uchar
 */
expand(from, to)
Desbuf from;			/* 8bit/unsigned char string */
char to[64];			/* 1bit/char string */
{
	register int i, j;		/* counters in for loop */

	for(i = 0; i < 8; i++)
		for(j = 0; j < 8; j++)
			to[i*8+j] = (CHAR(from, i)>>(7-j))&01;
}

/*
 * change from 1 bit/char to 8 bits/Uchar
 */
compress(from, to)
char from[64];			/* 1bit/char string */
Desbuf to;			/* 8bit/unsigned char string */
{
	register int i, j;		/* counters in for loop */

	for(i = 0; i < 8; i++){
	 	CHAR(to, i) = 0;
		for(j = 0; j < 8; j++)
			CHAR(to, i) = (from[i*8+j]<<(7-j))|CHAR(to, i);
	}
}


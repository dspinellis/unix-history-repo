/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Phil Karn, derived from original work by Jim Gillogly and
 * Richard Outerbridge.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

/* Encrypt/decrypt command compatible with Sun's "des" command */
#include <stdio.h>

char iv[8];	/* Initial vector for CBC mode */
int block;

main(argc,argv)
int argc;
char *argv[];
{
	int c,cnt,encrypt,decrypt,hexflag;
	register int i;
	char key[8],tkey1[20],tkey2[20],*akey,*getpass();
	extern char *optarg;

	hexflag = block = encrypt = decrypt = 0;
	akey = NULL;
	while((c = getopt(argc,argv,"hedk:b")) != EOF){
		switch(c){
		case 'h':
			hexflag++;
			break;
		case 'e':
			encrypt++;
			break;
		case 'd':
			decrypt++;
			break;
		case 'k':
			akey = optarg;
			break;
		case 'b':
			block++;
			break;
		}
	}
	if(encrypt == 0 && decrypt == 0){
		fprintf(stderr,"Usage: des -e|-d [-h] [-k key]\n");
		exit(2);
	}
	if(akey == NULL){
		/* No key on command line, prompt for it */
		memset(tkey1,0,sizeof(tkey1));
		memset(tkey2,0,sizeof(tkey2));
		for(;;){
			akey = getpass("Enter key: ");
			strncpy(tkey1,akey,sizeof(tkey1));
			akey = getpass("Enter key again: ");
			strncpy(tkey2,akey,sizeof(tkey2));
			if(strncmp(tkey1,tkey2,sizeof(tkey1)) != 0){
				fprintf(stderr,"Key mistyped, try again\n");
			} else
				break;
		}
		akey = tkey1;
	}
	if(hexflag){
		for(i=0;i<16;i++){
			if(htoa(akey[i]) == -1){
				fprintf(stderr,"Non-hex character in key\n");
				exit(1);
			}
		}
		gethex(key,akey,8);
	} else {
		strncpy(key,akey,8);
		/* Set up key, determine parity bit */
		for(cnt = 0; cnt < 8; cnt++){
			c = 0;
			for(i=0;i<7;i++)
				if(key[cnt] & (1 << i))
					c++;
			if((c & 1) == 0)
				key[cnt] |= 0x80;
			else
				key[cnt] &= ~0x80;
		}
	}
	/* Blot out original key */
	i = strlen(akey);
	i = (i < 8) ? i : 8;
	memset(akey,0,i);

	desinit(0);
	setkey(key);

	/* Initialize IV to all zeros */
	memset(iv,0,8);

	if(encrypt){
		doencrypt();
	} else {
		dodecrypt();
	}
}
/* Encrypt standard input to standard output */
doencrypt()
{
	char work[8],*cp,*cp1;
	int cnt,i;

	for(;;){
		if((cnt = fread(work,1,8,stdin)) != 8){
			/* Put residual byte count in the last block.
			 * Note that garbage is left in the other bytes,
			 * if any; this is a feature, not a bug, since it'll
			 * be stripped out at decrypt time.
			 */
			work[7] = cnt;
		}
		if(!block){
			/* CBC mode; chain in last cipher word */
			cp = work;
			cp1 = iv;
			for(i=8; i!=0; i--)
				*cp++ ^= *cp1++;
		}
		endes(work);	/* Encrypt block */
		if(!block){	/* Save outgoing ciphertext for chain */
			memcpy(iv,work,8);
		}
		fwrite(work,1,8,stdout);
		if(cnt != 8)
			break;
	}
}
dodecrypt()
{
	char work[8],nwork[8],ivtmp[8],*cp,*cp1;
	int cnt,i;


	cnt = fread(work,1,8,stdin);	/* Prime the pump */
	for(;;){
		if(!block){	/* Save incoming ciphertext for chain */
			memcpy(ivtmp,work,8);
		}
		dedes(work);
		if(!block){	/* Unchain block, save ciphertext for next */
			cp = work;
			cp1 = iv;
			for(i=8; i!=0; i--){
				*cp++ ^= *cp1++;
			}
			memcpy(iv,ivtmp,8);
		}
		/* Save buffer pending next read */
		memcpy(nwork,work,8);
		/* Try to read next block */
		cnt = fread(work,1,8,stdin);
		if(cnt != 8){	/* Can "only" be 0 if not 8 */
			/* Prev block was last one, write appropriate number
			 * of bytes
			 */
			cnt = nwork[7];
			if(cnt < 0 || cnt > 7){
				fprintf(stderr,"Corrupted file or wrong key\n");
			} else if(cnt != 0)
				fwrite(nwork,1,cnt,stdout);
			exit(0);
		} else {
			/* Now okay to write previous buffer */
			fwrite(nwork,1,8,stdout);
		}

	}
}
/* Convert hex/ascii nybble to binary */
int
htoa(c)
char c;
{
	if(c >= '0' && c <= '9')
		return c - '0';
	if(c >= 'a' && c <= 'f')
		return 10 + c - 'a';
	if(c >= 'A' && c <= 'F')
		return 10 + c - 'A';
	return -1;
}
/* Convert bytes from hex/ascii to binary */
gethex(result,cp,cnt)
register char *result;
register char *cp;
register int cnt;
{
	while(cnt-- != 0){
		*result = htoa(*cp++) << 4;
		*result++ |= htoa(*cp++);
	}
}
#ifdef	DEBUG
put8(cp)
register char *cp;
{
	int i;

	for(i=0;i<8;i++){
		fprintf(stderr,"%02x ",*cp++ & 0xff);
	}
}
#endif

/*
 * $Source: /afs/athena.mit.edu/astaff/project/kerberos/src/lib/des/RCS/cbc_encrypt.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * These routines perform encryption and decryption using the DES
 * private key algorithm, or else a subset of it -- fewer inner loops.
 * (AUTH_DES_ITER defaults to 16, may be less.)
 *
 * Under U.S. law, this software may not be exported outside the US
 * without license from the U.S. Commerce department.
 *
 * These routines form the library interface to the DES facilities.
 *
 * Originally written 8/85 by Steve Miller, MIT Project Athena.
 */

#ifndef	lint
static char rcsid_cbc_encrypt_c[] =
"$Id: cbc_encrypt.c,v 4.10 90/01/02 13:46:14 jtkohl Exp $";
#endif	lint

#include <mit-copyright.h>
#include <stdio.h>
#include <des.h>

extern int des_debug;
extern int des_debug_print();

/*
 * This routine performs DES cipher-block-chaining operation, either
 * encrypting from cleartext to ciphertext, if encrypt != 0 or
 * decrypting from ciphertext to cleartext, if encrypt == 0.
 *
 * The key schedule is passed as an arg, as well as the cleartext or
 * ciphertext.	The cleartext and ciphertext should be in host order.
 *
 * NOTE-- the output is ALWAYS an multiple of 8 bytes long.  If not
 * enough space was provided, your program will get trashed.
 *
 * For encryption, the cleartext string is null padded, at the end, to
 * an integral multiple of eight bytes.
 *
 * For decryption, the ciphertext will be used in integral multiples
 * of 8 bytes, but only the first "length" bytes returned into the
 * cleartext.
 */

int
des_cbc_encrypt(in,out,length,key,iv,encrypt)
    des_cblock *in;		/* >= length bytes of input text */
    des_cblock *out;		/* >= length bytes of output text */
    register long length;	/* in bytes */
    int encrypt;		/* 0 ==> decrypt, else encrypt */
    des_key_schedule key;		/* precomputed key schedule */
    des_cblock *iv;		/* 8 bytes of ivec */
{
    register unsigned long *input = (unsigned long *) in;
    register unsigned long *output = (unsigned long *) out;
    register unsigned long *ivec = (unsigned long *) iv;

    unsigned long i,j;
    static unsigned long t_input[2];
    static unsigned long t_output[2];
    static unsigned char *t_in_p;
    static unsigned long xor_0, xor_1;

    t_in_p = (unsigned char *) t_input;
    if (encrypt) {
#ifdef MUSTALIGN
	if ((long) ivec & 3) {
	    bcopy((char *)ivec++, (char *)&t_output[0], sizeof(t_output[0]));
	    bcopy((char *)ivec, (char *)&t_output[1], sizeof(t_output[1]));
	}
	else
#endif
	{
	    t_output[0] = *ivec++;
	    t_output[1] = *ivec;
	}

	for (i = 0; length > 0; i++, length -= 8) {
	    /* get input */
#ifdef MUSTALIGN
	    if ((long) input & 3) {
		bcopy((char *)input++,(char *)&t_input[0],sizeof(t_input[0]));
		bcopy((char *)input++,(char *)&t_input[1],sizeof(t_input[1]));
	    }
	    else
#endif
	    {
		t_input[0] = *input++;
		t_input[1] = *input++;
	    }

	    /* zero pad */
	    if (length < 8)
		for (j = length; j <= 7; j++)
		    *(t_in_p+j)= 0;

#ifdef DEBUG
	    if (des_debug)
		des_debug_print("clear",length,t_input[0],t_input[1]);
#endif
	    /* do the xor for cbc into the temp */
	    t_input[0] ^= t_output[0];
	    t_input[1] ^= t_output[1];
	    /* encrypt */
	    (void) des_ecb_encrypt(t_input,t_output,key,encrypt);
	    /* copy temp output and save it for cbc */
#ifdef MUSTALIGN
	    if ((long) output & 3) {
		bcopy((char *)&t_output[0],(char *)output++,
		      sizeof(t_output[0]));
		bcopy((char *)&t_output[1],(char *)output++,
		      sizeof(t_output[1]));
	    }
	    else
#endif
	    {
		*output++ = t_output[0];
		*output++ = t_output[1];
	    }

#ifdef DEBUG
	    if (des_debug) {
		des_debug_print("xor'ed",i,t_input[0],t_input[1]);
		des_debug_print("cipher",i,t_output[0],t_output[1]);
	    }
#endif
	}
	return 0;
    }

    else {
	/* decrypt */
#ifdef MUSTALIGN
	if ((long) ivec & 3) {
	    bcopy((char *)ivec++,(char *)&xor_0,sizeof(xor_0));
	    bcopy((char *)ivec,(char *)&xor_1,sizeof(xor_1));
	}
	else
#endif
	{
	    xor_0 = *ivec++;
	    xor_1 = *ivec;
	}

	for (i = 0; length > 0; i++, length -= 8) {
	    /* get input */
#ifdef MUSTALIGN
	    if ((long) input & 3) {
		bcopy((char *)input++,(char *)&t_input[0],sizeof(t_input[0]));
		bcopy((char *)input++,(char *)&t_input[1],sizeof(t_input[0]));
	    }
	    else
#endif
	    {
		t_input[0] = *input++;
		t_input[1] = *input++;
	    }

	    /* no padding for decrypt */
#ifdef DEBUG
	    if (des_debug)
		des_debug_print("cipher",i,t_input[0],t_input[1]);
#else
#ifdef lint
	    i = i;
#endif
#endif
	    /* encrypt */
	    (void) des_ecb_encrypt(t_input,t_output,key,encrypt);
#ifdef DEBUG
	    if (des_debug)
		des_debug_print("out pre xor",i,t_output[0],t_output[1]);
#endif
	    /* do the xor for cbc into the output */
	    t_output[0] ^= xor_0;
	    t_output[1] ^= xor_1;
	    /* copy temp output */
#ifdef MUSTALIGN
	    if ((long) output & 3) {
		bcopy((char *)&t_output[0],(char *)output++,
		      sizeof(t_output[0]));
		bcopy((char *)&t_output[1],(char *)output++,
		      sizeof(t_output[1]));
	    }
	    else
#endif
	    {
		*output++ = t_output[0];
		*output++ = t_output[1];
	    }

	    /* save xor value for next round */
	    xor_0 = t_input[0];
	    xor_1 = t_input[1];
#ifdef DEBUG
	    if (des_debug)
		des_debug_print("clear",i,t_output[0],t_output[1]);
#endif
	}
	return 0;
    }
}

/*
 * $Source: /afs/athena.mit.edu/astaff/project/kerberos/src/lib/des/RCS/pcbc_encrypt.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1985, 1986, 1987, 1988 by the Massachusetts Institute
 * of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * These routines perform encryption and decryption using the DES
 * private key algorithm, or else a subset of it-- fewer inner loops.
 * ( AUTH_DES_ITER defaults to 16, may be less)
 *
 * Under U.S. law, this software may not be exported outside the US
 * without license from the U.S. Commerce department.
 *
 * The key schedule is passed as an arg, as well as the cleartext or
 * ciphertext.	 The cleartext and ciphertext should be in host order.
 *
 * These routines form the library interface to the des facilities.
 *
 * spm 8/85	MIT project athena
 */

#ifndef	lint
static char rcsid_pcbc_encrypt_c[] =
"$Id: pcbc_encrypt.c,v 4.11 90/01/02 13:46:30 jtkohl Exp $";
#endif	lint

#include <mit-copyright.h>
#include <stdio.h>
#include <des.h>
#include "des_internal.h"

extern int des_debug;
extern int des_debug_print();

/*
 * pcbc_encrypt is an "error propagation chaining" encrypt operation
 * for DES, similar to CBC, but that, on encryption, "xor"s the
 * plaintext of block N with the ciphertext resulting from block N,
 * then "xor"s that result with the plaintext of block N+1 prior to
 * encrypting block N+1. (decryption the appropriate inverse.  This
 * "pcbc" mode propagates a single bit error anywhere in either the
 * cleartext or ciphertext chain all the way through to the end. In
 * contrast, CBC mode limits a single bit error in the ciphertext to
 * affect only the current (8byte) block and the subsequent block.
 *
 * performs pcbc error-propagation chaining operation by xor-ing block
 * N+1 with both the plaintext (block N) and the ciphertext from block
 * N.  Either encrypts from cleartext to ciphertext, if encrypt != 0
 * or decrypts from ciphertext to cleartext, if encrypt == 0
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
 *
 * This is NOT a standard mode of operation.
 *
 */

int
des_pcbc_encrypt(in,out,length,key,iv,encrypt)
    des_cblock *in;		/* >= length bytes of inputtext */
    des_cblock *out;		/* >= length bytes of outputtext */
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
		bcopy((char *)input,(char *)&t_input[0],sizeof(t_input[0]));
		bcopy((char *)(input+1),(char *)&t_input[1],sizeof(t_input[1]));
	    }
	    else
#endif
	    {
		t_input[0] = *input;
		t_input[1] = *(input+1);
	    }

	    /* zero pad */
	    if (length < 8) {
		for (j = length; j <= 7; j++)
		    *(t_in_p+j)= 0;
	    }

#ifdef DEBUG
	    if (des_debug)
		des_debug_print("clear",length,t_input[0],t_input[1]);
#endif
	    /* do the xor for cbc into the temp */
	    t_input[0] ^= xor_0 ;
	    t_input[1] ^= xor_1 ;
	    /* encrypt */
	    (void) des_ecb_encrypt(t_input,t_output,key,encrypt);

	    /*
	     * We want to XOR with both the plaintext and ciphertext
	     * of the previous block, before we write the output, in
	     * case both input and output are the same space.
	     */
#ifdef MUSTALIGN
	    if ((long) input & 3) {
		bcopy((char *)input++,(char *)&xor_0,sizeof(xor_0));
		xor_0 ^= t_output[0];
		bcopy((char *)input++,(char *)&xor_1,sizeof(xor_1));
		xor_1 ^= t_output[1];
	    }
	    else
#endif
	    {
		xor_0 = *input++ ^ t_output[0];
		xor_1 = *input++ ^ t_output[1];
	    }


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
	t_output[0] = 0;
	t_output[1] = 0;
	xor_0 = 0;
	xor_1 = 0;
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
		bcopy((char *)input++,(char *)&t_input[1],sizeof(t_input[1]));
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
	    t_output[0] ^= xor_0 ;
	    t_output[1] ^= xor_1 ;
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
	    xor_0 = t_output[0] ^ t_input[0];
	    xor_1 = t_output[1] ^ t_input[1];

#ifdef DEBUG
	    if (des_debug)
		des_debug_print("clear",i,t_output[0],t_output[1]);
#endif
	}
	return 0;
    }
}

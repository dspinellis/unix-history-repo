/*
 * $Source: /afs/athena.mit.edu/astaff/project/kerberos/src/lib/des/RCS/cksum.c,v $
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
 * (AUTH_DES_ITER defaults to 16, may be less.)
 *
 * Under U.S. law, this software may not be exported outside the US
 * without license from the U.S. Commerce department.
 * 
 * These routines form the library interface to the DES facilities.
 *
 *	spm	8/85	MIT project athena
 */

#ifndef	lint
static char rcsid_cksum_c[] =
"$Id: cksum.c,v 4.10 90/01/02 13:46:25 jtkohl Exp $";
#endif	lint

#include <mit-copyright.h>
#include <stdio.h>
#include <strings.h>

#include <des.h>
#include "des_internal.h"

extern int des_debug;
extern int des_debug_print();

/*
 * This routine performs DES cipher-block-chaining checksum operation,
 * a.k.a.  Message Authentication Code.  It ALWAYS encrypts from input
 * to a single 64 bit output MAC checksum.
 *
 * The key schedule is passed as an arg, as well as the cleartext or
 * ciphertext. The cleartext and ciphertext should be in host order.
 *
 * NOTE-- the output is ALWAYS 8 bytes long.  If not enough space was
 * provided, your program will get trashed.
 *
 * The input is null padded, at the end (highest addr), to an integral
 * multiple of eight bytes.
 */

unsigned long
des_cbc_cksum(in,out,length,key,iv)
    des_cblock *in;		/* >= length bytes of inputtext */
    des_cblock *out;		/* >= length bytes of outputtext */
    register long length;	/* in bytes */
    des_key_schedule key;		/* precomputed key schedule */
    des_cblock *iv;		/* 8 bytes of ivec */
{
    register unsigned long *input = (unsigned long *) in;
    register unsigned long *output = (unsigned long *) out;
    unsigned long *ivec = (unsigned long *) iv;

    unsigned long i,j;
    static unsigned long t_input[2];
    static unsigned long t_output[8];
    static unsigned char *t_in_p;

    t_in_p = (unsigned char *) t_input;
#ifdef MUSTALIGN
    if ((long) ivec & 3) {
	bcopy((char *)ivec++,(char *)&t_output[0],sizeof(t_output[0]));
	bcopy((char *)ivec,(char *)&t_output[1],sizeof(t_output[1]));
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
	t_input[0] ^= t_output[0] ;
	t_input[1] ^= t_output[1] ;
	/* encrypt */
	(void) des_ecb_encrypt(t_input,t_output,key,1);
#ifdef DEBUG
	if (des_debug) {
	    des_debug_print("xor'ed",i,t_input[0],t_input[1]);
	    des_debug_print("cipher",i,t_output[0],t_output[1]);
	}
#else
#ifdef lint
	i = i;
#endif
#endif
    }
    /* copy temp output and save it for checksum */
#ifdef MUSTALIGN
    if ((long) output & 3) {
	bcopy((char *)&t_output[0],(char *)output++,sizeof(t_output[0]));
	bcopy((char *)&t_output[1],(char *)output,sizeof(t_output[1]));
    }
    else
#endif
    {
	*output++ = t_output[0];
	*output = t_output[1];
    }

    return (unsigned long) t_output[1];
}

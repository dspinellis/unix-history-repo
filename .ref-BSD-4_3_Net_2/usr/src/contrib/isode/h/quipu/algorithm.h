/* algorithm.h - */

/* 
 * $Header: /f/osi/h/quipu/RCS/algorithm.h,v 7.1 91/02/22 09:25:23 mrose Interim $
 *
 *
 * $Log:	algorithm.h,v $
 * Revision 7.1  91/02/22  09:25:23  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/28  06:26:53  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#ifndef QUIPUALGORITHM
#define QUIPUALGORITHM

struct PublicKeyAlgorithm {
	char *name;
	IFP  encrypt;
	IFP  decrypt;
	IFP  read_secret;
	IFP  write_secret;
	IFP  read_public;
	IFP  write_public;
	IFP  pack_encrypted;
	IFP  unpack_encrypted;
	IFP  pack_public;
	IFP  unpack_public;
	IFP  pack_secret;
	IFP  unpack_secret;
	IFP  get_blocksize;
	IFP  free_encrypted;
};

struct HashAlgorithm {
	char *hash_name;
	IFP  hash_block;
	IFP  hash_short;
	IFP  hash_start;
	IFP  hash_end;
        IFP  hash_pack;
	IFP  hash_unpack;
	IFP  hash_blocksize;
	IFP  hash_compare;
	IFP  hash_free;
}; 

struct SignatureAlgorithm {
	char *sig_name;
	struct PublicKeyAlgorithm *sig_public;
	struct HashAlgorithm *sig_hash;
};

#endif /* QUIPUALGORITHM */


/* null-int.c - does nothing ... */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/null_int.c,v 7.1 91/02/22 09:19:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/null_int.c,v 7.1 91/02/22 09:19:42 mrose Interim $
 *
 *
 * $Log:	null_int.c,v $
 * Revision 7.1  91/02/22  09:19:42  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/28  06:27:21  mrose
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

#include "manifest.h"

extern char *calloc();

int null_encrypt(in, out, key, parms)
struct NullEncrypted *in;
struct NullEncrypted **out;
struct NullPublicKey *key;
struct NullParameters *parms;
{
  return (OK);
}

int null_decrypt(in, out, key, parms)
struct NullEncrypted *in;
struct NullEncrypted **out;
struct NullSecretKey *key;
struct NullParameters *parms;
{
  return (OK);
}

int null_read_secret_key(in, out, parms)
char *in;
struct NullSecretKey **out;
struct NullParameters **parms;
{
  return (OK);
}

int null_write_secret_key(in, out, parms)
struct NullSecretKey *in;
char *out;
struct NullParameters *parms;
{
  return (OK);
}

int null_read_public_key(in, out, parms)
char *in;
struct NullPublicKey **out;
struct NullParameters **parms;
{
  return (OK);
}

int null_write_public_key(in, out, parms)
struct NullPublicKey *in;
char *out;
struct NullParameters *parms;
{
  return (OK);
}

int null_pack_public_key(in, out, len, parms)
struct NullPublicKey *in;
char **out;
int *len;
struct NullParameters *parms;
{
  return (OK);
}

int null_unpack_public_key(in, out, parms)
char *in;
struct NullPublicKey **out;
struct NullParameters **parms;
{
  return (OK);
}

int null_pack_secret_key(in, out, len, parms)
struct NullPublicKey *in;
char **out;
int *len;
struct NullParameters *parms;
{
  return (OK);
}

int null_unpack_secret_key(in, out, parms)
char *in;
struct NullSecretKey **out;
struct NullParameters **parms;
{
  return (OK);
}

int null_free_encrypted(in)
struct NullEncrypted *in;
{
  return (OK);
}

int null_get_blocksize(parms)
struct NullParameters *parms;
{
  return (512);
}

int null_pack_encrypted(in, out, len, parms)
struct NullEncrypted *in;
char **out;
int *len;
struct NullParameters *parms;
{
char *result;

  result = calloc(1, 64);
  *out = result;
  *len = 512;
  return (OK);
}

int null_unpack_encrypted(in, out, parms)
char *in;
struct NullEncrypted **out;
struct NullParameters *parms;
{
  return (OK);
}

int null_init_hash(hash, key, parms)
struct NullHash **hash;
struct NullPublicKey *key;
struct NullParameters *parms;
{
  (*hash) = (struct NullHash *) 0;
   return (OK);
}

int null_hash_block(hash, cp, key, parms)
struct NullHash *hash;
char *cp;
struct NullPublicKey *key;
struct NullParameters *parms;
{
  return (OK);
}

int null_hash_short_block(hash, cp, len, key, parms)
struct NullHash *hash;
char *cp;
int len;
struct NullPublicKey *key;
struct NullParameters *parms;
{
  return (OK);
}

int null_terminate_hash(hash)
struct NullHash *hash;
{
  return (OK);
}

int null_compare_hash(a, b)
struct NullHash *a;
struct NullHash *b;
{
  return (OK);
}

int null_pack_hash(in, out, len, parms)
struct Nullhash *in;
char **out;
int *len;
struct NullParameters *parms;
{
char *result;

  result = calloc(1, 64);
  *out = result;
  *len = 512;
  return (OK);
}

int null_unpack_hash(in, out, parms)
char *in;
struct NullHash **out;
struct NullParameters *parms;
{
  return (OK);
}

int null_free_hash(hash)
struct NullHash *hash;
{
  return (OK);
}


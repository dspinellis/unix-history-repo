/* security.c - Check security parameters */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/security.c,v 7.3 91/02/22 09:20:12 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/security.c,v 7.3 91/02/22 09:20:12 mrose Interim $
 *
 *
 * $Log:	security.c,v $
 * Revision 7.3  91/02/22  09:20:12  mrose
 * Interim 6.8
 * 
 * Revision 7.1  89/12/19  16:20:47  mrose
 * sync
 * 
 * Revision 6.0  89/09/08  10:20:02  mrose
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


#include "logger.h"
#include "quipu/ds_error.h"
#include "quipu/commonarg.h"
#include "pepsy.h"
#include "quipu/AF_pre_defs.h"
#include "quipu/algorithm.h"

extern int dn_print();
extern LLog *log_dsap;
#ifndef NO_STATS
extern LLog *log_stat;
#endif

#define adios(a, b) fatal(-1, b)

char *new_version();
unsigned *compute_signature();
struct signature *sign_operation_aux();
struct signature *sign_operation();

struct GenericHash *pe2hash();

/* 
 * Cache holding keys of trusted certification authorities
 */

struct ca_record *ca_key_cache = (struct ca_record *) 0;

/*
 * Cache holding keys of users (untrusted)
 */

struct ca_record *user_key_cache = (struct ca_record *) 0;

/*
 * Own certificate. (For convenient access).
 */

struct certificate *my_certificate = (struct certificate *) 0;

/*
 * Secret key.
 */

static struct GenericSecretKey *my_secret_key;
static struct GenericParameters *my_key_parms;

/*
 * Options to hash function. 
 */

static struct GenericParameters *my_hash_parms;

struct ca_record *find_user_keyinfo();
struct ca_record *find_ca_keyinfo();

/*
 * Callbacks that actually do the work
 */



extern int null_encrypt(), null_decrypt(), null_read_secret_key(),
	null_write_secret_key(), null_read_public_key(), 
	null_write_public_key(),
	null_pack_encrypted(), null_unpack_encrypted(), null_pack_public_key(),
	null_unpack_public_key(), null_pack_secret_key(), 
	null_unpack_secret_key(),
	null_get_blocksize(), null_free_encrypted();

static struct PublicKeyAlgorithm null_pk_algorithm = {
	"quipuEncryptionAlgorithm.2",	
	null_encrypt,
	null_decrypt,
	null_read_secret_key,
	null_write_secret_key,
	null_read_public_key,
	null_write_public_key,
	null_pack_encrypted,
	null_unpack_encrypted,
	null_pack_public_key,
	null_unpack_public_key,
	null_pack_secret_key,
	null_unpack_secret_key,
	null_get_blocksize,
	null_free_encrypted
};

static struct PublicKeyAlgorithm *pk_alg = &null_pk_algorithm;



extern int null_hash_block(), null_hash_short_block(), null_init_hash(),
	null_terminate_hash(), null_pack_hash(), null_unpack_hash(),
	null_get_blocksize(), null_compare_hash(), null_free_hash();

static struct HashAlgorithm null_hash_algorithm = {
	"quipuHashAlgorithm.2",
	null_hash_block,
	null_hash_short_block,
	null_init_hash,
	null_terminate_hash,
        null_pack_hash,
	null_unpack_hash,
	null_get_blocksize,
	null_compare_hash,
	null_free_hash
}; 

static struct HashAlgorithm *hash_alg = &null_hash_algorithm;


static struct SignatureAlgorithm md2_rsa_algorithm = {
	"quipuSignatureAlgorithm.2",
	&null_pk_algorithm,
	&null_hash_algorithm
};

static struct SignatureAlgorithm *sig_alg = &md2_rsa_algorithm;

void set_algorithms(alg)
struct SignatureAlgorithm *alg;
{
  pk_alg = alg->sig_public;
  hash_alg = alg->sig_hash;
  sig_alg = alg;
}

/*
 * Check security parameters - return 0 or the number of the security error.
 */

/* ARGSUSED */
int check_security_parms(data, type, module, sp, sig, nameptr)
caddr_t data;
int type;
modtyp *module;
struct security_parms *sp;
struct signature *sig;
DN *nameptr;
{
extern long time();
long time_now;
long time_then;
long delta;

  /* If parameters are present, they must be valid */

  if (sp != (struct security_parms *) 0)
  {
    if (sp->sp_time != NULLCP)
    {
      (void) time(&time_now);
      time_then = gtime(ut2tm(str2utct(sp->sp_time, strlen(sp->sp_time))));
      delta = time_now - time_then;
    }
    else
      delta = 0L;

#ifndef NO_STATS
    DLOG(log_dsap, LLOG_NOTICE, 
	("Delay=%D s, protection%s requested, certificate%s present",
		delta, 
		(sp->sp_target == '\0') ? " not" : "",
		(sp->sp_path == (struct certificate_list *) 0) ? " not" : "" 
		));
   /* NB : must use "" rather than NULLCP for the above to work. */
#endif
   }

/* If no signature is provided, nothing else to do */

  if (sig == (struct signature *) 0)
	return (0);

#ifndef NO_STATS
    DLOG(log_dsap, LLOG_NOTICE, ("Operation is signed"));
#endif

/* Policy : signed messages must have security parameters present. */
  if (sp == (struct security_parms *) 0)
  {
#ifndef NO_STATS
    DLOG(log_dsap, LLOG_EXCEPTIONS, ("No security parameters present"));
#endif
    return (DSE_SC_INVALIDCREDENTIALS);
  }

/* Policy: signed messages must have a time-stamp. */
  if (sp->sp_time == NULLCP)
  {
#ifndef NO_STATS
    DLOG(log_dsap, LLOG_EXCEPTIONS, ("No time-stamp present"));
#endif
    return (DSE_SC_INVALIDCREDENTIALS);
  }

/* Policy: a certification path must be provided. */
  if (sp->sp_path == (struct certificate_list *) 0)
  {
#ifndef NO_STATS
    DLOG(log_dsap, LLOG_EXCEPTIONS, ("No certification path"));
#endif
    return (DSE_SC_INVALIDCREDENTIALS);
  }

  return check_cert_path(data, type, module, sp->sp_path, sig, nameptr);
}

int check_cert_path(data, type, module, path, sig, nameptr)
caddr_t data;
int type;
modtyp *module;
struct certificate_list *path;
struct signature *sig;
DN *nameptr;
{
struct ca_record *keyinfo;
int result;
struct GenericPublicKey *ca_public, *user_public;
struct GenericParameters *ca_parms, *user_parms;
char *now;

  if (path == (struct certificate_list *) 0)
  {
#ifndef NO_STATS
	DLOG(log_dsap, LLOG_EXCEPTIONS, ("No certification path provided"));
#endif
	return (DSE_SC_AUTHENTICATION);
  }

  pslog(log_dsap, LLOG_NOTICE, "Certificate subject:", 
	dn_print, (caddr_t) path->cert->subject);


  keyinfo = find_user_keyinfo(path->cert->subject);

  if (keyinfo != (struct ca_record *) 0)
  {
   if (oid_cmp(keyinfo->key.alg.algorithm, name2oid(pk_alg->name)) != 0)
   {
     DLOG(log_dsap, LLOG_EXCEPTIONS, ("Cryptographic algorithm not supported"));
     return (DSE_SV_UNAVAILABLE);
   }
   else
      result = (*pk_alg->unpack_public)(keyinfo->key.value, 
	&user_public, &user_parms);
  }
  else
  {
   pslog(log_dsap, LLOG_NOTICE, "Certificate issuer:", 
	dn_print, (caddr_t) path->cert->issuer);

   keyinfo = find_ca_keyinfo(path->cert->issuer);

   if (keyinfo == (struct ca_record *) 0)
   {
#ifndef NO_STATS
     DLOG(log_dsap, LLOG_EXCEPTIONS, ("Invalid certification path"));
#endif
     return (DSE_SC_AUTHENTICATION);
   }

   DLOG(log_dsap, LLOG_NOTICE, ("Checking certificate"));

   now = new_version();

   if (strcmp(now, keyinfo->valid.not_before) < 0)
   {
#ifndef NO_STATS
     DLOG(log_dsap, LLOG_EXCEPTIONS, ("CA key not yet valid"));
#endif
     return (DSE_SC_AUTHENTICATION);
   }

   if (strcmp(now, keyinfo->valid.not_after) > 0)
   {
#ifndef NO_STATS
     DLOG(log_dsap, LLOG_EXCEPTIONS, ("CA key has expired"));
#endif
     return (DSE_SC_AUTHENTICATION);
   }

   
   if (strcmp(now, path->cert->valid.not_before) < 0)
   {
#ifndef NO_STATS
     DLOG(log_dsap, LLOG_EXCEPTIONS, ("Certificate not yet valid"));
#endif
     return (DSE_SC_AUTHENTICATION);
   }

   if (strcmp(now, path->cert->valid.not_after) > 0)
   {
#ifndef NO_STATS
    DLOG(log_dsap, LLOG_EXCEPTIONS, ("Certificate has expired"));
#endif
     return (DSE_SC_AUTHENTICATION);
   }

   if (oid_cmp(keyinfo->key.alg.algorithm, name2oid(pk_alg->name)) != 0)
   {
      DLOG(log_dsap, LLOG_EXCEPTIONS, ("Algorithm not supported"));
      return (DSE_SV_UNAVAILABLE);
   }
   else
     result = (pk_alg->unpack_public)(keyinfo->key.value, &ca_public, &ca_parms);

   if (result == OK)
      result = check_signature((caddr_t) path->cert, 
			_ZCertificateToSignAF,
			&_ZAF_mod,
			&(path->cert->sig),
			ca_public, ca_parms);
   if (result == NOTOK)
   {
#ifndef NO_STATS
     DLOG(log_dsap, LLOG_EXCEPTIONS, ("Invalid certificate"));
#endif
     return (DSE_SC_INVALIDSIGNATURE); 
   }
   if (oid_cmp(path->cert->key.alg.algorithm, name2oid(pk_alg->name)) != 0)
   {
      DLOG(log_dsap, LLOG_EXCEPTIONS, ("Algorithm not supported"));
      return (DSE_SV_UNAVAILABLE);
   }
   if (result == OK)
     result = (pk_alg->unpack_public)(path->cert->key.value, 
			&user_public, &user_parms);
  }

  DLOG(log_dsap, LLOG_NOTICE, ("Checking user's signature"));

  if (result == OK)
      result = check_signature(data, type, module, sig,
			user_public, user_parms);
  if (result == NOTOK)
  {
    DLOG(log_dsap, LLOG_EXCEPTIONS, ("Invalid user signature"));
    return (DSE_SC_INVALIDSIGNATURE); 
  }

  DLOG(log_dsap, LLOG_NOTICE, ("Signature was OK"));

#ifndef NO_STATS
  pslog(log_dsap, LLOG_NOTICE, "Operation signed by", (IFP) dn_print,
	(caddr_t) path->cert->subject);
#endif	

  *nameptr = path->cert->subject;

  return (OK);
}

/*
 * Having decided that a CA is trusted (eg. by looking a tailor file),
 * add its key to the cache.
 */

int add_ca_key(str)
char *str;
{
struct key_info key;
struct validity valid;
DN name;
char *ptr;
OID alg;

  ptr = index(str, '#');
  if (ptr == NULLCP)
    return (NOTOK);
  *ptr = '\0';
  ptr++;
  name = str2dn(str);
  if (name == NULLDN)
  {
    DLOG(log_dsap, LLOG_FATAL, ("Invalid CA name: %s", str));
    return (NOTOK);
  }

  str = ptr;
  ptr = index(str, '#');
  if (ptr == NULLCP)
    return (NOTOK);
  *ptr = '\0';
  ptr++;
  alg = name2oid(str);
  if (alg == NULLOID)
  {
    DLOG(log_dsap, LLOG_FATAL, ("Invalid algorithm: %s", str));
    return (NOTOK);
  }
  key.alg.algorithm = alg;

  str = ptr;
  ptr = index(str, '#');
  if (ptr == NULLCP)
  {
    DLOG(log_dsap, LLOG_FATAL, ("Algorithm parameters missing"));
    return (NOTOK);
  }
  *ptr = '\0';
  ptr++;
  str2alg(str, &(key.alg));

  str = ptr;
  ptr = index(str, '#');
  if (ptr != NULLCP)
  {
    *ptr = '\0';
    ptr++;
  }
  str2encrypted(str, &(key.value), &(key.n_bits));

  if (ptr)
  {
    str = ptr;
    ptr = index(str, '#');
    if (ptr == NULLCP)
    {
      DLOG(log_dsap, LLOG_FATAL, ("End of key velidity not specified"));
      return (NOTOK);
    }
    *ptr = '\0';
    ptr++;
    valid.not_before = strdup(str);
    str = ptr;
    valid.not_after = strdup(str);
  }
  else
  {
    valid.not_before = "000000000000Z";
    valid.not_after =  "891231235959Z";
  }
  return (add_ca_key_aux(name, &key, &valid));
}

int add_ca_key_aux(name, key, valid)
DN name;
struct key_info *key;
struct validity *valid;
{
struct ca_record *new;

  pslog(log_dsap, LLOG_NOTICE, "Adding CA:", dn_print, (caddr_t) name);

  new = (struct ca_record *) calloc(1, sizeof(*new));
  if (new == (struct ca_record *) 0)
	return (NOTOK);

  new->name = name;
  bcopy((char *)key, (char *)&(new->key), sizeof(struct key_info));
  new->valid.not_before = valid->not_before;
  new->valid.not_after = valid->not_after;
  new->next = ca_key_cache;
  ca_key_cache = new;

  return (OK);
} 


/* ARGSUSED */
static struct ca_record *find_keyinfo_aux(cache, name)
struct ca_record *cache;
DN name;
{
struct ca_record *ptr;

  ptr = cache;

  while (ptr)
  {
   if (dn_cmp(name, ptr->name) == 0)
     return (ptr);
   ptr = ptr->next;
  }

  return (ptr); /* ie. NULL */
}

struct ca_record *find_user_keyinfo(name)
DN name;
{
  return (find_keyinfo_aux(user_key_cache, name));
}

struct ca_record *find_ca_keyinfo(name)
DN name;
{
  return (find_keyinfo_aux(ca_key_cache, name));
}

/*
 * Read secret key from a file.
 */

/* ARGSUSED */
int set_secret_key(str)
char *str;
{
int rc;

  rc = (pk_alg->read_secret)(str, &my_secret_key, &my_key_parms);
  return (rc);
}

/*
 * Compute signature. To do this, have to know canonical BER encoding of the
 * data structure. Hence, this routine takes a PEPY-produced encoder as one
 * parameter, and uses it to produce a PE.
 */

struct signature *sign_operation(data, type, module)
char *data;
int type;
modtyp *module;
{
  return sign_operation_aux(data, type, module, 
	my_secret_key, my_key_parms, my_hash_parms);
}

/* ARGSUSED */
struct signature *sign_operation_aux(data, type, module, key, parms, mdparms)
char     *data; 
int      type;
modtyp   *module;
struct   GenericSecretKey *key;
struct   GenericParameters *parms;
struct   GenericParameters *mdparms;
{
struct signature *result;
unsigned *csig;
PE pe;

  enc_f(type, module, &pe, 0, 0, 0, data);
  
  csig = compute_signature(pe, key, parms, mdparms);

  result = (struct signature *) calloc(1, sizeof(*result));

  (pk_alg->pack_encrypted)(csig, &(result->encrypted), &(result->n_bits), parms);
  result->alg.algorithm = name2oid(sig_alg->sig_name);
  result->alg.algorithm = oid_cpy(result->alg.algorithm);
  result->alg.p_type = ALG_PARM_UNKNOWN;
  result->alg.asn = pe_alloc(PE_CLASS_UNIV, PE_FORM_PRIM, PE_PRIM_NULL);

  (pk_alg->free_encrypted)(csig, parms);
  pe_free(pe); 

  return (result);
}

int check_signature(data, type, module, sig, pubkey, parms)
char   *data;
int    type;
modtyp *module;
struct signature *sig;
struct GenericPublicKey *pubkey;
struct GenericParameters *parms;
{
PE pe;
unsigned *mac;
unsigned *r;
int      i;
int      result;
char *cp;
char *cp2;
int len;
struct GenericHash *hash, *hash2;

  if (oid_cmp(sig->alg.algorithm, name2oid(sig_alg->sig_name)) != 0)
  {
    DLOG(log_dsap, LLOG_EXCEPTIONS, 
	("check_signature: Cryptographic algorithm not supported"));
    return (NOTOK);
  }
  enc_f(type, module, &pe, 0, 0, 0, data); 

  (pk_alg->unpack_encrypted)(sig->encrypted, &mac, parms); 

  hash = pe2hash(pe, pubkey, parms);

  (pk_alg->encrypt)(mac, &r, pubkey, parms);

  (pk_alg->pack_encrypted)(r, &cp2, &len, parms);

  (hash_alg->hash_unpack)(cp2+(pk_alg->get_blocksize)(parms)/8
	- (hash_alg->hash_blocksize)(parms)/8, &hash2, parms); 

  result = (hash_alg->hash_compare)(hash, hash2);

  /* pe_free(pe); */ /* really ought to free this */
  (hash_alg->hash_free)(hash);
  (hash_alg->hash_free)(hash2);
  (pk_alg->free_encrypted)(r, parms);
  (pk_alg->free_encrypted)(mac, parms);

  return (result);
}

unsigned *compute_signature(pe, key, parms, mdparms)
PE       pe;
struct   GenericSecretKey *key;
struct   GenericParameters *parms;
struct   GenericParameters *mdparms;
{
unsigned *r;
unsigned *b;
struct GenericHash *hash;
char *cp;
int n_bits;
char buff[4096];

  hash = pe2hash(pe, (caddr_t) 0, mdparms);
  (hash_alg->hash_pack)(hash, &cp, &n_bits, mdparms);
  bzero(buff, (pk_alg->get_blocksize)(parms)/8);
  bcopy(cp, buff+(pk_alg->get_blocksize)(parms)/8
	- (hash_alg->hash_blocksize)(mdparms)/8,
	(hash_alg->hash_blocksize)(mdparms)/8);
  (pk_alg->unpack_encrypted)(buff, &b, parms);
  free(cp);

  (pk_alg->decrypt)(b, &r, key, parms);

/*  (pk_alg->free_encrypted)(b, parms); */

  return (r);
}

struct GenericHash *pe2hash(pe, key, parm)
PE pe;
struct GenericPublicKey *key;
struct GenericParameters *parm;
{
PS stream;
int blocksize;
int length;
int n_blocks;
int extra;
char *txt;
char *buff;
struct GenericHash *hash;
int i;
int j;
int strategy;

  strategy = ps_len_strategy;
  ps_len_strategy = PS_LEN_LONG;

  stream = ps_alloc(str_open);
  if (stream == NULLPS)
	adios(NULLCP, "ps_alloc failed");

  if (str_setup(stream, NULLCP, 4096, 0) == NOTOK)
	adios(NULLCP, "str_setup failed");

  if (pe2ps(stream,pe) != OK) 
	adios(NULLCP, "pe2ps failed");

  blocksize = (hash_alg->hash_blocksize)(parm)/8;

  length = stream -> ps_byteno;
  txt = stream -> ps_base;

  n_blocks = length/blocksize;
  extra    = length%blocksize;
  buff     = calloc(blocksize, sizeof(char));

/* Set the `initialisation vector' to all zeros */

  (hash_alg->hash_start)(&hash, key, parm);

/* For each complete block ... */

  for (i=0;i<n_blocks;i++)
   {
    for (j=0;j<blocksize;j++)
    buff[j] = txt[i*blocksize + j];
    (hash_alg->hash_block)(hash, buff, key, parm);
   }

/* Pad the last block */

  if (extra != 0)
  {
    for (j=0; j<extra; j++)
      buff[j] = txt[n_blocks*blocksize + j];

    (hash_alg->hash_short)(hash, buff, extra, key, parm);
  }
  else
    (hash_alg->hash_short)(hash, buff, extra, key, parm);

  (hash_alg->hash_end)(hash, key, parm);

  ps_free(stream);

  ps_len_strategy = strategy;

  return (hash);
}

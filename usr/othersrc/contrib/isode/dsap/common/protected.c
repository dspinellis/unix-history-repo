/* protected.c - ProtectedPassword attribute syntax */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/protected.c,v 7.7 91/03/09 11:53:32 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/protected.c,v 7.7 91/03/09 11:53:32 mrose Exp $
 *
 *
 * $Log:	protected.c,v $
 * Revision 7.7  91/03/09  11:53:32  mrose
 * update
 * 
 * Revision 7.6  91/02/22  09:20:01  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/11/20  15:29:24  mrose
 * cjr
 * 
 * Revision 7.4  90/10/17  11:42:35  mrose
 * sync
 * 
 * Revision 7.3  90/01/12  08:08:26  mrose
 * again
 * 
 * Revision 7.2  90/01/11  23:53:12  mrose
 * lint
 * 
 * Revision 7.1  89/12/19  16:19:29  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:47:43  mrose
 * Release 6.0
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
#include "quipu/util.h"
#include "quipu/attr.h"
#include "quipu/authen.h"
#include "quipu/syntaxes.h"

extern LLog *log_dsap;
extern char dsa_mode;
char *cryptparse();

static PE prot_enc (x)
struct protected_password *x;
{
PE result = NULLPE;

	(void) encode_Quipu_ProtectedPassword (&result, 0, 0, NULLCP, x);
	return (result);
}

static struct protected_password * prot_dec (pe)
PE pe;
{
struct protected_password *result;

	if (decode_Quipu_ProtectedPassword (pe, 0, NULLIP, NULLVP, &result) 
		== NOTOK)
		return ((struct protected_password *) 0);
	return (result);
}

static struct protected_password *str2prot(str)
char *str;
{
struct protected_password *result;
char *octparse();

  result = (struct protected_password *)
	calloc(1, sizeof(*result));

  if (result == (struct protected_password *) 0)
	return (result);

  /* Using strlen means can't have zeros in the password */
  result->passwd = cryptparse(str);
  result->n_octets = strlen(result->passwd);
  result->is_protected[0] = '\0';
  result->time1 = NULLCP;
  result->time2 = NULLCP;
  result->random1 = (struct random_number *) 0;
  result->random2 = (struct random_number *) 0;

  return (result);
}

static prot_print (ps, parm, format)
PS ps;
struct protected_password *parm;
int format;
{
char *cp;
extern char * cryptstring();

  /* Make a null-terminated copy */
  cp = malloc((unsigned)(parm->n_octets + 1));
  bcopy(parm->passwd, cp, parm->n_octets);
  cp[parm->n_octets] = '\0';

  if (dsa_mode == FALSE) {
    if (format == READOUT) 
	    ps_print (ps, "HIDDEN");
    else
	    octprint(ps, cp, format);
  } else {
    ps_print(ps, "{CRYPT}");
    octprint(ps, cryptstring(cp), format);
  }
  free(cp);
}


/* Portable conversion from OCTET STRING to whatever structure is
 * used to hold a hash. This is currently an unsigned long, which limits the
 * length of a hash.
 */


/* The reverse operation. Currently, hashes are always 4 octets long. */

char *hash2str(hash, len)
unsigned long hash;
int *len;
{
char *result;
int i;

  result = malloc(5);
  if (result == NULLCP)
    return (result);

  for (i=0; i<4; i++)
  {
   result[i] = (char) (hash & 255);
   hash = hash >> 8;
  }

  *len = 4;
  return (result);
}

/* insecure hash function for testing purposes */

/* ARGSUSED */
unsigned long hash_passwd(seed, str, len)
unsigned long seed;
char *str;
int len;
{
  seed = 0;

  DLOG(log_dsap, LLOG_DEBUG, ("Hash = %D", seed));

  return (seed);
}

/* ARGSUSED */
int check_guard(pwd, pwd_len, salt, hval, hlen)
char *pwd; /* This string is not null-terminated */
int pwd_len;
char *salt; /* Null-terminated salt */
char *hval; /* This string is not null-terminated */
int hlen;
{
  return (2);
}

static int prot_cmp (a, b)
struct protected_password  *a, *b;
{
int retval;

 if (a->is_protected[0] == (char) 0)
   {
   if (b->is_protected[0] == (char) 0)
     {
     /* Both are unencrypted. Do a direct compare. */
     if (a->n_octets != b->n_octets)
       retval = 2;
     else
       retval = (strncmp(a->passwd, b->passwd, a->n_octets) == 0)? 0:2;
     }
   else
     retval = check_guard(a->passwd, a->n_octets, b->time1, b->passwd, b->n_octets);
   }
   else
   {
   if (b->is_protected[0] == (char) 0)
     retval = check_guard(b->passwd, b->n_octets, a->time1, a->passwd, a->n_octets);
   else
     {
     /* Both are encrypted. 
      * This case does not occur with sane usage of this syntax.
      * However, we have to handle it in case a DUA tries it.
      * To preserve semantics of `equals', should check whether a & b
      * are both guarded versions of the same thing, BUT the encryption
      * mechanism prevents us doing this check.
      * 
      * To make evrything mathematically correct, should re-write it
      * to use '>=' rather than '='. Unfortunately, can't check '>='
      * with a directory COMPARE operation ...
      */
     if (a->n_octets != b->n_octets)
       retval = 2;
     else
       retval = (strncmp(a->passwd, b->passwd, a->n_octets) == 0)? 0:2;
     }
   }
  return (retval);
}

static struct protected_password *prot_cpy(parm)
struct protected_password *parm;
{
struct protected_password *result;

  result = (struct protected_password *)
	calloc(1, sizeof(*result));

  result->passwd = malloc((unsigned)parm->n_octets);
  if (result->passwd == NULLCP)
    return ((struct protected_password *) 0);
  bcopy(parm->passwd, result->passwd, parm->n_octets);

  result->n_octets = parm->n_octets;
  if (parm->time1 == NULLCP)
	result->time1 = NULLCP;
  else
	result->time1 = strdup(parm->time1);

  if (parm->time2 == NULLCP)
	result->time2 = NULLCP;
  else
	result->time2 = strdup(parm->time2);

  result->random1 = (struct random_number *) 0;
  result->random2 = (struct random_number *) 0;

  result->is_protected[0] = parm->is_protected[0];

  return (result);
}

static prot_free(parm)
struct protected_password *parm;
{
  if (parm->passwd != NULLCP)
	free(parm->passwd);
  if (parm->time1 != NULLCP)
	free(parm->time1);
  if (parm->time2 != NULLCP)
	free(parm->time2);

  free((char *) parm);
}

protected_password_syntax ()
{
	(void) add_attribute_syntax ("ProtectedPassword",
		(IFP) prot_enc,	(IFP) prot_dec,
		(IFP) str2prot,	prot_print,
		(IFP) prot_cpy,	prot_cmp,
		prot_free,	NULLCP,
		NULLIFP,	FALSE);
}


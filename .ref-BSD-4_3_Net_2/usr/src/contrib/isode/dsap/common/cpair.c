/* cpair.c - CertificatePair attribute syntax */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/cpair.c,v 7.2 91/02/22 09:18:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/cpair.c,v 7.2 91/02/22 09:18:53 mrose Interim $
 *
 *
 * $Log:	cpair.c,v $
 * Revision 7.2  91/02/22  09:18:53  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/01/11  18:35:34  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  21:41:59  mrose
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


#include "quipu/authen.h"
#include "quipu/syntaxes.h"

/* We import these functions, which ought to be internal to certificate.c */
struct certificate *cert_cpy();
struct certificate *str2cert();

PE cpair_enc(parm)
struct certificate_list *parm;
{
PE pe;

  encode_AF_CertificatePair(&pe, 0, 0, NULLCP, parm);
  return (pe);
}

struct certificate_list *cpair_dec(pe)
PE pe;
{
struct certificate_list *result;

  if (decode_AF_CertificatePair(pe, 0, NULLIP, NULLVP, &result) == NOTOK)
	return (struct certificate_list *) 0;
  return (result);
}

struct certificate_list *str2cpair(str)
char *str;
{
struct certificate_list *result;
char *ptr;

  result = (struct certificate_list *) calloc(1, sizeof(*result));
  if (result == (struct certificate_list *) 0)
	return (result);
  

  /* If there isn't a '|', the pair is technically illegal. However,
   * allow this case to mean : "the string contains a certificate,
   * which is the forward cross certificate".
   */

  ptr = index(str, '|');
  if (ptr != NULLCP)
  {
    *ptr = '\0';
    ptr++;
  }

  /* Need to cook up a quick test for whether a string contains a certificate
   * or whitespace. A certificate will always contain a '#', so use this.
   */

  if (index(str, '#') != NULLCP)
  {
    result->cert = str2cert(str);
    if (result->cert == (struct certificate *) 0)
    {
      free((char *) result);
      return ((struct certificate_list *) 0);
    }
  }

  str = ptr;
  if ((str != NULLCP) && (index(str, '#') != NULLCP))
  {
    result->cert = str2cert(str);
    if (result->cert == (struct certificate *) 0)
    {
      free((char *) result);
      return ((struct certificate_list *) 0);
    }
  }

  return (result);
}

int printcpair(ps, parm, format)
PS ps;
struct certificate_list *parm;
int format;
{
  if (parm->cert)
	printcert(ps, parm->cert, format);

  ps_printf(ps, "|");

  if (parm->reverse)
	printcert(ps, parm->reverse, format);
}

struct certificate_list *cpair_cpy(parm)
struct certificate_list *parm;
{
struct certificate_list *result;

  result = (struct certificate_list *) calloc(1, sizeof(*result));
  if (result == (struct certificate_list *) 0)
	return (result);
  if (parm->cert)
	result->cert = cert_cpy(parm->cert);
  if (parm->reverse)
	result->reverse = cert_cpy(parm->reverse);

  return (result);
}


int cpair_cmp(a, b)
struct certificate_list *a, *b;
{
int retval;

  if (a->cert == (struct certificate *) 0)
  {
    if (b->cert == (struct certificate *) 0)
	retval = 0;
    else
        retval = 1;
  }
  else
  {
    if (b->cert == (struct certificate *) 0)
	retval = -1;
    else
	retval = cert_cmp(a->cert, b->cert);
  }

  if (retval != 0)
	return (retval);
    
  if (a->reverse == (struct certificate *) 0)
  {
    if (b->reverse == (struct certificate *) 0)
	retval = 0;
    else
        retval = 1;
  }
  else
  {
    if (b->reverse == (struct certificate *) 0)
	retval = -1;
    else
	retval = cert_cmp(a->reverse, b->reverse);
  }

  return (retval);
}

cpair_free(parm)
struct certificate_list *parm;
{
  if (parm->cert)
	cert_free(parm->cert);
  if (parm->reverse)
	cert_free(parm->reverse);
  free((char *) parm);
}

certificate_pair_syntax()
{
  (void) add_attribute_syntax(
	"CertificatePair",
	(IFP) cpair_enc,	(IFP) cpair_dec,
	(IFP) str2cpair,	printcpair,
	(IFP) cpair_cpy,	cpair_cmp,
	cpair_free,	NULLCP,
	NULLIFP,	TRUE);
}

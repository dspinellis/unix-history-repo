#include "defs.h"
#include "conv.h"

int badvalue;


/*  The following constants are used to check the limits of  */
/*  conversions.  Dmaxword is the largest double precision   */
/*  number which can be converted to a two-byte integer      */
/*  without overflow.  Dminword is the smallest double       */
/*  precision value which can be converted to a two-byte     */
/*  integer without overflow.  Dmaxint and dminint are the   */
/*  analogous values for four-byte integers.                 */

/* short array should be correct for both VAX and TAHOE */

LOCAL short dmaxword[] = { 0x47ff, 0xfeff, 0xffff, 0xffff }; /* 32767.5 */
LOCAL short dminword[] = { 0xc800, 0x007f, 0xffff, 0xffff }; /* -32768.499999999999 */

LOCAL short dmaxint[]  = { 0x4fff, 0xffff, 0xfeff, 0xffff }; /* 2147483647.5 */
LOCAL short dminint[]  = { 0xd000, 0x0000, 0x007f, 0xffff }; /* -2147483648.4999999 */

LOCAL short dmaxreal[] = { 0x7fff, 0xffff, 0x7fff, 0xffff }; /* 1.7014117838986683e+38 */
LOCAL short dminreal[] = { 0xffff, 0xffff, 0x7fff, 0xffff }; /* -1.7014117838986683e+38 */



/*  The routines which follow are used to convert  */
/*  constants into constants of other types.       */

LOCAL char *
grabbits(len, cp)
int len;
Constp cp;
{

  static char *toobig = "bit value too large";

  register char *p;
  register char *bits;
  register int i;
  register int k;
  register int lenb;

  bits = cp->const.ccp;
  lenb = cp->vleng->constblock.const.ci;

  p = (char *) ckalloc(len);

  if (len >= lenb)
    k = lenb;
  else
    {
      k = len;
      if ( badvalue == 0 )
	{
#if (HERE == PDP11 || HERE == VAX)
	  i = len;
	  while ( i < lenb && bits[i] == 0 )
	    i++;
	  if (i < lenb)
	    badvalue = 1;
#else
	  i = lenb - len - 1;
	  while ( i >= 0 && bits[i] == 0)
	    i--;
	  if (i >= 0)
	    badvalue = 1;
#endif
	  if (badvalue)
	    warn(toobig);
	}
    }

#if (HERE == PDP11 || HERE == VAX)
  i = 0;
  while (i < k)
    {
      p[i] = bits[i];
      i++;
    }
#else
  i = lenb;
  while (k > 0)
    p[--k] = bits[--i];
#endif

  return (p);
}



LOCAL char *
grabbytes(len, cp)
int len;
Constp cp;
{
  register char *p;
  register char *bytes;
  register int i;
  register int k;
  register int lenb;

  bytes = cp->const.ccp;
  lenb = cp->vleng->constblock.const.ci;

  p = (char *) ckalloc(len);

  if (len >= lenb)
    k = lenb;
  else
    k = len;

  i = 0;
  while (i < k)
    {
      p[i] = bytes[i];
      i++;
    }

  while (i < len)
    p[i++] = BLANK;

  return (p);
}



LOCAL expptr
cshort(cp)
Constp cp;
{
  static char *toobig = "data value too large";
  static char *reserved = "reserved operand assigned to an integer";
  static char *compat1 = "logical datum assigned to an integer variable";
  static char *compat2 = "character datum assigned to an integer variable";

  register expptr p;
  register short *shortp;
  register ftnint value;
  register long *rp;
  register double *minp;
  register double *maxp;
  realvalue x;

  switch (cp->vtype)
    {
    case TYBITSTR:
      shortp = (short *) grabbits(2, cp);
      p = (expptr) mkconst(TYSHORT);
      p->constblock.const.ci = *shortp;
      free((char *) shortp);
      break;

    case TYSHORT:
      p = (expptr) cpexpr(cp);
      break;

    case TYLONG:
      value = cp->const.ci;
      if (value >= MINWORD && value <= MAXWORD)
	{
	  p = (expptr) mkconst(TYSHORT);
	  p->constblock.const.ci = value;
	}
      else
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(toobig);
	    }
	  p = errnode();
	}
      break;

    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      minp = (double *) dminword;
      maxp = (double *) dmaxword;
      rp = (long *) &(cp->const.cd[0]);
      x.q.word1 = rp[0];
      x.q.word2 = rp[1];
      if (x.f.sign == 1 && x.f.exp == 0)
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(reserved);
	    }
	  p = errnode();
	}
      else if (x.d >= *minp && x.d <= *maxp)
	{
	  p = (expptr) mkconst(TYSHORT);
	  p->constblock.const.ci = x.d;
	}
      else
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(toobig);
	    }
	  p = errnode();
	}
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0 )
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      shortp = (short *) grabbytes(2, cp);
      p = (expptr) mkconst(TYSHORT);
      p->constblock.const.ci = *shortp;
      free((char *) shortp);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
clong(cp)
Constp cp;
{
  static char *toobig = "data value too large";
  static char *reserved = "reserved operand assigned to an integer";
  static char *compat1 = "logical datum assigned to an integer variable";
  static char *compat2 = "character datum assigned to an integer variable";

  register expptr p;
  register ftnint *longp;
  register long *rp;
  register double *minp;
  register double *maxp;
  realvalue x;

  switch (cp->vtype)
    {
    case TYBITSTR:
      longp = (ftnint *) grabbits(4, cp);
      p = (expptr) mkconst(TYLONG);
      p->constblock.const.ci = *longp;
      free((char *) longp);
      break;

    case TYSHORT:
      p = (expptr) mkconst(TYLONG);
      p->constblock.const.ci = cp->const.ci;
      break;

    case TYLONG:
      p = (expptr) cpexpr(cp);
      break;

    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      minp = (double *) dminint;
      maxp = (double *) dmaxint;
      rp = (long *) &(cp->const.cd[0]);
      x.q.word1 = rp[0];
      x.q.word2 = rp[1];
      if (x.f.sign == 1 && x.f.exp == 0)
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(reserved);
	    }
	  p = errnode();
	}
      else if (x.d >= *minp && x.d <= *maxp)
	{
	  p = (expptr) mkconst(TYLONG);
	  p->constblock.const.ci = x.d;
	}
      else
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(toobig);
	    }
	  p = errnode();
	}
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0 )
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      longp = (ftnint *) grabbytes(4, cp);
      p = (expptr) mkconst(TYLONG);
      p->constblock.const.ci = *longp;
      free((char *) longp);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
creal(cp)
Constp cp;
{
  static char *toobig = "data value too large";
  static char *compat1 = "logical datum assigned to a real variable";
  static char *compat2 = "character datum assigned to a real variable";

  register expptr p;
  register long *longp;
  register long *rp;
  register double *minp;
  register double *maxp;
  realvalue x;
  float y;

  switch (cp->vtype)
    {
    case TYBITSTR:
      longp = (long *) grabbits(4, cp);
      p = (expptr) mkconst(TYREAL);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = *longp;
      free((char *) longp);
      break;

    case TYSHORT:
    case TYLONG:
      p = (expptr) mkconst(TYREAL);
      p->constblock.const.cd[0] = cp->const.ci;
      break;

    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      minp = (double *) dminreal;
      maxp = (double *) dmaxreal;
      rp = (long *) &(cp->const.cd[0]);
      x.q.word1 = rp[0];
      x.q.word2 = rp[1];
      if (x.f.sign == 1 && x.f.exp == 0)
	{
	  p = (expptr) mkconst(TYREAL);
	  rp = (long *) &(p->constblock.const.cd[0]);
	  rp[0] = x.q.word1;
	}
      else if (x.d >= *minp && x.d <= *maxp)
	{
	  p = (expptr) mkconst(TYREAL);
	  y = x.d;
	  p->constblock.const.cd[0] = y;
	}
      else
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(toobig);
	    }
	  p = errnode();
	}
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0)
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      longp = (long *) grabbytes(4, cp);
      p = (expptr) mkconst(TYREAL);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = *longp;
      free((char *) longp);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
cdreal(cp)
Constp cp;
{
  static char *compat1 =
	"logical datum assigned to a double precision variable";
  static char *compat2 =
	"character datum assigned to a double precision variable";

  register expptr p;
  register long *longp;
  register long *rp;

  switch (cp->vtype)
    {
    case TYBITSTR:
      longp = (long *) grabbits(8, cp);
      p = (expptr) mkconst(TYDREAL);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[1] = longp[1];
      free((char *) longp);
      break;

    case TYSHORT:
    case TYLONG:
      p = (expptr) mkconst(TYDREAL);
      p->constblock.const.cd[0] = cp->const.ci;
      break;

    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      p = (expptr) mkconst(TYDREAL);
      longp = (long *) &(cp->const.cd[0]);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[1] = longp[1];
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0 )
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      longp = (long *) grabbytes(8, cp);
      p = (expptr) mkconst(TYDREAL);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[1] = longp[1];
      free((char *) longp);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
ccomplex(cp)
Constp cp;
{
  static char *toobig = "data value too large";
  static char *compat1 = "logical datum assigned to a complex variable";
  static char *compat2 = "character datum assigned to a complex variable";

  register expptr p;
  register long *longp;
  register long *rp;
  register double *minp;
  register double *maxp;
  realvalue re, im;
  int overflow;
  float x;

  switch (cp->vtype)
    {
    case TYBITSTR:
      longp = (long *) grabbits(8, cp);
      p = (expptr) mkconst(TYCOMPLEX);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[2] = longp[1];
      free((char *) longp);
      break;

    case TYSHORT:
    case TYLONG:
      p = (expptr) mkconst(TYCOMPLEX);
      p->constblock.const.cd[0] = cp->const.ci;
      break;

    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      overflow = 0;
      minp = (double *) dminreal;
      maxp = (double *) dmaxreal;
      rp = (long *) &(cp->const.cd[0]);
      re.q.word1 = rp[0];
      re.q.word2 = rp[1];
      im.q.word1 = rp[2];
      im.q.word2 = rp[3];
      if (((re.f.sign == 0 || re.f.exp != 0) &&
	   (re.d < *minp || re.d > *maxp))       ||
	  ((im.f.sign == 0 || re.f.exp != 0) &&
	   (im.d < *minp || re.d > *maxp)))
	{
	  if (badvalue <= 1)
	    {
	      badvalue = 2;
	      err(toobig);
	    }
	  p = errnode();
	}
      else
	{
	  p = (expptr) mkconst(TYCOMPLEX);
	  if (re.f.sign == 1 && re.f.exp == 0)
	    re.q.word2 = 0;
	  else
	    {
	      x = re.d;
	      re.d = x;
	    }
	  if (im.f.sign == 1 && im.f.exp == 0)
	    im.q.word2 = 0;
	  else
	    {
	      x = im.d;
	      im.d = x;
	    }
	  rp = (long *) &(p->constblock.const.cd[0]);
	  rp[0] = re.q.word1;
	  rp[1] = re.q.word2;
	  rp[2] = im.q.word1;
	  rp[3] = im.q.word2;
	}
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0)
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      longp = (long *) grabbytes(8, cp);
      p = (expptr) mkconst(TYCOMPLEX);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[2] = longp[1];
      free((char *) longp);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
cdcomplex(cp)
Constp cp;
{
  static char *compat1 = "logical datum assigned to a complex variable";
  static char *compat2 = "character datum assigned to a complex variable";

  register expptr p;
  register long *longp;
  register long *rp;

  switch (cp->vtype)
    {
    case TYBITSTR:
      longp = (long *) grabbits(16, cp);
      p = (expptr) mkconst(TYDCOMPLEX);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[1] = longp[1];
      rp[2] = longp[2];
      rp[3] = longp[3];
      free((char *) longp);
      break;

    case TYSHORT:
    case TYLONG:
      p = (expptr) mkconst(TYDCOMPLEX);
      p->constblock.const.cd[0] = cp->const.ci;
      break;

    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      p = (expptr) mkconst(TYDCOMPLEX);
      longp = (long *) &(cp->const.cd[0]);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[1] = longp[1];
      rp[2] = longp[2];
      rp[3] = longp[3];
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0 )
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      longp = (long *) grabbytes(16, cp);
      p = (expptr) mkconst(TYDCOMPLEX);
      rp = (long *) &(p->constblock.const.cd[0]);
      rp[0] = longp[0];
      rp[1] = longp[1];
      rp[2] = longp[2];
      rp[3] = longp[3];
      free((char *) longp);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
clogical(cp)
Constp cp;
{
  static char *compat1 = "numeric datum assigned to a logical variable";
  static char *compat2 = "character datum assigned to a logical variable";

  register expptr p;
  register long *longp;
  register short *shortp;
  register int size;

  size = typesize[tylogical];

  switch (cp->vtype)
    {
    case TYBITSTR:
      p = (expptr) mkconst(tylogical);
      if (tylogical == TYSHORT)
	{
	  shortp = (short *) grabbits(size, cp);
	  p->constblock.const.ci = (int) *shortp;
	  free((char *) shortp);
	}
      else
	{
	  longp = (long *) grabbits(size, cp);
	  p->constblock.const.ci = *longp;
	  free((char *) longp);
	}
      break;

    case TYSHORT:
    case TYLONG:
    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYLOGICAL:
      p = (expptr) cpexpr(cp);
      p->constblock.vtype = tylogical;
      break;

    case TYCHAR:
      if ( !ftn66flag && badvalue == 0 )
	{
	  badvalue = 1;
	  warn(compat2);
	}

    case TYHOLLERITH:
      p = (expptr) mkconst(tylogical);
      if (tylogical == TYSHORT)
	{
	  shortp = (short *) grabbytes(size, cp);
	  p->constblock.const.ci = (int) *shortp;
	  free((char *) shortp);
	}
      else
	{
	  longp = (long *) grabbytes(4, cp);
	  p->constblock.const.ci = *longp;
	  free((char *) longp);
	}
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



LOCAL expptr
cchar(len, cp)
int len;
Constp cp;
{
  static char *compat1 = "numeric datum assigned to a character variable";
  static char *compat2 = "logical datum assigned to a character variable";

  register expptr p;
  register char *value;

  switch (cp->vtype)
    {
    case TYBITSTR:
      value = grabbits(len, cp);
      p = (expptr) mkstrcon(len, value);
      free(value);
      break;

    case TYSHORT:
    case TYLONG:
    case TYREAL:
    case TYDREAL:
    case TYCOMPLEX:
    case TYDCOMPLEX:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat1);
	}
      p = errnode();
      break;

    case TYLOGICAL:
      if (badvalue <= 1)
	{
	  badvalue = 2;
	  err(compat2);
	}
      p = errnode();
      break;

    case TYCHAR:
    case TYHOLLERITH:
      value = grabbytes(len, cp);
      p = (expptr) mkstrcon(len, value);
      free(value);
      break;

    case TYERROR:
      p = errnode();
      break;
    }

  return (p);
}



expptr
convconst(type, len, const)
int type;
int len;
Constp const;
{
  register expptr p;

  switch (type)
    {
    case TYSHORT:
      p = cshort(const);
      break;

    case TYLONG:
      p = clong(const);
      break;

    case TYREAL:
      p = creal(const);
      break;

    case TYDREAL:
      p = cdreal(const);
      break;

    case TYCOMPLEX:
      p = ccomplex(const);
      break;

    case TYDCOMPLEX:
      p = cdcomplex(const);
      break;

    case TYLOGICAL:
      p = clogical(const);
      break;

    case TYCHAR:
      p = cchar(len, const);
      break;

    case TYERROR:
    case TYUNKNOWN:
      p = errnode();
      break;

    default:
      badtype("convconst", type);
    }

  return (p);
}

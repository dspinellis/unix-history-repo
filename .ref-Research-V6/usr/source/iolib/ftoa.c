ftoa (x, str, prec, format)
float x;
char *str;
{
/* converts a floating point number to an ascii string */
/* x is stored into str, which should be at least 30 chars long */
int ie, i, k, ndig, fstyle;
double y;
if (nargs() != 7)
  IEHzap("ftoa  ");
ndig = ( prec<=0) ? 7 : (prec > 22 ? 23 : prec+1);
if  (format == 'f' || format == 'F')
 fstyle = 1;
else
 fstyle = 0;
/* print in e format unless last arg is 'f' */
ie = 0;
/* if x negative, write minus and reverse */
if ( x < 0)
  {
  *str++ = '-';
  x = -x;
  }

/* put x in range 1 <= x < 10 */
if (x > 0.0) while (x < 1.0)
  {
  x =* 10.0;
  ie--;
  }
while (x >= 10.0)
  {
  x = x/10.0;
  ie++;
  }

/* in f format, number of digits is related to size */
if (fstyle) ndig =+ ie;

/* round. x is between 1 and 10 and ndig will be printed to
   right of decimal point so rounding is ... */
for (y = i = 1; i < ndig; i++)
  y = y/10.;
x =+ y/2.;
if (x >= 10.0) {x = 1.0; ie++;} /* repair rounding disasters */
/* now loop.  put out a digit (obtain by multiplying by
  10, truncating, subtracting) until enough digits out */
/* if fstyle, and leading zeros, they go out special */
if (fstyle && ie<0)
  {
  *str++ = '0'; *str++ = '.';
  if (ndig < 0) ie = ie-ndig; /* limit zeros if underflow */
  for (i = -1; i > ie; i--)
    *str++ = '0';
  }
for (i=0; i < ndig; i++)
  {
  k = x;
  *str++ = k + '0';
  if (i == (fstyle ? ie : 0)) /* where is decimal point */
    *str++ = '.';
  x =- (y=k);
  x =* 10.0;
  }

/* now, in estyle,  put out exponent if not zero */
if (!fstyle && ie != 0)
  {
  *str++ = 'E';
  if (ie < 0)
    {
    ie = -ie;
    *str++ = '-';
    }
  for (k=100; k > ie; k =/10);
  for (; k > 0; k =/10)
       {
       *str++ = ie/k + '0';
       ie = ie%k;
       }
  }
*str = '\0';
return;
}

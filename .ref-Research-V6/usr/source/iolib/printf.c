char  *_ptrbf, *_ptrst, *__fmt;
printf(a1,a2,a3,a4){
auto char  c, *s,  adj, *ptr,*p, buf[30];
extern cputc(),_putstr(), cout;
auto int  *adx, x, n, m, width, prec,i, padchar, fd;
double zz, *dblptr;
char (*f)();
_ptrbf = buf;

fd=cout;
adx = &a1;
f = cputc;
if (a1 == -1)
  {
  f = _putstr;
  _ptrst = a2;
  adx =+ 2;
  }
else if (a1 >= 0 && a1 <= 9)
  fd = *adx++;
__fmt = *adx++;


while( c = *__fmt++ ){
   if(c != '%') (*f)(c,fd);
   else { x = *adx++;
      if( *__fmt == '-' ){ adj = 'l';  __fmt++; }
      else adj = 'r';
   padchar = (*__fmt=='0') ? '0' : ' ';
      width = __conv();
      if( *__fmt == '.'){++__fmt; prec = __conv();}
      else prec = 0;

   s = 0;
   switch ( c = *__fmt++ ) {
     case 'D':
     case 'd':
	_prt1(x); break;
     case 'o':
     case 'O':
         _prnt8(x); break;
     case 'x':
     case 'X':
          _prntx(x); break;
      case 'S':
     case 's':    s=x;
        break;
     case 'C':
     case 'c':   *_ptrbf++ = x&0777;
         break;
     case 'E':
     case 'e':
     case 'F':
     case 'f':
      dblptr = adx-1;
      zz = *dblptr;
      adx =+ 3;
      ftoa (zz, buf, prec, c);
      prec = 0;
      s = buf;
     break;
     default:   (*f)(c,fd);
         adx--;
   }
   if (s == 0)
    {*_ptrbf = '\0'; s = buf;}
   n = _clenf (s);
   n = (prec<n && prec != 0) ? prec : n;
   m = width-n;
   if (adj == 'r') while (m-- > 0) (*f)(padchar,fd);
   while (n--) (*f)(*s++,fd);
   while (m-- > 0) (*f)(padchar,fd);
   _ptrbf = buf;
   }
}
if(a1 == -1) (*f)('\0',fd);
}


_prnt8 (n)
{ /* print in octal */
int p, k, sw;
if (n==0) {*_ptrbf++ = '0'; return;}
sw = 0;
for (p=15; p >= 0; p =- 3)
  if ((k = (n>>p)&07) || sw)
   {
    *_ptrbf++ = '0' + k;
     sw = 1;
     }
}
_prntx (n)
{
	int d,a;
	if (a = n>>4)
		_prntx ( a & 07777);
	d = n&017;
	*_ptrbf++ =  d > 9 ? 'A'+d-10 : '0' + d;
}

__conv()
{
auto c,n;
n = 0;
while( ((c = *__fmt++) >= '0') && (c<='9')) n = n*10+c-'0';
__fmt--;
return(n);
}

_putstr(chr,str){
*_ptrst++ = chr;
return; ieh305i(); /* force loading of dummy.s */
}
_prt1(n)
{
int digs[15], *dpt;
dpt = digs;
if (n >= 0)
   n = -n;
else
   *_ptrbf++ = '-';
for (; n != 0; n = n/10)
 *dpt++ = n%10;
if (dpt == digs)
   *dpt++ = 0;
while (dpt != digs)
   { --dpt;
   *_ptrbf++ =  '0' - *dpt;
}
}

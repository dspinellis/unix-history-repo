_Iint (iptr, length, numbase)
	int *iptr, length;
{
	int n, minus, numdig;
	extern int _Isfil, (*_Iungc)(), (*_Igetc)();
	int c, dval;

n = minus = numdig = 0;
switch ((c=_Inxch()))
	{
	case '-': minus = 1;
	case '+': break;
	default: (*_Iungc)(c,_Isfil);
	}
while ((dval=_Idigt(c=((*_Igetc)(_Isfil)), numbase ) ) >= 0 && numdig++ < length)
	n = n*numbase + dval;
(*_Iungc)(c,_Isfil);
if (numdig == 0)
	return (-1);
if (iptr == 0)
	return (0);
*iptr = minus ? -n : n;
return (1);
}

_Idigt (x, base)
{
switch (x)
	{
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
		return(x-'0');
	case '8':
	case '9':
		if (base > 8)
			return(x - '0');
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
		if (base >10)
			return(x - 'a' + 10);
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
		if (base > 10)
			return(x-'A' + 10);
	}
return(-1);
}

_Ilong (dptr, length)
	double *dptr;
	int length;
{
	char temp[75];
	int _Inodg();
	double x;
	double atof();

if (_Isstr(temp, length, _Inodg) < 0)
	return (-1);
x = atof(temp);
if (dptr == 0)
	return (0);
*dptr = x;
return (1);
}

_Isstr (sptr, length, stopf)
	char *sptr;
	int length, (*stopf)();
{
	int ch, initlen, _Inxch();
	extern int _Isfil, (*_Igetc)(), (*_Iungc)();

initlen = length;
if ((ch=_Inxch()) < 0)
	return (-1);
(*_Iungc)(ch,_Isfil);
while (!((*stopf)(ch=(*_Igetc)(_Isfil))) && length-- > 0)
	if (sptr != 0)
		*(sptr++) = ch;
if (ch >= 0)
	(*_Iungc)(ch,_Isfil);
if (length == initlen)
	return (-1);
if (sptr == 0)
	return (0);
*sptr = '\0';
return (1);
}

_Iestr (c)
char c;
{
if (_Ispce(c)) return (1);
if (c == '\0') return (1);
return (0);
}

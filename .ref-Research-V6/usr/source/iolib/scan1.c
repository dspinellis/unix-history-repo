scanf (p1, p2, p3, p4)
	int p1, p2, p3, p4;
{
/* first arg can be a control string, a file id, or -1 */
	int ptrs[10], j, ip, flp, k;
	char *np;
/*	extern int cin;*/
extern (*_Igetc)(), (*_Iungc)(), cgetc(), ungetc(), _Igstr(), _Iungs();
extern char *_Iinpt;
ip = 0;
if (p1 == -1)
  {k = 1; _Iinpt = p2;}
else if (p1 >= 0 && p1 < 10)
  k = 0;
else
  k = -1;
if (k <= 0)
  {_Igetc = cgetc; _Iungc = ungetc;}
else
  {_Igetc = _Igstr; _Iungc = _Iungs;}
j = 0;
for (np = (&p2)[k]; *np; np++)
    if (*np == '%' && *(np+1) != '%' && *(np+1) != '*')
	ptrs[ip++] = (&p3)[(j++)+k];
return (_Iscan ((k==0 ?  p1 : 0), (&p2)[k], ptrs));
}

_Iscan (fileid, format, listp)
	char *format;
	int *listp;
{
	char ch, _Inxch();
	int nmatch;
	extern int _Isfil;
	_Isfil = fileid;
nmatch = 0;
while (1) switch (ch= *format++)
	{
	case '\0': return (nmatch);
	case '%': switch (_Isfrm(&format, *listp++))
			{
			case 0: listp--; break;
			case -1: return (nmatch > 0 ? nmatch : -1);
			default: nmatch++;
			}
	case ' ':
	case '\n':
	case '\t': break;
	default: if (ch != _Inxch())
			return(nmatch);
	}
}

int _Isfil 0;

_Ichar (cptr)
	char *cptr;
{
	char ch, _Inxch();

if ((ch = _Inxch()) < 0)
	return (-1);
if (cptr == 0)
	return (0);
*cptr = ch;
return (1);
}

_Iflot (fptr, length)
	float *fptr;
	int length;
{
	char temp[75];
	int _Inodg();
	float x;
	double atof();

if (_Isstr(temp, length, _Inodg) < 0)
	return (-1);
x = atof(temp);
if (fptr == 0)
	return (0);
*fptr = x;
return (1);
}

_Inodg (ch)
char ch;
{
if (_Idigt(ch,10) >= 0) return (0);
switch (ch)
	{
	case 'E':
	case 'e':
	case '.': case '+': case '-':
		return (0);
	}
return (1);
}

_Isfrm (spec, pointer)
	char **spec;
	int pointer;
{
	int length, lflag, _Iestr(), _Ispnd();
	char ch;
length = lflag = 0;
while (1) switch (ch = *((*spec)++))
	{
	case '*': pointer=0; break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		length = length*10 + ch - '0' ;
		lflag++;
		break;
	case 'o': /* octal */
		return(_Iint(pointer, lflag ? length : 100, 8));
	case 'x': /* hex */
		return(_Iint(pointer, lflag ? length : 100, 16));
	case 'd': /* decimal */
		return (_Iint(pointer, lflag ? length : 100, 10));
	case 'c': /* character */
		return (_Ichar(pointer));
	case 's': /* string */
		return (_Isstr(pointer, lflag ? length : 100, _Iestr));
	case 'f':
	case 'e': /* float */
		return (_Iflot(pointer, lflag ? length : 100));
	case 'l': /*  (long) double or int */
		switch(*(*spec)++)
			{
			case 'f': case 'F':
			case 'e': case 'E':
				return (_Ilong (pointer, lflag ? length : 100));
			default: printf(2, "long not yet implemented\n");
				return(0);
			}
	case '[': /* special strings */
		_Imtab(spec);
		return (_Isstr (pointer, lflag ? length : 100, _Ispnd));
	case '%':
		if (_Inxch() != '%')
			return (-1);
		return(0);
	case '\0':
		_Ierr("scanf: bad format termination\n");
	default: _Ierr ("scanf: format character %c", ch);
	}
}

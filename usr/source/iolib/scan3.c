_Ierr (message, a, b, c, d, e)
char message[];
	{
	extern int cgoof;
	printf("ERROR ");
	printf(message, a, b, c, d, e);
	cputc('\n');
	cexit(cgoof);
	}
char _Iendm[128] {0};
_Imtab (formatp)
char **formatp;
{
/* make up special table of string ending characters */
int i, normal;
char ch;
/* normally all characters end string except those listed */
normal = 1;
if (**formatp == '^')
	{normal = 0; (*formatp)++;}
for (i= 0; i < 128; i++)
	_Iendm[i] = normal;
while ((ch = *((*formatp)++)) != ']')
	_Iendm[ch] = !_Iendm[ch];

}

_Inxch ()
/* returns next character which is not _Ispce */
{
	extern int _Isfil, (*_Igetc)();
        int ch;
while ((ch = (*_Igetc)(_Isfil)) > 0 && _Ispce(ch));
if  (ch > 0)
	return (ch);
return (-1);
}

_Ispce (c)
char c;
{
switch (c)
	{
	case ' ':
	case '\n':
	case '\t': return(1);
	}
return(0);
}

_Ispnd (ch)
char ch;
{
return (_Iendm[ch] > 0);
}

char *_Iinpt;
int (*_Igetc)(), (*_Iungc)();
_Igstr ()
{
extern char *_Iinpt;
return (*_Iinpt++);
}

_Iungs(ch)
{
extern char *_Iinpt;
*--_Iinpt = ch;
}

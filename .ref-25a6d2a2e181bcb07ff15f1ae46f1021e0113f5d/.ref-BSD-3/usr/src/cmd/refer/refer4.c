# include "refer..c"
static gate = 0;
static char buff[LLINE];
output (s)
	char *s;
{
if (gate)
	fputs(buff,ftemp);
else
	gate=1;
strcpy(buff,s);
if (strlen(buff)>LLINE)
	err("one buff too big (%d)!", LLINE);
}
append(s)
	char *s;
{
char *p, *r; int lch;
trimnl(buff);
for (p=buff; *p; p++)
	;
lch = *--p;
switch (lch)
	{
	case '.': case ',':
		*p=0;
		r="\\*(<";
		while (*r) *p++= *r++;
		*p++ = lch;
		*p=0;
	}
strcat(buff,s);
switch(lch)
	{
	case '.': case ',':
		for(p=buff; *p; p++)
			;
		if (*--p=='\n')*p=0;
		r = "\\*(>";
		while (*r) *p++ = *r++;
		*p++ = lch;
		*p++ = '\n';
		*p=0;
	}
if (strlen(buff)>LLINE)
	err("output buff too long (%d)", LLINE);
}

flout()
{
if (gate)
	fputs(buff,ftemp);
gate=0;
}

char *
trimnl(ln)
	char *ln;
{
register char *p = ln;
while (*p) p++;
p--;
if (*p == '\n') *p=0;
return(ln);
}

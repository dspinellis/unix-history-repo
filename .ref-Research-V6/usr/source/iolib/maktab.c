char IEH3endm[128] {0};
IEH3mtab (formatp)
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
	IEH3endm[i] = normal;
while ((ch = *((*formatp)++)) != ']')
	IEH3endm[ch] = !IEH3endm[ch];

}

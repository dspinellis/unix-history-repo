IEH3nodg (ch)
char ch;
{
if (IEH3digt(ch)) return (0);
switch (ch)
	{
	case 'E':
	case 'e':
	case '.': case '+': case '-':
		return (0);
	}
return (1);
}

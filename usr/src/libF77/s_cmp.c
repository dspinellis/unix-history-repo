int s_cmp(a, b, la, lb)	/* compare two strings */
char *a, *b;
long int la, lb;
{
char *aend, *bend;
aend = a + la;

if(la <= lb)
	{
	while(a < aend)
		if(*a != *b)
			return( *a - *b );
		else
			{ ++a; ++b; }

	}

else
	{
	bend = b + lb;
	while(b < bend)
		if(*a == *b)
			{ ++a; ++b; }
		else
			return( *a - *b );
	while(a < aend)
		if(*a != ' ')
			return(*a - ' ');
		else	++a;
	}
return(0);
}

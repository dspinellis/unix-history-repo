#define MAXINTLENGTH 10
char *icvt(value,ndigit,sign) long value; int *ndigit,*sign;
{	static char buf[MAXINTLENGTH+1];
	register int i;
	if(value>0) *sign=0;
	else if(value<0)
	{	value = -value;
		*sign= 1;
	}
	else
	{	*sign=0;
		*ndigit=1;
		buf[MAXINTLENGTH]='0';
		return(&buf[MAXINTLENGTH]);
	}
	for(i=MAXINTLENGTH-1;value>0;i--)
	{	*(buf+i)=(int)(value%10)+'0';
		value /= 10;
	}
	*ndigit=MAXINTLENGTH-1-i;
	return(&buf[i+1]);
}

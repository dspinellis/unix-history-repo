int	bct[4];
int	optr[4];
char	bsp[2048];

char	*obuf[4]	{bsp,
			bsp + 512,
			bsp + 1024,
			bsp + 1536
			};

int	nflush;

put(fil,string,n)
	char	*string;
{
	extern	utmp;
	int	i;
	char	*o;

/*printf("%d %c %d\n",fil,*string,n);/*DEBUG*/

	string--;

	if((i = optr[fil] + n - 512) >= 0) {
		n =- i;
		o = &obuf[fil][optr[fil]] -1;
		while(--n >= 0)
			*++o = *++string;
		optr[fil] = 512;
		flsh(fil);
		n = i;
	}

	o = &obuf[fil][optr[fil]] - 1;
	optr[fil] =+ n;

	while(--n >= 0) {
		*++o = *++string;
	}
	return(0);
}

flsh(fil)
{
	extern	tp[],utmp;

	if(optr[fil] <= 0)	return(optr[fil]);

	if(bct[fil]++ >= 128 && utmp == 0) {
		printf("Wraparound temp file %d\n",fil);
		dexit();
	}
	nflush++;
	if(write(tp[fil],obuf[fil],optr[fil]) != optr[fil])
		return(-1);
	optr[fil] = 0;
	return(0);
}


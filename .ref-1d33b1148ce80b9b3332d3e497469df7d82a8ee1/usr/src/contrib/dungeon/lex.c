#define FALSE	0
#define TRUE	1

lex_(inbuf, inlnt, outbuf, op, vbflag, lprscon)
	char inbuf[78];
	int outbuf[40], *inlnt, *op, *vbflag;
	int *lprscon;	/* added */
{
	/*
	 * lex - lexical analyzer, converted from fortran
	 *
	 * input: one line of ascii characters
	 * output: tokenized input, packed in radix-50 format
	 */

	char	j;
	int	cp, i, k, prsptr;
	static int	num601 = {601};

	for (i=0; i<40; i++)
		outbuf[i] = 0;
	*op = -1;
	prsptr = *lprscon - 1;
	/* printf("lex: inbuf=%s, inlnt=%d\n", inbuf, *inlnt); */

toknlp:
	*op += 2;
	cp = 0;
	while ((*lprscon)++ <= *inlnt) {
		j = inbuf[prsptr++];
		/* printf("lex: chr=%c\n", j); */
		if ((j == '.') || (j == ','))
			break;
		else if (j == ' ')
			if (cp)		/* if (cp != 0) */
				goto toknlp;
			else
				continue;   /* first token */
		else if ((j >= 'A') && (j <= 'Z'))
			j -= '@';
		else if (((j >= '1') && (j <= '9')) || (j == '-'))
			j -= 0x12;
		else {
			if (*vbflag)
				rspeak_(&num601);
			return(FALSE);
		}

		if (cp >= 6)
			/*
			 * ignore remainder of any token > 6 chars
			 */
			continue;
		/*
		 * pack three chars per word in radix-50 format
		 */
		k = *op + (cp/3) - 1;
		/* printf("*op=%d, cp=%d, k=%d\n", *op, cp, k); */
		switch (cp%3) {
			case 0:
				outbuf[k] += j * 1560;
			case 1:
				outbuf[k] += j * 39;
			case 2:
				outbuf[k] += j;
		}
		cp++;
	}
	if (*lprscon > *inlnt)
		*lprscon = 1;
	if (!cp)	/* if (cp == 0) */
		if (*op == 1)
			return(FALSE);   /* no valid tokens */
		else {
			*op -= 2;
			return(TRUE);
		};
}

/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)number.c	4.6 (Berkeley) 6/1/90";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>

#define	YES		1
#define	NO		0
#define	EOS		'\0'
#define	MAXNUM		65		/* biggest number we handle */

static char	*name1[] = {
	"",		"one",		"two",		"three",
	"four",		"five",		"six",		"seven",
	"eight",	"nine",		"ten",		"eleven",
	"twelve",	"thirteen",	"fourteen",	"fifteen",
	"sixteen",	"seventeen",	"eighteen",	"nineteen",
},
		*name2[] = {
	"",		"ten",		"twenty",	"thirty",
	"forty",	"fifty",	"sixty",	"seventy",
	"eighty",	"ninety",
},
		*name3[] = {
	"hundred",	"thousand",	"million",	"billion",
	"trillion",	"quadrillion",	"quintillion",	"sextillion",
	"septillion",	"octillion",	"nonillion",	"decillion",
	"undecillion",	"duodecillion",	"tredecillion",	"quattuordecillion",
	"quindecillion",		"sexdecillion",	
	"septendecillion",		"octodecillion",
	"novemdecillion",		"vigintillion",
};

main(argc,argv)
	int	argc;
	char	**argv;
{
	register int	cnt;
	char	line[MAXNUM * 2 + 2];		/* MAXNUM '.' MAXNUM '\0' */

	if (argc > 1)
		for (cnt = 1;cnt < argc;++cnt) {
			convert(argv[cnt]);
			puts("...");
		}
	else
		while (fgets(line,sizeof(line),stdin)) {
			convert(line);
			puts("...");
		}
	exit(0);
}

static
convert(line)
	char	*line;
{
	register int	len,
			ret;
	register char	*C,
			*fraction;

	for (fraction = NULL, C = line;*C && *C != '\n';++C)
		if (!isdigit(*C))
			switch(*C) {
			case '-':
				if (C != line)
					usage(NO);
				break;
			case '.':
				if (!fraction) {
					fraction = C + 1;
					*C = EOS;
					break;
				}
			default:
				usage(NO);
			}
	*C = EOS;
	if (*line == '-') {
		puts("minus");
		++line;
	}
	ret = NO;
	if (len = strlen(line)) {
		if (len > MAXNUM)
			usage(YES);
		ret = unit(len,line);
	}
	if (fraction && (len = strlen(fraction))) {
		if (len > MAXNUM)
			usage(YES);
		for (C = fraction;*C;++C)
			if (*C != '0') {
				if (ret)
					puts("and");
				if (unit(len,fraction)) {
					++ret;
					pfract(len);
				}
				break;
			}
	}
	if (!ret)
		puts("zero.");
}

static
unit(len,C)
	register int	len;
	register char	*C;
{
	register int	off,
			ret;

	ret = NO;
	if (len > 3) {
		if (len % 3) {
			off = len % 3;
			len -= off;
			if (number(C,off)) {
				ret = YES;
				printf(" %s.\n",name3[len / 3]);
			}
			C += off;
		}
		for (;len > 3;C += 3) {
			len -= 3;
			if (number(C,3)) {
				ret = YES;
				printf(" %s.\n",name3[len / 3]);
			}
		}
	}
	if (number(C,len)) {
		puts(".");
		ret = YES;
	}
	return(ret);
}

static
number(C,len)
	register char	*C;
	int	len;
{
	register int	val,
			ret;

	ret = 0;
	switch(len) {
	case 3:
		if (*C != '0') {
			++ret;
			printf("%s hundred",name1[*C - '0']);
		}
		++C;
		/*FALLTHROUGH*/
	case 2:
		val = (C[1] - '0') + (C[0] - '0') * 10;
		if (val) {
			if (ret++)
				putchar(' ');
			if (val < 20)
				fputs(name1[val],stdout);
			else {
				fputs(name2[val / 10],stdout);
				if (val % 10)
					printf("-%s",name1[val % 10]);
			}
		}
		break;
	case 1:
		if (*C != '0') {
			++ret;
			fputs(name1[*C - '0'],stdout);
		}
	}
	return(ret);
}

static
pfract(len)
	register int	len;
{
	static char	*pref[] = { "", "ten-", "hundred-" };

	switch(len) {
	case 1:
		puts("tenths.");
		break;
	case 2:
		puts("hundredths.");
		break;
	default:
		printf("%s%sths.\n",pref[len % 3],name3[len / 3]);
	}
}

static
usage(toobig)
	int	toobig;
{
	if (toobig)
		fprintf(stderr,"number: number too large, max %d digits.\n",MAXNUM);
	fputs("usage: number # ...\n",stderr);
	exit(-1);
}

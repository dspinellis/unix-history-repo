#ifndef lint
static char sccsid[] = "@(#)font.c	2.2 (CWI) 87/04/01";
#endif lint
# include "e.h"

setfont(ch1)
	char *ch1;
{
	yyval = ft;
	if (strcmp(ch1, "I") == 0) {	/* I and italic mean merely position 2 */
		*ch1 = '2';
		ft = ITAL;
	} else if (strcmp(ch1, "B") == 0) {	/* and similarly for B & bold */
		*ch1 = '3';
		ft = BLD;
	} else if (strcmp(ch1, "R") == 0) {	/* and R and roman */
		*ch1 = '1';
		ft = ROM;
	} else {
		ft = ROM;	/* assume it's a roman style */
	}
	ftp++;
	if (ftp >= &ftstack[10])
		error(FATAL, "font stack overflow");
	ftp->ft = ft;
	if (ch1[1] == 0) {	/* 1-char name */
		ftp->name[0] = *ch1;
		ftp->name[1] = '\0';
	} else
		sprintf(ftp->name, "(%s", ch1);
	dprintf(".\tsetfont %s %c\n", ch1, ft);
}

font(p1, p2)
	int p1, p2;
{
		/* old font in p1, new in ft */
	yyval = p2;
	lfont[yyval] = rfont[yyval] = ft==ITAL ? ITAL : ROM;
	ftp--;
	ft = p1;
}

globfont()
{
	char temp[20];

	getstr(temp, sizeof(temp));
	yyval = eqnreg = 0;
	if (strcmp(temp, "I") == 0 || strncmp(temp, "it", 2) == 0) {
		ft = ITAL;
		strcpy(temp, "2");
	} else if (strcmp(temp, "B") == 0 || strncmp(temp, "bo", 2) == 0) {
		ft = BLD;
		strcpy(temp, "3");
	} else if (strcmp(temp, "R") == 0 || strncmp(temp, "ro", 2) == 0) {
		ft = ROM;
		strcpy(temp, "1");
	} else { 
		ft = ROM;	/* assume it's a roman style */
	}
	ftstack[0].ft = ft;
	if (temp[1] == 0)	/* 1-char name */
		strcpy(ftstack[0].name, temp);
	else
		sprintf(ftstack[0].name, "(%.2s", temp);
}

fatbox(p)
	int p;
{
	extern float Fatshift;

	yyval = p;
	printf(".ds %d \\*(%d\\h'-\\w'\\*(%d'u+%gm'\\*(%d\n", p, p, p, Fatshift, p);
}

/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define TXT_TO_C	1	/* must be a number for MAC compiler */

#include "funcdefs.c"

#ifdef MAC
#include "vars.c"
#endif

#undef putchar	/* From jove.h via funcdefs.c, conflicts with STDIO */
#undef putc
#undef getc
#undef EOF
#undef FILE
#undef BUFSIZ
#undef NULL
#include <stdio.h>

matchcmd(choices, what)
register struct cmd	choices[];
register char	*what;
{
	register int	len;
	int	i,
		found = 0,
		save,
		exactmatch = -1;

	len = strlen(what);
	for (i = 0; choices[i].Name != 0; i++) {
		if (*what != *choices[i].Name)
			continue;
#ifdef MAC /* see "left-margin" and "left-margin-here" */
		if (strcmp(what, choices[i].Name) == 0)
#else
		if (strncmp(what, choices[i].Name, len) == 0)
#endif
		return i;
	}
	return -1;
}

#ifdef MAC
matchvar(choices, what)
register struct variable choices[];
register char	*what;
{
	register int	len;
	int	i,
		found = 0,
		save,
		exactmatch = -1;

	len = strlen(what);
	for (i = 0; choices[i].Name != 0; i++) {
		if (*what != *choices[i].Name)
			continue;
		if (strcmp(what, choices[i].Name) == 0)
			return i;
	}
	return -1;
}
#endif

char *
PPchar(c)
int	c;
{
	static char	str[16];
	char	*cp = str;

	if (c & 0200) {
		c &= ~0200;
		strcpy(cp, "M-");
		cp += 2;
	}
	if (c == '\033')
		strcpy(cp, "ESC");
#ifdef IBMPC
	else if (c == '\377')
	        strcpy(cp, "M");
#endif /* IBMPC */
	else if (c < ' ')
		(void) sprintf(cp, "C-%c", c + '@');
	else if (c == '\177')
		strcpy(cp, "^?");
	else
		(void) sprintf(cp, "%c", c);
	return str;
}

void
extract(into, from)
char	*into,
	*from;
{
	from += 2;	/* Past tab and first double quote. */
	while ((*into = *from++) != '"')
		into += 1;
	*into = 0;
}


void

#ifdef MAC
_main()		/* for Mac, so we can use redirection */
#else
main()
#endif
{
	FILE	*ifile,
		*of;
	char	line[100],
#ifdef MAC
		*which,
#endif
		comname[70];
	int	comnum,
		ch,
#ifdef MAC
		inmenu = 0,
#endif
		savech = -1,
		errors = 0;

	ifile = stdin;
	of = stdout;
	if (ifile == NULL || of == NULL) {
		printf("Cannot read input or write output.\n");
		exit(1);
	}
	while (fgets(line, sizeof line, ifile) != NULL) {
		if (strncmp(line, "#if", 3) == 0) {
			savech = ch;
			fprintf(of, line);
			continue;
		} else if (strncmp(line, "#else", 5) == 0) {
			if (savech == -1)
				fprintf(stderr, "WARNING: ifdef/endif mismatch!\n");
			else
				ch = savech;
			fprintf(of, line);
			continue;
		} else if (strncmp(line, "#endif", 6) == 0) {
			savech = -1;
			fprintf(of, line);
			continue;
#ifdef MAC
		} else if (strncmp(line, "#MENU", 5) == 0) {
			inmenu = 1;
			continue;
#endif
		} else if (strncmp(line, "\t\"", 2) != 0) {
			fprintf(of, line);
			ch = 0;
			continue;
		}
		extract(comname, line);
		if (strcmp(comname, "unbound") == 0)
			comnum = 12345;
		else {
#ifdef MAC
			which = "commands";
#endif
			comnum = matchcmd(commands, comname);
#ifdef MAC
			if (comnum < 0 && inmenu) {
				comnum = matchvar(variables, comname);
				which = "variables";
			}
#endif
			if (comnum < 0) {
#ifdef MAC
				fprintf(stderr, "Warning: cannot find item \"%s\".\n", comname);
#else
				fprintf(stderr, "Warning: cannot find command \"%s\".\n", comname);
#endif
				errors += 1;
				comnum = 12345;
			}
		}
#ifdef MAC
		if(inmenu) {
			if (comnum == 12345)
				fprintf(of, "	(data_obj *) 0,\n");
			else
				fprintf(of, "	(data_obj *) &%s[%d],\n",which, comnum);
		} 
		else {
#endif
		if (comnum == 12345)
			fprintf(of, "	(data_obj *) 0,                 /* %s */\n", PPchar(ch++));
		else
			fprintf(of, "	(data_obj *) &commands[%d],	/* %s */\n", comnum, PPchar(ch++));
	}
#ifdef MAC
	}
#endif
	fclose(of);
	fclose(ifile);
	exit(errors);
}

/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define TXT_TO_C	1	/* must be a number for MAC compiler */

#include <stdio.h>

#include "funcdefs.c"

#ifdef	MAC
#include "vars.c"
#endif

private int
matchcmd(choices, what)
register const struct cmd	choices[];
register char	*what;
{
	register int	i;

	for (i = 0; choices[i].Name != NULL; i++) {
		if (what[0] == choices[i].Name[0]
		&& strcmp(what, choices[i].Name) == 0)
			return i;
	}
	return -1;
}

#ifdef	MAC
matchvar(choices, what)
register struct variable choices[];
register char	*what;
{
	register int	len;
	int	i;

	len = strlen(what);
	for (i = 0; choices[i].Name != NULL; i++) {
		if (what[0] == choices[i].Name[0]
		&& strcmp(what, choices[i].Name) == 0)
			return i;
	}
	return -1;
}
#endif

static int
StartsWith(s, pre)
const char *s, *pre;
{
    return strncmp(s, pre, strlen(pre)-1) == 0;
}

private char *
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
	else if (c < ' ')
		(void) sprintf(cp, "C-%c", c + '@');
	else if (c == '\177')
		strcpy(cp, "^?");
	else
		(void) sprintf(cp, "%c", c);
	return str;
}

private void
extract(into, from)
char	*into,
	*from;
{
	from += 2;	/* Past tab and first double quote. */
	while ((*into = *from++) != '"')
		into += 1;
	*into = '\0';
}


void

#ifdef	MAC
_main()		/* for Mac, so we can use redirection */
#else
main()
#endif
{
	FILE	*ifile,
		*of;
	char	line[100],
		comname[70];
	int	comnum,
		ch = 0,
		savech = -1,
		errors = 0;
#ifdef	MAC
	char	*which;
	bool	inmenu = NO;
#endif

	ifile = stdin;
	of = stdout;
	if (ifile == NULL || of == NULL) {
		printf("Cannot read input or write output.\n");
		exit(1);
	}
	while (fgets(line, sizeof line, ifile) != NULL) {
		if (StartsWith(line, "#if")) {
			savech = ch;
			fprintf(of, line);
			continue;
		} else if (StartsWith(line, "#else")) {
			if (savech == -1)
				fprintf(stderr, "WARNING: ifdef/endif mismatch!\n");
			else
				ch = savech;
			fprintf(of, line);
			continue;
		} else if (StartsWith(line, "#endif")) {
			savech = -1;
			fprintf(of, line);
			continue;
#ifdef	MAC
		} else if (StartsWith(line, "#MENU")) {
			inmenu = YES;
			continue;
#endif
		} else if (!StartsWith(line, "\t\"")) {
			/* If unrecognized, pass and prepare to start new table */
			fprintf(of, line);
			ch = 0;
			continue;
		}
		extract(comname, line);
		if (strcmp(comname, "unbound") == 0) {
			comnum = -1;
		} else {
			comnum = matchcmd(commands, comname);
#ifdef	MAC
			which = "commands";
			if (comnum < 0 && inmenu) {
				comnum = matchvar(variables, comname);
				which = "variables";
			}
#endif
			if (comnum < 0) {
				fprintf(stderr, "Warning: cannot find \"%s\".\n", comname);
				errors += 1;
			}
		}
#ifdef	MAC
		if (inmenu) {
			if (comnum < 0)
				fprintf(of, "\t(data_obj *) NULL,\n");
			else
				fprintf(of, "\t(data_obj *) &%s[%d],\n",which, comnum);
		} else /*...*/
#endif
		{
			if (comnum < 0)
				fprintf(of, "\t(data_obj *) NULL,\t\t/* %s */\n", PPchar(ch));
			else
				fprintf(of, "\t(data_obj *) &commands[%d],\t/* %s */\n", comnum, PPchar(ch));
			ch += 1;
		}
	}
	fclose(of);
	fclose(ifile);
	exit(errors);
}

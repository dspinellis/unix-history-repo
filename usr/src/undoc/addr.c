# include	<stdio.h>
# include	<ctype.h>

# define	reg	register
# define	bool	char

# define	TRUE	1
# define	FALSE	0

static char	*sccsid ="@(#)addr.c	1.8 (Berkeley) 4/10/81";

struct fd {
	char	*f_name;
	char	*f_desc;
	char	*f_value;
};

typedef struct fd	FDES;

char	*Fmtfile,
	*Addrfile,
	Fmt[BUFSIZ],
	*malloc();

FDES	Fmtab[BUFSIZ],
	*name();

main(ac, av)
int	ac;
char	**av;
{
	setbuf(stdout, 0);
	if (ac != 3) {
		printf("usage: %s fmt-file addr-file\n", av[0]);
		exit(1);
	}
	Fmtfile = av[1];
	Addrfile = av[2];
	parsefmt();
	doaddrs();
}

/*
 * parse the fmt file
 */
parsefmt()
{
	reg FILE	*inf;
	reg char	*sp, *fmt;
	reg FDES	*fp;
	reg bool	inquest, not;
	char		buf[80];

	if ((inf = fopen(Fmtfile, "r")) == NULL) {
		perror(Fmtfile);
		exit(1);
	}
	for (fp = Fmtab; fp < &Fmtab[BUFSIZ]; fp++)
		fp->f_name = NULL;

	inquest = FALSE;
	for (fmt = Fmt; (*fmt = getc(inf)) != EOF; fmt++)
		if (*fmt == '<' || *fmt == '?') {
			register char	c;

			if (inquest && *fmt == '?') {
				inquest = FALSE;
				if ((c = getc(inf)) == '\n') {
					*fmt++ = '\n';
					*fmt = '?';
				}
				else
					ungetc(c, inf);
				continue;
			}
			if (*fmt == '?')
				if ((c = getc(inf)) == '!')
					*fmt = '!';
				else
					ungetc(c, inf);
			sp = buf;
			while ((*sp = getc(inf)) != '>' && *sp != ':')
				sp++;
			c = *sp;
			*sp++ = '\0';
			fp = name(buf);
			fp->f_name = malloc(sp - buf);
			strcpy(fp->f_name, buf);
			if (c == ':' && *fmt == '<') {
				for (sp = buf; (*sp = getc(inf)) != '>'; sp++)
					continue;
				*sp++ = 0;
				fp->f_desc = malloc(sp - buf);
				strcpy(fp->f_desc, buf);
			}
			else if (*fmt == '?' || *fmt == '!')
				inquest = TRUE;
			else
				fp->f_desc = "";
			*++fmt = fp - Fmtab;
		}
	fclose(inf);
	*fmt = '\0';
# ifdef DEBUG
	printf("---\n");
	inquest = FALSE;
	for (fmt = Fmt; *fmt; fmt++) {
		putchar(*fmt);
		if (*fmt == '?' || *fmt == '!') {
			if (!inquest)
				printf("%d", *++fmt);
			inquest = !inquest;
		}
		else if (*fmt == '<')
			printf("%d", *++fmt);
	}
	printf("---\n");
	inquest = FALSE;
	for (fmt = Fmt; *fmt; fmt++)
		if (*fmt == '<') {
			fp = &Fmtab[*++fmt];
			printf("<%s", fp->f_name);
			if (strlen(fp->f_desc))
				printf(":%s", fp->f_desc);
			putchar('>');
		}
		else if (*fmt == '?' || *fmt == '!')
			if (!inquest) {
				fp = &Fmtab[*++fmt];
				printf("%c%s:", *fmt, fp->f_name);
				inquest = TRUE;
			}
			else
				inquest = FALSE;
		else
			putchar(*fmt);
	printf("---\n");
# endif
}

doaddrs()
{
	reg FILE	*inf;
	reg char	*sp;
	reg FDES	*fp;
	reg int		len;
	reg bool	justprinted;
	char		buf[BUFSIZ];

	if ((inf = fopen(Addrfile, "r")) == NULL) {
		perror(Addrfile);
		exit(1);
	}

	for (fp = Fmtab; fp->f_name != NULL; fp++)
		fp->f_value = NULL;

	while (fgets(buf, BUFSIZ, inf) != NULL) {
		justprinted = FALSE;
		buf[strlen(buf)-1] = '\0';
		if (strcmp(buf, "$") == 0) {
			printaddr();
			for (fp = Fmtab; fp->f_name != NULL; fp++)
				if (fp->f_value != NULL) {
					cfree(fp->f_value);
					fp->f_value = NULL;
				}
			justprinted = TRUE;
			continue;
		}
		for (sp = buf; !isspace(*sp) && *sp != '\0'; sp++)
			continue;
		len = sp - buf;
		for (fp = Fmtab; fp->f_name != NULL; fp++)
			if (strncmp(fp->f_name, buf, len) == 0) {
				while (isspace(*sp))
					sp++;
				if ((len = strlen(sp)) == 0)
					fp->f_value = NULL;
				else {
					fp->f_value = malloc(len + 1);
					strcpy(fp->f_value, sp);
				}
				break;
			}
	}
	if (!justprinted)
		printaddr();
}

printaddr()
{
	reg char	*sp;
	reg FDES	*fp;
	reg bool	printout, inquest;
	char		buf[80];

	printout = TRUE;
	inquest = FALSE;
	for (sp = Fmt; *sp; sp++)
		if (*sp == '<' || *sp == '?' || *sp == '!') {
			register char	c;

			if (*sp == '?' && inquest) {
				inquest = FALSE;
				printout = TRUE;
				continue;
			}
			c = *sp++;
			fp = &Fmtab[*sp];
			if (c == '<' && printout) {
				sprintf(buf, "%%%ss", fp->f_desc);
				printf(buf, fp->f_value);
			}
			else if (c == '?' || c == '!') {
				inquest = TRUE;
				printout = (fp->f_value != NULL);
				if (c == '!')
					printout = !printout;
			}
		}
		else if (printout == TRUE)
			putchar(*sp);
}

FDES *
name(str)
reg char	*str; {

	reg FDES	*fp;

	for (fp = Fmtab; fp->f_name != NULL; fp++)
		if (strcmp(str, fp->f_name) == 0)
			return fp;
	return fp;
}

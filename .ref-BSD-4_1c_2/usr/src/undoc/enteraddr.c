# include	<stdio.h>
# include	<ctype.h>

# define	reg	register
# define	bool	char

# define	TRUE	1
# define	FALSE	0

static char	*sccsid ="@(#)enteraddr.c	1.6 (Berkeley) 4/10/81";

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
	parseaddr();
	doaddrs();
	putchar('\n');
}

/*
 * parse the fmt file
 */
parseaddr()
{
	reg FILE	*inf;
	reg char	*sp;
	reg FDES	*fp;
	reg char	c;
	char		buf[80];

	if ((inf = fopen(Fmtfile, "r")) == NULL) {
		perror(Fmtfile);
		exit(1);
	}
	for (fp = Fmtab; fp < &Fmtab[BUFSIZ]; fp++)
		fp->f_name = NULL;

	while ((c = getc(inf)) != EOF)
		if (c == '<') {
			register char	c;

			sp = buf;
			while ((*sp = getc(inf)) != '>' && *sp != ':')
				sp++;
			c = *sp;
			*sp++ = '\0';
			fp = name(buf);
			fp->f_name = malloc(sp - buf);
			strcpy(fp->f_name, buf);
			if (c == ':') {
				for (sp = buf; (*sp = getc(inf)) != '>'; sp++)
					continue;
				*sp++ = 0;
				fp->f_desc = malloc(sp - buf);
				strcpy(fp->f_desc, buf);
			}
			else
				fp->f_desc = NULL;
		}
	fclose(inf);
}

doaddrs()
{
	reg FILE	*outf;
	reg char	*sp;
	reg FDES	*fp;
	reg int		len;
	char		buf[BUFSIZ];

	if ((outf = fopen(Addrfile, "a")) == NULL) {
		perror(Addrfile);
		exit(1);
	}

	for (fp = Fmtab; fp->f_name != NULL; fp++)
		fp->f_value = NULL;

	for (;;) {
		for (fp = Fmtab; fp->f_name != NULL; fp++) {
			printf("%s", fp->f_name);
			if (fp->f_value != NULL)
				printf(" (%s)", fp->f_value);
			else if (fp->f_desc != NULL)
				printf(" (%s)", fp->f_desc);
			printf(": ");
			if (fgets(buf, BUFSIZ, stdin) == NULL)
				return;
			buf[strlen(buf)-1] = '\0';
			if (buf[0] == '\0')
				if (fp->f_value == NULL)
					if (fp->f_desc != NULL)
						strcpy(buf, fp->f_desc);
					else
						strcpy(buf, "");
				else
					continue;
			else if (fp->f_value != NULL)
				cfree(fp->f_value);
			fp->f_value = malloc(strlen(buf) + 1);
			strcpy(fp->f_value, buf);
		}
		putchar('\n');
		for (fp = Fmtab; fp->f_name != NULL; fp++)
			printf("%s: %s\n", fp->f_name, fp->f_value);
		printf("correct? ");
		fgets(buf, BUFSIZ, stdin);
		if (buf[0] == 'y') {
			for (fp = Fmtab; fp->f_name != NULL; fp++) {
				fprintf(outf, "%s", fp->f_name);
				if (*fp->f_value != '\0')
					fprintf(outf, " %s", fp->f_value);
				putc('\n', outf);
				cfree(fp->f_value);
				fp->f_value = NULL;
			}
			fprintf(outf, "$\n");
		}
		putchar('\n');
	}
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

#ifndef lint
static char Notice[] = "Copyright (c) 1984, 1985 Adobe Systems Incorporated";
static char *RCSID="$Header: psdit.c,v 2.1 85/11/24 11:50:41 shore Rel $";
#endif
# define XMOD
/* psdit.c
 *
 * Copyright (c) 1984, 1985 Adobe Systems Incorporated

.
.
.
.

private devcntrl(fp)	/* interpret device control functions */
FILE *fp;
{
    char    str[20], str1[50], buf[50];
    int     c, n, res, minh, minv;

    fscanf (fp, "%s", str);
    switch (str[0]) {		/* crude for now */
	case 'i': 		/* initialize */
	    fileinit ();
	    t_init ();
.
.
.
.
.
	    break;
	case 'S': 		/* slant */
	    fscanf (fp, "%d", &n);
	    t_slant (n);
	    lastcmd = FNT;
	    break;
#ifdef XMOD
	case 'X': {		/* \X command from ditroff */
            int last;
	    char largebuf[128];
	    fscanf (fp, "%1s", str);
	    switch (str[0]) {
		case 'p' :
		    FlushShow(0);MoveTo();DoMove();
		    fgets(largebuf, sizeof(largebuf), fp);
		    last = strlen(largebuf) - 1;
		    if (last >= 0 && largebuf[last] == '\n') {
			ungetc('\n', fp);
			largebuf[last] = ' ';
		    }
		    fputs(largebuf, tf);
		    putc('\n', tf);
		    break;
		case 'f' :
		    FlushShow(0);MoveTo();DoMove();
		    if (fscanf(fp, "%s", largebuf) == 1) {
			char *nl = (char *) index(largebuf, '\n');
			if (nl) *nl = '\0';
			includefile(largebuf);
		    } else
			fprintf(stderr, "warning - include cmd w/o path.\n");
		    break;
	    }
	}
	break;
#endif
    }
    /* skip rest of input line */
    while ((c = getc (fp)) != '\n') {if (c == EOF) break;};
}

#ifdef XMOD
includefile(filenm)
char *filenm; {

	FILE *inf;
	int ch, c1, c2, firstch = 0;

	if (!(inf = fopen(filenm, "r"))) {
		fprintf(stderr, "psdit: fopen(%s): ", filenm);
		perror();
		exit(1);
	}
	c1 = fgetc(inf); c2 = fgetc(inf);
	if (c1 != '%' || c2 != '!')
		fprintf(stderr, "psdit: %s not a postscript file.\n", filenm),
		exit(1);

	fputs("%!", tf);
	while ((ch = fgetc(inf)) != EOF) {
		fputc(ch, tf);
		if (firstch && ch == '%') {
			/* we have to double leading '%'s */
			fputc('%', tf);
		}
		firstch = (ch == '\n');
	}			
	fclose(inf);
}
#endif
private fileinit()	/* read in font and code files, etc. */
{

.
.
.
.

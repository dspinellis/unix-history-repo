/*
**  TSET -- set terminal modes
**
**	Eric Allman -- 10/77
*/

char	Erase_char;		/* new erase character */
char	Kill_char;		/* new kill character */

struct ttymode
{
	char	ibaud, obaud;	/* input & output baud rates */
	char	erase, kill;	/* erase and kill characters */
	int	mode;		/* other modes */
};

char	Ttyn;			/* terminal number */
char	*Ttytype;		/* type of terminal */
char	*Dialtype;		/* override type if dialup terminal */
int	Dash_u;			/* update htmp flag */
int	Dash_h;			/* read htmp flag */

char	Usage[]	"usage: tset [-eC] [-kC] [-fT] [-dT] [-tN] [-h] [-u]\n";

# define	BACKSPACE	('H' & 037)
# define	CONTROLX	('X' & 037)
# define	FILEDES		1

char	Capbuf[256];		/* line from /etc/ttycap for this Ttytype */


main(argc, argv)
int	argc;
char	*argv[];
{
	struct ttymode	mode;
	char		buf[256];
	char		*bufp;
	register char	*p;
	register int	i;
	int		error;
	int		mdvect[2];
	extern char	ttycap[];

	/* scan argument list and collect flags */
	error = 0;
	while ((p = *++argv) != -1)
	{
		if (p[0] == '-')
		{
			switch (p[1])
			{

			  case 'e':	/* erase character */
				if (p[2] == 0)
					Erase_char = BACKSPACE;
				else
					Erase_char = p[2];
				continue;

			  case 'k':	/* kill character */
				if (p[2] == 0)
					Kill_char = CONTROLX;
				else
					Kill_char = p[2];
				continue;

			  case 'f':	/* force processing */
				if (p[2] == 0)
					Ttytype = "pd";
				else
					Ttytype = &p[2];
				continue;

			  case 'd':	/* dialup type */
				Dialtype = &p[2];
				continue;

			  case 't':	/* terminal number */
				Ttyn = p[2];
				continue;

			  case 'h':	/* get type from htmp */
				Dash_h++;
				continue;

			  case 'u':	/* update htmp */
				Dash_u++;
				continue;

			}
		}
		prs("Bad flag ");
		prs(p);
		prs("\n");
		error++;
	}

	if (error)
	{
		prs(Usage);
		exit(1);
	}

	/* change standard output if -t flag specified */
	if (Ttyn != 0)
	{
		p = "/dev/ttyx";
		p[8] = Ttyn;
		i = open(p, 1);
		if (i < 0)
		{
			prs("Cannot open ");
			prs(p);
			prs("\n");
			exit(1);
		}
		close(FILEDES);
		dup(i);
		close(i);
	}

	/* find terminal type */
	if (Ttytype == 0)
	{
		/* -f not specified */
		if (Ttyn == 0)
		{
			/* no -t flag either */
			Ttyn = ttyn(FILEDES);
		}

		/* get type from /etc/ttytype or /etc/htmp */
		if (Dash_h)
		{
			hget(Ttyn);
			Ttytype = hsgettype();
		}
		else
		{
			Ttytype = stypeof(Ttyn);
		}

		/* check for dialup override */
		if (Dialtype != 0 && Ttytype[0] == 'd' && Ttytype[1] == 'u')
		{
			Ttytype = Dialtype;
			Dash_u++;
		}
	}

	/* Ttytype now contains a pointer to the type of the terminal */
	if (gtty(FILEDES, &mode) < 0)
	{
		prs("Standard output not a terminal\n");
		exit(1);
	}

	/* get terminal capabilities */
	switch (tgetent(Capbuf, Ttytype))
	{

	  case -1:
		prs("Cannot open ");
		prs(ttycap);
		prs("\n");
		exit(-1);

	  case 0:
		prs("Type ");
		prs(Ttytype);
		prs(" unknown\n");
		exit(1);
	}

	/* determine erase and kill characters */
	if (Erase_char == 0)
	{
		if (mode.erase == 0)
			Erase_char = '#';
		if (tgetflag("bs") && !tgetflag("os"))
			Erase_char = BACKSPACE;
	}
	setek("Erase", &mode.erase, Erase_char);

	if (Kill_char == 0 && mode.kill == 0)
		Kill_char = '@';
	setek("Kill", &mode.kill, Kill_char);

	/* set modes */
	tgetmodes(mdvect);
	mode.mode =& ~mdvect[0];
	mode.mode =| mdvect[1];
	stty(FILEDES, &mode);

	/* output startup string */
	bufp = buf;
	if (tgetstr("if", &bufp) != 0)
		cat(buf);
	bufp = buf;
	if (tgetstr("is", &bufp) != 0)
		prs(buf);

	/* update htmp */
	if (Dash_u)
	{
		if (Ttyn == 0)
			Ttyn = ttyn(FILEDES);
		if (Ttyn == 'x')
			prs("Cannot update htmp\n");
		else
		{
			if (!Dash_h)
			{
				/* get old htmp entry */
				if (hget(Ttyn) < 0)
				{
					perror("htmp");
					exit(-1);
				}
			}
			hsettype(Capbuf[0] | (Capbuf[1] << 8));
			hput(Ttyn);
		}
	}

	exit(0);
}


setek(name, old, new)
char	*name;
char	*old;
char	new;
{
	register char	*p;

	if (new == 0 || new == *old)
		return;
	*old = new;
	prs(name);
	prs(" set to ");
	if (new < 040)
	{
		prs("control-");
		new = (new & 037) | 0100;
	}
	p = "x\n";
	p[0] = new;
	prs(p);
}


prs(s)
char	*s;
{
	register char	*p;
	register char	*q;
	register int	i;

	p = q = s;
	i = 0;
	while (*q++ != 0)
		i++;

	if (i > 0)
		write(FILEDES, p, i);
}


cat(file)
char	*file;
{
	register int	fd;
	register int	i;
	char		buf[512];

	fd = open(file, 0);
	if (fd < 0)
	{
		prs("Cannot open ");
		prs(file);
		prs("\n");
		exit(-1);
	}

	while ((i = read(fd, buf, 512)) > 0)
		write(FILEDES, buf, i);

	close(fd);
}

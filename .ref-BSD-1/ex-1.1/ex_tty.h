/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

int	TTY;

char	*CLEAR, *NDSPACE, *UPLINE, *HSTR, *CE;

char	CA, AM, BS, OS, PT, NOCR;
int	LINES, COLUMNS;

char	UPPERCASE;

char	ttytype[];

int	outcol, outline, destcol, destline;

/*
 * As yet unused capabilities...
 *
char	*AL, *DL, *CD;
char	IT;
int	MODES[2];
 */

/**
 **	open a file
 **/

int	__Filemode	0644;

copen(filename, xmode)
char	*filename;
int	xmode;
{
	register int	fn;
	register int	mode;

	mode = xmode;
	switch (mode)
	{

	  case 'r':		/* read mode */
		mode = 0;
	  case 0:
		fn = open(filename, 0);
		break;

	  case 'm':		/* modify */
		mode = 3;
	  case 3:
		fn = open(filename, 2);
		break;

	  case 'a':		/* append */
		mode = 2;
	  case 2:
		fn = open(filename, 1);
		if (fn >= 0)
			break;

	  case 'w':		/* write */
		mode = 1;
	  case 1:
		fn = creat(filename, __Filemode);
		break;

	  default:
		__error("copen: bad file mode %o on %s", mode, filename);
	}
	if (fn >= 0)
		__makbuf(fn, mode);
	return (fn);
}

copen (filename, type)
char *filename;
int type;
{
int fn;
switch (type)
	{
	case 'r': type = 0;
	case 0:	  fn = open(filename,0);
		break;
	case 'a': type = 2;
	case 2:	if ((fn=open(filename,1))>=0)
			break;	/* courtesy of sny */
	/* append defaults to write if file missing */
	case 'w': type = 1;
	case 1: fn = creat(filename,0666);
		break;
	default: IEH3err("copen: bad file %s",filename);
	}
if (fn >= 0)
	IEH3mbuf(fn,type);
return(fn);
}

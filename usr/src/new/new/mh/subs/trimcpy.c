char *trimcpy(cp)
register char *cp;
{
	register char *sp;

	while(*cp == ' ' || *cp == '\t')
		cp++;
	sp = cp;
	while(*sp)
		if(*sp++ == '\n')
			break;
	*--sp = 0;
	sp = (char *) malloc(sp - cp + 1);
	strcpy(sp, cp);
	return(sp);
}

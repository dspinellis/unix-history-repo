char *getcpy(str)
{
	register char *cp;

	cp = (char *) malloc(strlen(str) + 1);
	strcpy(cp, str);
	return(cp);
}

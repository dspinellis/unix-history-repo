char *add(this, that)
register char *this, *that;
{
	register char *r;

	if(!this)
		this = "";
	if(!that)
		that = "";
	r = (char *) malloc(strlen(this)+strlen(that)+1);
	sprintf(r, "%s%s", that, this);
	if(*that)
		cndfree(that);
	return(r);
}

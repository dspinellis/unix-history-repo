ssequal(substr, str)
char *substr, *str;
{
	while(*substr)
		if(*substr++ != *str++)
			return(0);
	return(1);
}

getargs(s, arps)
	char *s, *arps[];
{
	int i;
i = 0;
while (1)
	{
	arps[i++]=s;
	while (*s != 0 && *s!=' '&& *s != '\t')s++;
	if (*s==0) break;
	*s++ =0;
	while (*s==' ' || *s=='\t')s++;
	if (*s==0)break;
	}
return(i);
}

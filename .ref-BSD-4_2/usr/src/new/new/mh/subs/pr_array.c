pr_array(cp,ap)
char *cp,  **ap;
{
	register  int  i;

	for(i=0;  *ap;  ap++,i++)
		printf("%s[%d]=> %s\n", cp,i,*ap);
}


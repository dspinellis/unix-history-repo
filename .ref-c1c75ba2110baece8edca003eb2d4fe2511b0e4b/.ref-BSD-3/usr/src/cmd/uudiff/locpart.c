main(argc,argv)
	char *argv[];
{
char *p;
for (p=argv[1]; *p; p++);
while (*p != '/') p--;
printf("%s\n", p+1);
}

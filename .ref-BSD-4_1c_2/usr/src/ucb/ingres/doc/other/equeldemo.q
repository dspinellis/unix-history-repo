/*
*  This is a sample EQUEL program. It assumes the
*  presence of the EMPLOYEE relation inside the
*  DEMO data base. To use the program, type an
*  employee name and the program will either respond
*  with the SALARY or with the message that
*  the employee is not in the data base.
*  Typing a '?' will result in the list of
*  all names in the data base being printed.
*
*  Exit by typing a control-d
*
*  To compile and run this program do the following:
*
*  equel equeldemo.q
*  cc    equeldemo.c -lq
*  a.out
*/
main()
{
##	char	NAME[20];
##	int	SAL;
	char	flag;

##	ingres demo
##	range of e is employee
	while (eread(NAME))
	{
		if(NAME[0] == '?')
		{
##			retrieve (NAME=e.name)
##			{
				printf("%s\n",NAME);
##			}
			continue;
		}
		flag = 0;
##		retrieve (SAL = e.salary) where
##		e.name = NAME
##		{
			printf("The salary of %s is %d\n",NAME,SAL);
			flag = 1;
##		}
	if(!flag) printf("%s is not in the data base\n",NAME);
	}
##	exit
}

eread(p)
char	*p;
{
	char	c;
	printf("enter name:");
	while(c = getchar())
	{
		if(c == '\n')
		{
			*p = 0;
			return(1);
		}
		*p++ = c;
	}
	return(0);
}

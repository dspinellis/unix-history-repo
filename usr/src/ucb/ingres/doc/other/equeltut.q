#
/*
** This	is intended as a tutorial example of  an  Equel	 program.
** You	should	be  familiar  with  both  C and	Quel before going
** through the examples.  The program may be run to see	 how  the
** examples  actually  work.  To compile and run this program you
** should run the following shell commands:
**
**		equel equeltut.q
**		cc equeltut.c -lq
**		a.out
**
** The first command invokes the Equel	pre-processor  which  in-
** serts  code	to send	queries	to INGRES.  The	output is left in
** the file "equeltut.c" in  this  case.   In  general,	the  pre-
** processor is	invoked	as:
**
**		equel [-d] [-f]	[-r] file1.q [file2.q ...]
**
** The output is left in "file1.c", etc.  The -d flag tells Equel
** to  leave line number information in	the file so that run time
** errors can be associated with the proper query.
**
** It is possible to use the C-preprocessor to include files with
** Equel statements and/or declarations	in it if these files have
** names ending	in ".q.h". Such	files will be processed	by equel,
** and	a C version left in the	file ending in ".c.h", which will
** be #included	by the C pre-processor.	Files which are	#included
** but whose names do not end in ".q.h"	will be	ignored	by Equel.
*/


/*
** Equel uses the same syntax as Quel in almost	all cases.  There
** are	a  few	differences  between Equel and Quel and	also some
** subtleties in interfaceing Quel and C constructs.  Some impor-
** tant	points are:
**
**	C-variables declared  to  Equel	 are  used  as	variables
** throughout Equel statements,	except inside  strings or when preceded
** by the non-referencing operator '#'.	 In particular,	be  care-
** ful with variable names which are the same as domain	names.
**
**	All strings passed to C-variables  from	 INGRES	 will  be
** null	 terminated.   This  will  make	them one byte longer than
** they	were in	the relation, so you must declare character arrays
** to be one byte longer than the domain from which the data will
** come.
**
**	Retrieve statements with no result relation have  a  dif-
** ferent  interpretation  in  Equel than in Quel.  There will be
** many	examples of this.
*/





/*
** First some of the queries found in "A Tutorial on INGRES"  are
** mapped  into	 Equel	so  that the similarities and differences
** between the two modes of accessing INGRES can be seen.
*/




/*
** We start by declaring some variables	that will be  needed  for
** the interaction.  Note that the variables are global	to Equel,
** that	is the declarations are	in effect for the entire file.
*/

## char	pname[21];	/*
			** Pname is dimensioned	to hold	one  more
			** character  than the pname field of the
			** parts relation.  We will need the  ex-
			** tra	character so that Equel	will have
			** enough space	 to  null  terminate  the
			** string.   More on this later.  
			**	We must use the non-referencing 
			** operator  when using  "pname" as a 
			** field name, as the field has the same 
			** name as  tha  variable and equel will 
			** assume we mean the variable if we just 
			** write "pname."
			*/

## char	col[9];		/*
			** This	will be	used to	hold color attri-
			** butes  from the parts relation.  It is
			** named "col" insted of "color" so  that
			** the	term "p.color" does not	contain	a
			** variable reference, and  may	 be  used
			** without the non-referencing operator.
			*/

main(argc, argv)
int	argc;
char	*argv[];

{

	/*
	** We start the	interaction with INGRES	using  data  base
	** demo.
	*/


##	ingres "-i210" demo

	/*
	** Up to 9 arguments may be specified to the INGRES call.
	** Here	 we  have  modified  the  integer  output format.
	** Flags must be in quotes so that the plus or minus  are
	** not parsed incorrectly.
	*/



	/*
	** As in the INGRES tutorial, we may print the parts rela-
	** tion:
	*/

##	print parts

	/*
	** Note	that this identical to the Quel	statement  except
	** that	the line is tagged with	the "##" telling the Equ-
	** el pre-processor to translate this line into	 standard
	** C
	*/

	/*
	** The next section of code is intended	to  parallel  the
	** third query	in the Tutorial	[page 4]
	*/

##	range of p is parts	/*
				** This	is identical to	the  Quel
				** syntax.   Note also the use of
				** a comment in	an  Equel  state-
				** ment
				*/


	/*
	 ** Note that the first	pname is assumed to refer to the  vari-
	 ** able  "pname", while the second pname is assumed to	be a
	 ** constant name (as opposed to the value of  the  vari-
	 ** able  "pname")  because of the non-referencing opera-
	 ** tor.
	*/
##	retrieve (pname	= p.#pname)
##	{
		/*
		** Everything inside the braces	is  repeated  for
		** each	tuple that is retrieved.
		*/

		printf("%s\n", pname);

		/*
		** pname is a properly terminated C string.  Equ-
		** el  null  terminates	 ALL  strings  which  are
		** passed from INGRES.	Strings	will be	of length
		** one	more than the width of the attribute.  It
		** is assumed that the user has	 provided  enough
		** room!!
		*/
##	}



	/*
	** Now we will retrieve	the colors and names of	the parts
	** We will skip	the error in the Tutorial and simply note
	** that	the  Equel  interpreter	 would	catch  the  error
	** presented on	page 4:
	**	##	retrieve pname = p.#pname, col =  p.color
	** with	the message:
	**	IS = '=' : line	7, syntax error	
	** which  is  almost as helpful as the Quel message.
	*/




##	retrieve (pname	= p.#pname, col	= p.color)
##		/*
		** The name "col" was used for the variable  name
		** insted of "color".  The latter would	be treat-
		** ed as a variable in the phrase  "p.color"  and
		** INGRES  would  see  "p." followed by	the value
		** color had at	runtime.
		**
		** The comment in this situation must start on	a
		** line	with a "##" since Equel	will look for the
		** "## {" to be	 contiguous  with  the	retrieve.
		** The same holds for blank lines, they must begin
		** with	a "##" if they come  before the	 "##  {".
		*/
##	{
		printf("The color of the %s is %s\n", pname,col);
##	}

	/*
	** The ##{ and ##} are needed, even if you wish to repeat
	** only one line of C-code inside the retrieve.
	*/

	/*
	** To retrieve and print the parts which are gray we  may
	** write:
	*/

	printf("The following parts are	gray:\n");
##	retrieve (pname	= p.#pname)
##	where	p.color	= "gray"
##	{
		printf("\t%s\n", pname);
##	}


	/*
	** The above query is similar to the query on page  5  of
	** the Tutorial.
	*/


	/*
	** In Equel there is no	notion of a "query buffer" as  in
	** the	INGRES	Terminal  Monitor.   If	we want	to do the
	** query on page 6 of the  Tutorial  we	 must  completely
	** specify the query (except for the range statements):
	*/

##	retrieve (pname	= p.#pname, col	= p.color)
##	where	p.color	= "gray"
##	or	p.color	= "pink"
##	{
		printf("The color of the %s is %s\n", pname, col);
##	}



	/*
	** We will now leave the Tutorial behind and use some  of
	** features particular to Equel.
	*/


	example1();

	/*
	** Next	we have	an interactive example...
	*/

	raise();

	/*
	** Next an example of "parametrized" Equel statements
	*/

	param_ex();
}


/*
** Suppose we want to bring parts of a	relation  into	core  for
** some	number crunching which would be	difficult in INGRES.
**
** This	example	brings elements	of the supply  relation	 into  an
** array of structures.
*/
# define	MAXDATA		20




/*
** This defines the fields "pnum", "snum", and	"quan" to Equel.
*/
## struct supply
## {
##	int pnum, snum;
##	int quan;
## };

/*
** The	##{ and	##} at the start and end of the	example1()  func-
** tion  indicate  the	 scope of variables declared within them.
** Therefore data is	considered by Equel to be local	to  exam-
** ple1.  Any	free  block (a ##{...##} not immeadiately after	a
** ##retrieve without a result	relation [an  into])  makes  vari-
** ables declared within it 	be local (there	is, however, only
** one	level of locality; i.e.	either a variable  is  global  to
** the	 file,	or  it	is  local to the outermost enclosing free
** block.
*/

example1()
## {

##	struct	supply	data [MAXDATA +	1];
	register int	i;

	i = 0;

##	range of s is supply

	/*
	** The structure field names are known	to  be	structure
	** fields  beacuse they	were declared as such, and follow
	** the structure variable "data".  On	the right side of
	** the	equals	sign  (=) they are not in the position of
	** structure fields so are assumed to  be  domain  names,
	** although  the  non-referencing  operator could be used
	** here	any way	for clarity.
	*/

##	retrieve (data [i].pnum = s.pnum,
##		data [i].snum	= s.snum,
##		data [i].quan	= s.quan)
##	where s.shipdate <= "76-12-10"
##	{
		printf("supplier #%d, supplies %d of part %d.\n",
		data [i].snum, data [i].quan, data [i].pnum);
		if (i++	>= MAXDATA - 1)
		{
			printf("Too much data!\n");
			break;
			/*
			** The break is	 legal	because	 the  re-
			** trieve  is  converted  into	a "while"
			** statement.  Break is	the only  accept-
			** able	 way to	get out	of a retrieve due
			** to an user detected error.	There  is
			** code	 after	the  "while" to	flush out
			** the data sent by INGRES which was  not
			** used	by the Equel process.
			*/
		}

##	}
## }







/*
** The routine provides	an  interactive	 secession  for	 updating
** salaries.  There are	other ways of accomplishing this interac-
** tion	but this mode brings out some of the possible pitfalls.
*/

raise()
## {
	int		flag;
	int		per;
##	char		percent[10];
##	char		rname[21];
##	char		ename[21];
##	int		sal;
##	char		domain[20];
##	char		info[255];
	extern		*IIinterrupt, reset();

##	range of e is employee

	/*
	** Since the range statement will be in	effect as long as
	** INGRES  is  running	we   declare it	at the top of the
	** loop	rather than each time through the loop.
	*/



	/*
	** Before entering the loop we arrange to  continue  pro-
	** cessing  after  an interrupt	from the user.	It is im-
	** perative that we do not catch the signal at this point
	** since  INGRES  will	catch  the signal and try to syn-
	** chronize with the Equel process.  When the Equel  pro-
	** cess	 has  been  synchronized  it will call (*IIinter-
	** rupt)().
	*/

	IIinterrupt = reset;
	setexit();
loop:
	printf("Please enter employee's	name\n");

	if (eread(ename))
		return (0);

	if (ename[0] ==	'?' && ename[1]	== '\0')
##		print employee
	else
	{
		flag = 0;

		/*
		** In this interaction we do  three  queries  and
		** let INGRES do the arithmetic.  The name is re-
		** trieved into	rname  since  ename  may  contain
		** pattern  matching characters	and more than one
		** name	may be retrieved.   For	 example  "Ross*"
		** may	be  entered  and  both Stanley and Stuart
		** will	get raises.
		*/

##		retrieve (rname	= e.name, sal =	e.salary)
##			where e.name = ename
##		{
			printf("The current salary of %s is %d\n",
				rname, sal);
			flag = 1;
##		}

		if (!flag)
		{
			printf("No such	employee\n");
			goto loop;
		}
		printf("Enter percent increase=");
		if (eread(percent))
			goto loop;


		/*
		** There is no	facility  in  Equel  to	 examine,
		** modify and then put back a tuple.  The replace
		** must	contain	the qualification since	there  is
		** no  connection  between  the	previous retrieve
		** and the replace.
		*/
##		replace	e (salary = e.salary + float8(percent)/100.	* e.salary)
##			where e.name = ename


		per = atoi(percent);

##		retrieve (rname	= e.name, sal =	e.salary)
##			where e.name = ename
##		{
			printf("With that ");
			if (per	< 5)
				printf("piddly");
			else if	(per < 10)
				printf("modest");
			else if	(per < 30)
				printf("inflation fighting");
			else
				printf("tremendous");
			printf(" raise,	%s now makes $%d\n",rname,sal);
##		}


		printf("Do you want any	other information about	%s?\n"
			, ename);

		if (eread(domain) || domain[0] == 'n' )
			goto loop;

		printf("Enter domain:  ");

		if (eread(domain))
			goto loop;

		/*
		** If the user responds	with a '?' then	show  him
		** all	possible  domains by printing out the at-
		** tributes of that relation from  the	tuple  in
		** the "attribute" relation.
		*/

		if (domain[0] == '?' &&	domain[1] == '\0')
		{

##			range of a is attribute

##			retrieve(domain	= a.attname)
##				where a.attrelid = "employee"
##			{
				printf("\t%s\n", domain);
##			}
			printf("Enter domain:  ");

			if (eread(domain))
				goto loop;
		}


		/*
		** Here	we use a C-variable  as	 a  domain  name.
		** The	value of the variable is passed	to INGRES
		** and interpreted as part of the query.
		*/



		/*
		** The ascii funciton is used because the type of
		** the	domain	is not known.  Ascii applied to	a
		** character domain does nothing.
		*/
##		retrieve (rname	= e.name, info = ascii(e.domain))
##			where e.name = ename
##		{

			printf("%s\t%s = %s\n",	rname, domain, info);
##		}

	}
	goto loop;
}

/*
** This	routine	shows the use of parametrized  equel  statements.
** These  are  equel statements	where the target list is undeter-
** mined until	run-time.  In  this  way  a  variable  number  of
** domains,  or	 variable  types  may  be  used	in the same Equel
** statements.
*/

param_ex()
{
	char		name [25];	/*
					 ** Variables used in the
					 ** target   list   of	a
					 ** parametrized   state-
					 ** ment  need not be de-
					 ** clared to equel.
					*/
	register char	*string;
	int		empno;
	char		*tl_vector [100];

	/*
	** Another way to do
	**  ##	retrieve (name = e.#name, empno	=e.number) 
	**  ##	{
	**		printf("employee #%d  is  called  %s.\n",
	** 		empno, name);
	**  ##	}
	*/

	/*
	** This	statement initializes the target  list	variable.
	** The '%' sequences indicate the type of the correspond-
	** ing argument	following.  Valid types	are :
	**	%c -- string of	any length
	**	%i2, %i4 -- integer or long
	**	%f4, %f8 -- float or double
	*/

	string = "%c is	e.name,	%i2 = e.number";
	tl_vector [0] = name;
	tl_vector [1] = &empno;

##	param retrieve (string, tl_vector)
##	/*
	** This	statement could	also be	written	
	** ## param retrieve ("%c is e.name, %i2 = e.number", 
	** ##    tl_vector)
	*/
##	{
		printf("employee #%d is	called %s.\n", empno, name);
##	}

	/*
	** Parametrized  append,  copy,  create, define view,
	** retrieve with a result relation, and replace, may 
	** also be used.
	**
	** One could  say  :  
	** ##  param append to employee ("name is %c, number is %i2",
	** ##	  tl_vector)
	*/
##}



/*
** This	routine	reads a	string from the	terminal  and  null  ter-
** minates it.	It returns 1 when an eof is read.
*/

eread(p)
char	*p;
{
	char	c;
	while(c	= getchar())
	{
		if(c ==	'\n')
		{
			*p = 0;
			return(0);
		}
		*p++ = c;
	}
	return(1);
}

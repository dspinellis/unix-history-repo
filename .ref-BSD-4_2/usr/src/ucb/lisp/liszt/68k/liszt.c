#ifndef lint
static char *rcsid = "$Header: liszt.c 1.6 83/07/25 12:07:31 layer Exp $";
#endif

/*
**			-[Wed May  4 18:18:10 1983 by layer]-
**
**
** liszt :: interface to the lisp compiler
**
** There are two reasons for having a C interface to the compiler:
**	1) Fseek doesn't work properly from lisp, and there is no
**	   lseek.
**	2) To start up a process from a 1 Mb process, like when liszt forks
**	   a /usr/lib/lisp/as to assemble the output of the compiler, takes
**	   too long. (The compiler is 100 times larger than this program.)
**
**/

#include	<stdio.h>
#include	"../../franz/h/aout.h"
#include	"../../franz/h/config.h"

#ifndef LISZT
#define LISZT	"/usr/ucb/xliszt"
#endif

#ifndef AS
#define AS	"/usr/lib/lisp/as"
#endif

#ifndef OFFSET
#define OFFSET		0x0
#endif

main(argc,argv,envp)
	register char *argv[], **envp;
{
	struct exec header;
	register int autorun = 0;
	register int no_assem = 0;
	register char *p;
	register int oflag = 0, cur;
	register int objp;
	register int slen;
	char temp[20], tempfile[20], srcfile[20], outfile[20];
	char *args_to_string();
	char *flags_to_string();
	char command[1024];
    
	/*
	**  process arguments :: check for -r and -S switches
	**
	**  -F is a special flag that means just to fix the offset in the
	** object file, and then quit.
	**
	**/
	/* If no args, then give the user an interactive liszt */
        if (argc == 1)
	{
		execle(LISZT, "liszt", 0, envp);
		exit(100);
	} else
	{
		for (cur = 1; cur < argc; cur++) {
			if (*argv[cur] == '-')
			{
				p = argv[cur];
				p++;
				if (*p == 'o')
				{
		    			strcpy(outfile,argv[++cur]);
		    			oflag++;
				}
				else if (*p == 'F')
				{
					strcpy(outfile,argv[++cur]);
					autorun = 1;
					goto fixit;
				}
				else
				{
		    			for (; *p; p++) {
						switch (*p)
						{
			    			case 'S':
			        			no_assem++;
			        			break;
			    			case 'r':
			        			autorun++;
			        			break;
			    			default:
			        			break;
						}
		    			}
				}
	    		}
			else
			{	/* must be filename */
	        		strcpy(srcfile, argv[cur]);
	    		}
        	}
    	}

	if (no_assem)
	{
    		sprintf(command, "%s %s", LISZT, args_to_string(argv));
#ifdef debug
    		printf("%s\n", command);
#else
		exit(system(command));
#endif
	}

	sprintf(tempfile, "/tmp/Lzt%d.s", getpid());

	/* If output file not given, then we deduce it... */
	if (oflag == 0)
	{
		strcpy(outfile,srcfile);
		slen = strlen(outfile);
		if (outfile[slen - 2] == '.')
		{
			outfile[slen - 1] = 'o';
		}
		else
		{
			strcpy(temp,outfile);
			sprintf(outfile, "%s.o", temp);
		}
	}

	sprintf(command, "%s -S%s -o %s %s",
			 LISZT, flags_to_string(argc, argv), tempfile, srcfile);
#ifdef debug
	printf("%s\n", command);
#else
    	if (system(command) != 0)
	{
		exit(101);
    	}
#endif

	sprintf(command, "%s -o %s %s", AS, outfile, tempfile);
#ifdef debug
	printf("%s\n", command);
#else
	if (system(command) != 0)
	{
		exit(102);
	}
	unlink(tempfile);

fixit:
    	if (autorun)
	{
		if ((objp = open(outfile,2)) == -1)
		{
	    		perror(outfile);
	    		exit(103);
		}
		if (read(objp,&header,sizeof header) != sizeof (struct exec))
		{
			perror("read failed");
			exit(1);
		}

		/* Change the offset to the correct value */
		header.a_entry = OFFSET;

		/* seek back to beginning */
		if (lseek(objp,0,0) != 0)
		{
	    		perror("seek failed");
	    		exit(104);
		}

		/* write the new a.out header... */
		if (write(objp,&header,sizeof header) != sizeof(struct exec))
		{
	    		perror("write failed");
	    		exit(105);
		}

		/* make it executable */
		chmod(outfile, 0755);
    	} else
		exit(0);
#endif
}

char *
args_to_string(pp)
	char *pp[];
{
    	char result[1024];
    	register int ii = 0,
		     jj = 1;

    	for (; pp[jj]; ii++, jj++)
	{
		xstrcpy(&result[ii],pp[jj]);
		ii = ii + strlen(pp[jj]);
    	}
    	result[ii++] = '\0';
    	return(result);
}

char *
flags_to_string(argc, argv)
	int argc;
	char *argv[];
{
    	char result[128];
    	register int chari, word, skipnext = 0, out = 0;

	for (word = 1; word < argc; word++)
	{
		if (skipnext)
		{
			skipnext = 0;
			word++;
			continue;
		}
		if (*argv[word] == '-')
			for (chari = 1; argv[word][chari]; chari++)
				if (argv[word][chari] == 'o')
					skipnext = 1;
				else
					result[out++] = argv[word][chari];
	}
    	result[out] = '\0';
    	return(result);
}

xstrcpy(s, t)
	char *s, *t;
{
    	while (*t != '\0')
	{
        	*s++ = *t++;
    	}
    	*s = ' ';
}

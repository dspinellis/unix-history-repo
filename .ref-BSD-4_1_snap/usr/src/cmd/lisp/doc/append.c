/*
 * append:  append a tail to a list of roots or prepend a head to a list
 *	    of tails. 
 * use:
 *	append tail root1 root2 ... rootn
 * result:
 *	root1tail root2tail ... rootntail
 * or
 * 	append -p root tail1 tail2 ... tailn
 *  result:
 *    	roottail1 roottail2 ... roottailn
 *
 * or
 *	append -s xtail root1xoldt root2xoldt ...
 *  result:
 *	root1xtail  root2xtail ...
 *   that is, each root is tested for the presence of 'x', the first character
 *   in the tail.  If it is present, then all characters beyond it are thrown
 *   away before merging.  This is useful for things like
 *	append -s .c foo.o bar.o baz.o =>> foo.c bar.c baz.c
 *
 * Useful in Makefiles due to the lack of such facilities in make.
 * 
*/
#include <stdio.h>

char buffer[2000];	/* nice and big */
char *rindex();

 main(argc,argv)
 char **argv;
 {
     int i, base;
     int prepend = 0,
         append = 0,
	 strip = 0;
     char stripchar;
     char *chp;
     
     if(argc <= 2)
     {
	 fprintf(stderr,"use: append tail root1 root2 ... rootn\n");
	 exit(1);
     }
     if(argv[1][0] == '-')
     {
	 switch(argv[1][1])
	 {
	     case 'p' : prepend = 1;
	     		break;
	     case 's' : strip = 1;
	     		append = 1;
			stripchar = argv[2][0];	/* first char of tail */
			break;
	     default:  fprintf(stderr,"append: illegal switch %s\n",argv[1]);
	     		exit(1);
	 }
	 base = 2;
     }
     else {
	 append = 1;
	 base = 1;
     }
     
     for(i = base +1; i < argc ; i++)
     {
	 if(append)
	 {
	    strcpy(buffer,argv[i]);
	    if(strip && (chp = rindex(buffer,stripchar)))
	    {
		*chp = '\0';
	    }
	    strcat(buffer,argv[base]);
	 }
	 else {
	     strcpy(buffer,argv[base]);
	     strcat(buffer,argv[i]);
	 }
	 printf("%s ",buffer);
     }
     printf("\n");
     exit(0);
 }
 

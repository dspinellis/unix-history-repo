#include "uucp.h"
#include <stdio.h>
char Sysfile[1]; /* for compiler */
 
 
/*******
 *      uuname  -  return list of all remote systems 
 *		   recognized by uucp, or  (with -l) the local
 *		   uucp name.
 *
 *      return codes: 0 | 1  (can't read)
 */
 
main(argc,argv)
	char *argv[];
	int argc;
{
	int i;
	FILE *np;
	char s[128];
	if(argv[1][0] == '-' && argv[1][1] == 'l') {
		uucpname(s);
		printf("%s\n",s);
		exit(0);
	}
        if(argc != 1) {printf("Usage: uunames [-l]\n"); exit(1);}
	if((np = fopen(SYSFILE,"r")) == NULL) {
		printf("%s (name file) protected\n",SYSFILE);
		exit(1);
	}
	while ( fgets(s,128,np) != NULL ) {
		for(i=0; s[i]!=' '; i++);
		s[i]='\0';
		if(s[0]=='x' && s[1]=='x' && s[2]=='x') continue ;
		printf("%s\n",s);
	}
 
	exit(0);
}

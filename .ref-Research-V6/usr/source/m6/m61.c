#

#include "m6.h"

main(argc,argv)
char **argv; 
{
	extern fin;
	int t;
	gf = ge = g0;
	pf = pe = p0;
	t = g0; 
	gmax =+ t-10;
	t = d0; 
	dmax =+ t-10;
	t = p0; 
	pmax =+ t-10;
	revptr(d0,d0+2,&df,&de);
	if(argc>1) {
		fin = open(argv[1],0);
		control();
		close(fin); 
	}
	fin = dup(0);
	control();  
}


diag(m) {
	printf("%s\n",m);
	iot(); 
}	/* abort */

int iot 4;


#ifndef lint
static char sccsid[] = "@(#)mkfont.c	4.3 %G%";
#endif lint

#include "mkfont1.c"

/*
  this program takes 102 width values
  (one per line) in the order provided bu Graphic
  Systems and prepares a C-compileable width table.
*/
char ibuf[512];
int id;
int width[102];
int ascii[102];
int zero;
int emw, hyw;
int xxx;

main(argc,argv)
int argc;
char **argv;
{
	register i, j;
	register char *p;

	while((--argc > 0) && ((++argv)[0][0]=='-')){
		switch(argv[0][1]){
			default:
				continue;
		}
	}
	if(argc){
		if((id=open(argv[0],0)) < 0){
			printf("Cannot open: %s.\n",argv[0]);
			exit(1);
		}
	}
	j = read(id,ibuf,512);
	p = ibuf;
	for(i=0; i<102; i++){
		width[i] = atoi(p);
		while(*p++ != '\n');
	}
	for(i=0; i<102; i++){
		if(font[i].name < 0177){
			ascii[i] = font[i].name;
		}else{
			for(j=0; chtab[j] != 0; j += 2){
				if(font[i].name == chtab[j])break;
			}
			ascii[i] = chtab[j+1] & 0377;
			if(chtab[j] == PAIR('h','y')) hyw = width[i];
			if(chtab[j] == PAIR('e','m')) emw = width[i];
		}
	}
	printf("char XXw[256-32] {\t/*XX*/\n");
	for(i=040; i<256; i++){
		if(i == 0377){
			printf("0};\n");
			break;
		}
		if(i == 0177){
			printf("6,\t %s\n",nametab[i-040]);
			continue;
		}
		if(i == 0226){
			printf("3,\t %s\n",nametab[i-040]);
			continue;
		}
		if(i == ' '){
			printf("12,\t %s\n",nametab[i-040]);
			continue;
		}
		if(i == '-'){
			printf("%d,\t %s\n",hyw,nametab[i-040]);
			continue;
		}
		for(j=0; j<102; j++){
			if(ascii[j] == i)break;
		}
		if(j == 102){
			printf("0,");
			zero++;
			if(nametab[i-040]){
				printf("\t %s\n",nametab[i-040]);
				zero = 0;
			}else if(i < 0177){
				printf("\t /*%c*/\n",i);
				zero = 0;
			}
			if(zero && !((i+1)%8)){
				printf("\n");
				zero = 0;
			}
		}else{
			if(zero){
				zero = 0;
				printf("\n");
			}
			printf("%d",width[j]);
			if(font[j].ctval)printf("+0%d00, ",font[j].ctval);
			else printf(",\t ");
			printf("%s\n",nametab[i-040]);
		}
	}
}

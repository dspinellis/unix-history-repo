main(argc,args)
char **args ;
{
register char *tape ;
int i , j ;
 
if (argc > 2) {
	usage :
		printf("usage : rew [[m]digit]\n") ;
		exit(1) ;
	}
tape = "/dev/tap0" ;
if (argc > 1) {
	j = 0 ;
	if (args[1][j] == 'm') {
		tape = "/dev/mt0" ;
		i = 7 ;
		j++ ;
		}
	else i = 8 ;
	if (args[1][j] != '\0') tape[i] = args[1][j] ;
	}
i = open(tape,0) ;
read(i,&j,sizeof(j)) ;
close(i) ;
}

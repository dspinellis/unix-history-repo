extern	fout;
int arg[3];

main() {
	int i;
	int die();
	fout = dup(1);

	gtty(2, arg);
	arg[2] =^ 020;
	stty(2, arg);

	signal(2, die);

	for(;;) {

		for(i=0; i<78; i++) {printf("\\\n");}
		for(i=0; i<78; i++) {printf("/\b\b\n");}
		}
	}

die() {
	arg[2] =^ 020;
	stty(2, arg);
	exit();
	}

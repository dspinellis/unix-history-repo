#include <sys/param.h>
#include <sys/ino.h>
#include <dumprestor.h>

#include <stdio.h>

main()
{
	struct idates idt;
	char response[100];
	FILE *tty;

	if (freopen("/etc/ddate", "r", stdin) < 0)
		perror("/etc/ddate"), exit(1);
	tty = fopen("/dev/tty", "r");
	while (fread(&idt, sizeof idt, 1, stdin) == 1) {
		fprintf(stderr,"%.8s %c %25.25s ? ", idt.id_name, idt.id_incno,
		    ctime(&idt.id_ddate));
		fgets(response,100,tty);
/*
		if (!strcmp(idt.id_name,"rrp0g"))
			strcpy(idt.id_name,"rrp2h");
*/
		if (response[0] == 'y')
		fwrite(&idt, sizeof idt, 1, stdout);
	}
}

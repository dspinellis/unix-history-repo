#include <sys/param.h>
#include <sys/ino.h>
#include <dumprestor.h>

#include <stdio.h>

main()
{
	struct idates idt;

	if (freopen("/etc/ddate", "r", stdin) < 0)
		perror("/etc/ddate"), exit(1);
	while (fread(&idt, sizeof idt, 1, stdin) == 1)
		printf("%16.16s %c %s", idt.id_name, idt.id_incno,
		    ctime(&idt.id_ddate));
}

#include "../h/param.h"
#include "../h/ino.h"
#include "../h/inode.h"
#include "../h/filsys.h"
#include "../h/dir.h"
#include "../h/vm.h"
#include <a.out.h>
#include "saio.h"

char line[100];

main()
{
	int i;

	printf("\nBoot\n");
	do {
		printf(": "); gets(line);
		i = open(line,0);
	} while (i < 0);

	copyunix(i);
}

copyunix(io)
register io;
{
	struct exec x;
	register int i;
	char *addr;

	i = read(io, (char *)&x, sizeof x);
	if (i != sizeof x || x.a_magic != 0410)
		_stop("Bad format\n");
	printf("%d", x.a_text);
	if (read(io, (char *)0, x.a_text) != x.a_text)
		goto shread;
	addr = (char *)x.a_text;
	while ((int)addr & CLOFSET)
		*addr++ = 0;
	printf("+%d", x.a_data);
	if (read(io, addr, x.a_data) != x.a_data)
		goto shread;
	addr += x.a_data;
	printf("+%d", x.a_bss);
	x.a_bss += 128*512;	/* slop */
	for (i = 0; i < x.a_bss; i++)
		*addr++ = 0;
	x.a_entry &= 0x7fffffff;
	printf(" start 0x%x\n", x.a_entry);
	(*((int (*)()) x.a_entry))();
	_exit();
shread:
	_stop("Short read\n");
}

#ifndef lint
static char sccsid[] = "@(#)hylog.c	4.2 (Berkeley) 7/8/83";
static char origsccsid[] = "@(#)hylog.c	1.2 Hyperchannel Log Printer 82/11/29";
#endif

#include <stdio.h>
#include <nlist.h>
#include <sys/types.h>

#define ok(x) (((int)(x)) & 0x7fffffff)
#include <sys/time.h>
#define HYLOG
#include <vaxif/if_hyreg.h>

struct nlist nl[2] = {
	{ "_hy_log" },
	{ 0 }
};

struct hy_log hy_log;

main()
{
	register unsigned char *p, *ep;
	register unsigned len;
	int mem;

	nlist("/vmunix", nl);
	if (nl[0].n_type == 0)
		done("No namelist\n");
	if ((mem = open("/dev/kmem", 0)) < 0)
		done("Can't oper /dev/kmem\n");
	lseek(mem, (long)nl[0].n_value, 0);
	read(mem, &hy_log, sizeof(hy_log));
	if (ok(hy_log.hyl_self) != ok(nl[0].n_value))
		done("hy_log.hyl_self not self referencing (namelist mismatch?)\n");
	ep = &hy_log.hyl_buf[ok(hy_log.hyl_ptr) -
		ok(& ( (struct hy_log *) (nl[0].n_value) )->hyl_buf[0])];
	p = &hy_log.hyl_buf[0];

	printf("%d bytes in log buffer\n", ep - p);

#define plong(name) \
	printf(" %s %08lx", name, *(unsigned long *)p); \
	p += sizeof(unsigned long);

#define pnlong(name) \
	printf(" %s %02x%02x%02x%02x", name, p[0], p[1], p[2], p[3]); \
	p += sizeof(unsigned long);

#define pnshort(name) \
	printf(" %s %02x%02x", name, p[0], p[1]); \
	p += sizeof(unsigned short);

#define pshort(name) \
	printf(" %s %04x", name, *(unsigned short *)p); \
	p += sizeof(unsigned short);

#define pbyte(name) printf(" %s %02x", name, *p++);

#define phdr() \
	pnshort("crtl"); \
	pnshort("access"); \
	putchar('\n'); \
	putchar('\t'); \
	pnshort("to"); \
	pnshort("from"); \
	pnshort("param"); \
	pbyte("type"); \
	pbyte("off");

	while (p < ep) {
		switch(*p++) {
		case HYL_NOP:
			printf("nop -\n");
			goto out;

		case HYL_UP:			/* no data */
			printf("up -");
			goto printdata;

		case HYL_STATUS:
			printf("status -");
			goto printdata;

		case HYL_STATISTICS:
			printf("statistics -");
			if (*p != sizeof(struct hy_stat))
				goto printdata;
			p++;
			pnlong("msgcnt"); pnlong("dbcnt");
			pnlong("tbusy");  pnlong("hwret");
			putchar('\n');
			putchar('\t');
			pnlong("crcbad"); pnlong("mcret"); pnlong("tdabort");
			pbyte("atype");
			pbyte("");
			pbyte("rev");
			pbyte("address");
			break;

		case HYL_XMIT:
			printf("xmt -");
			if (*p != (sizeof(struct hy_hdr) + sizeof(short)))
				goto printdata;
			p++;
			pshort("mplen");
			phdr();
			break;

		case HYL_RECV:
			printf("rcv -");
			if (*p != (sizeof(struct hy_hdr) + sizeof(short)))
				goto printdata;
			p++;
			pshort("length");
			phdr();
			break;

		case HYL_CMD:
			printf("cmd -");
			if (*p != 4)
				goto printdata;
			p++;
			pbyte("cmd");
			pbyte("state");
			pshort("count");
			break;

		case HYL_INT:
			printf("int -");
			if (*p == 4) {
				p++;
				pshort("csr");
				pshort("wcr");
			} else if (*p == 6) {
				p++;
				pbyte("state");
				pbyte("flags");
				pshort("csr");
				pshort("wcr");
			} else
				goto printdata;
			break;

		default:
			printf("unknown %d -", *p);
		printdata:
			len = *p++;
			while (len > 0) {
				printf(" %02x", *p++);
				len--;
			}
			break;
		}
		putchar('\n');
	}

out:
	printf("end of log\n");
}

done(s, p)
	char *s;
	int p;
{
	fprintf(stderr, s, &p);
	exit(1);
}

/*
  VAX page table entry
 */

struct pt_entry {
	int pg_pfnum:21,
		:2,
		pg_type:2,
		:1,
	    pg_m:1,
	    pg_prot:4,
	    pg_v:1;
};

#define PG_PFNUM 0x1fffff
#define PG_M 0x4000000
#define PG_PROT 0x78000000
#define PG_V 0x80000000

#define PG_NOACC 0
#define PG_KR 0x18000000
#define PG_KW 0x10000000
#define PG_UW 0x20000000
#define PGURKW 0x60000000
#define PG_URKR 0x78000000

#define PG_TXT 0x01800000

/* archive file definition for GNU software */

/* So far this is correct for BSDish archives.  Don't forget that
   files must begin on an even byte boundary. */

#ifndef __GNU_AR_H__
#define __GNU_AR_H__

#define ARMAG  "!<arch>\n"	/* For COFF and a.out archives */
#define ARMAGB "!<bout>\n"	/* For b.out archives */
#define SARMAG 8
#define ARFMAG "`\n"

struct ar_hdr {
  char ar_name[16];		/* name of this member */
  char ar_date[12];		/* file mtime */
  char ar_uid[6];		/* owner uid; printed as decimal */
  char ar_gid[6];		/* owner gid; printed as decimal */
  char ar_mode[8];		/* file mode, printed as octal   */
  char ar_size[10];		/* file size, printed as decimal */
  char ar_fmag[2];		/* should contain ARFMAG */
};

#endif /* __GNU_AR_H__ */

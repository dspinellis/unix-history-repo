/*
 * common encoding macro definitions
 */

#define DO_OFFSET(parm, p)	((parm) + (p)->pe_ucode)
#define NO_OFFSET(parm, p)	(parm)

/* various things From Pointer And Offset- *_FPAO */

#define INT_FPAO(parm, p)	(*(integer *)DO_OFFSET(parm, p))

#define REAL_FPAO(parm, p)	(*(double *)DO_OFFSET(parm, p))

#define CHAR_FPAO(parm, p)	(*(char *)DO_OFFSET(parm, p))

#define OID_FPAO(parm, p)	(*(OID *)DO_OFFSET(parm, p))
#define SOID_FPAO(parm, p)	((OID)NO_OFFSET(parm, p))

#define PTR_FPAO(parm, p)	(*(char **)DO_OFFSET(parm, p))
#define SPTR_FPAO(parm, p)	((char *)NO_OFFSET(parm, p))

#define QB_FPAO(parm, p)	(*(struct qbuf **)DO_OFFSET(parm, p))
#define SQB_FPAO(parm, p)	((struct qbuf *)NO_OFFSET(parm, p))

#define PE_FPAO(parm, p)	(*(PE *)DO_OFFSET(parm, p))
#define SPE_FPAO(parm, p)	((PE)NO_OFFSET(parm, p)

#define TYPE2MOD(mod, p)	((mod)->md_etab[p->pe_tag])

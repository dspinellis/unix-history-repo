
PE	mkpelist(), mkpe();

/* Possible operations we can test */
#define SIMP_SEND	1	/* Simple transfer - send some data */
#define CPLX_SEND	2	/* Complex transfer - send incremental */
#define SIMP_RCV	3	/* Simple transfer - receive some data */
#define CPLX_RCV	4	/* Complex transfer - receive incremental */
#define RCV_PLS		5	/* Receive a Please indication */
#define RCV_GIVE	6	/* Receive a Give indication */
#define SEND_PLS	7	/* Send a Please request */
#define SEND_GIVE	8	/* Send a Give request */
#define SEND_ABRT	9	/* Send an Abort */
#define RCV_ABRT	10	/* Receive an Abort */
#define RCV_CLOSE	11	/* Receive a CLOSE indication and respond */
#define SEND_CLOSE	12	/* Send a Close request */
#define RCV_FINISH	13	/* Receive a X410 Finish */
#define SEND_FINISH	14	/* Send a X410 Finish */

struct scsicmd {
	int lun;
	int readflag; /* 1=read, 0=write */
	int blkno; /* just used in seek sorting */

	unsigned int cdblen;
	caddr_t cdb;

	unsigned int datalen;
	caddr_t data;

	unsigned int ccblen;
	caddr_t ccb;
};
#define SCSICMD _IOWR('a', 150, struct scsicmd)

/* write regs */
#define AS_CONTROL 0
#define AS_DATA_OUT 1

/* read regs */
#define AS_STATUS 0
#define AS_DATA_IN 1
#define AS_INTR 2

/* control port bits */
#define AS_CONTROL_HRST 0x80 /* hard reset */
#define AS_CONTROL_SRST 0x40 /* soft reset */
#define AS_CONTROL_IRST 0x20 /* interrupt reset */
#define AS_CONTROL_SCRST 0x10 /* scsi bus reset */

/* status port bits */
#define AS_STATUS_STST 0x80 /* self test in progress */
#define AS_STATUS_DIAGF 0x40 /* internal diagnostic failure */
#define AS_STATUS_INIT 0x20 /* mailbox initialization required */
#define AS_STATUS_IDLE 0x10 /* scsi host adapter idle */
#define AS_STATUS_CDF 0x08 /* command/data out port full */
#define AS_STATUS_DF 0x04 /* data in port full */
/* 0x02 reserved */
#define AS_STATUS_INVDCMD 0x01 /* invalid host adapter command */

/* interrupt port bits */
#define AS_INTR_ANY 0x80
#define AS_INTR_SCRD 0x08 /* scsi reset detected */
#define AS_INTR_HACC 0x04 /* host adapter command complete */
#define AS_INTR_MBOA 0x02 /* mailbox out available */
#define AS_INTR_MBIF 0x01 /* mailbox in available */

/* command bytes */
#define AS_CMD_NOP 0x00
#define AS_CMD_MAILBOX_INIT 0x01
#define AS_CMD_START_SCSI_COMMAND 0x02
#define AS_CMD_START_BIOS_COMMAND 0x03
#define AS_CMD_ADAPTER_INQUIRY 0x04
#define AS_CMD_ENABLE_MBOA_INTR 0x05
#define AS_CMD_SET_SELECTION_TIMEOUT 0x06
#define AS_CMD_SET_BUS_ON_TIME 0x07
#define AS_CMD_SET_BUS_OFF_TIME 0x08
#define AS_CMD_SET_TRANSFER_SPEED 0x09
#define AS_CMD_RETURN_INSTALLED_DEVICES 0x0a
#define AS_CMD_RETURN_CONFIGURATION_DATA 0x0b
#define AS_CMD_ENABLE_TARGET_MODE 0x0c
#define AS_CMD_RETURN_SETUP_DATA 0x0d
#define AS_CMD_WRITE_ADAPTER_CHANNEL_2_BUFFER 0x1a
#define AS_CMD_READ_ADAPTER_CHANNEL_2_BUFGFER 0x1b
#define AS_CMD_WRITE_ADAPTER_FIFO_BUFFER 0x1c
#define AS_CMD_READ_ADAPTER_FIFO_BUFFER 0x1d
#define AS_CMD_ECHO 0x1f
#define AS_CMD_ADAPTER_DIAG 0x20
#define AS_CMD_SET_ADAPTER_OPTIONS 0x21

struct mailbox_entry {
	unsigned char cmd;
	unsigned char msb;
	unsigned char mid;
	unsigned char lsb;
};

#define MAXCDB 50
#define MAXSENSE 100

struct ccb {
	unsigned char ccb_opcode;
	unsigned char ccb_addr_and_control;
	unsigned char ccb_scsi_command_len;
	unsigned char ccb_requst_sense_allocation_len;
	unsigned char ccb_data_len_msb;
	unsigned char ccb_data_len_mid;
	unsigned char ccb_data_len_lsb;
	unsigned char ccb_data_ptr_msb;
	unsigned char ccb_data_ptr_mid;
	unsigned char ccb_data_ptr_lsb;
	unsigned char ccb_link_msb;
	unsigned char ccb_link_mid;
	unsigned char ccb_link_lsb;
	unsigned char ccb_link_id;
	unsigned char ccb_host_status;
	unsigned char ccb_target_status;
	unsigned char ccb_zero1;
	unsigned char ccb_zero2;
	unsigned char ccb_cdb[MAXCDB + MAXSENSE];
};
#define ccb_sense(ccb) ((ccb)->ccb_cdb + (ccb)->ccb_scsi_command_len)


/* Oasys object format header file for BFD.
   Contributed by Cygnus Support.  */

#define OASYS_MAX_SEC_COUNT 16
/* **** */

typedef struct oasys_archive_header {
  uint32_type  version;
  char create_date[12];
  char revision_date[12];
  uint32_type mod_count;
  uint32_type mod_tbl_offset;
  uint32_type sym_tbl_size;
  uint32_type sym_count;
  uint32_type sym_tbl_offset;
  uint32_type xref_count;
  uint32_type xref_lst_offset;
} oasys_archive_header_type;

typedef struct oasys_extarchive_header {
  char version[4];
  char create_date[12];
  char revision_date[12];
  char mod_count[4];
  char mod_tbl_offset[4];
  char sym_tbl_size[4];
  char sym_count[4];
  char sym_tbl_offset[4];
  char xref_count[4];
  char xref_lst_offset[4];
} oasys_extarchive_header_type;

typedef struct oasys_module_table {
  int32_type mod_number;
  char mod_date[12];
  int32_type mod_size;
  int32_type dep_count;
  int32_type depee_count;
  int32_type file_offset;
  int32_type sect_count;
  char *module_name;
  uint32_type module_name_size;
} oasys_module_table_type;


typedef struct oasys_extmodule_table_a {
  char mod_number[4];
  char mod_date[12];
  char mod_size[4];
  char dep_count[4];
  char depee_count[4];
  char sect_count[4];
  char file_offset[4];
  char mod_name[32];
} oasys_extmodule_table_type_a_type;

typedef struct oasys_extmodule_table_b {
  char mod_number[4];
  char mod_date[12];
  char mod_size[4];
  char dep_count[4];
  char depee_count[4];
  char sect_count[4];
  char file_offset[4];
  char mod_name_length[4];
} oasys_extmodule_table_type_b_type;


typedef enum oasys_record {
  oasys_record_is_end_enum = 0,
  oasys_record_is_data_enum = 1,
  oasys_record_is_symbol_enum = 2,
  oasys_record_is_header_enum = 3,
  oasys_record_is_named_section_enum = 4,
  oasys_record_is_com_enum = 5,
  oasys_record_is_debug_enum = 6,
  oasys_record_is_section_enum = 7,
  oasys_record_is_debug_file_enum = 8,
  oasys_record_is_module_enum = 9,
  oasys_record_is_local_enum = 10
} oasys_record_enum_type;

  

typedef struct oasys_record_header {
  uint8_type length;
  int8_type check_sum;
  int8_type type;
  int8_type fill;
} oasys_record_header_type;

typedef struct oasys_data_record {
  oasys_record_header_type header;
  uint8e_type relb;
  uint8e_type addr[4];
  /* maximum total size of data record is 255 bytes */
  uint8e_type data[246];
} oasys_data_record_type;

typedef struct oasys_header_record {
  oasys_record_header_type header;
  int8_type version_number;
  int8_type rev_number;
  char module_name[26-6];
  char description[64-26];
} oasys_header_record_type;

#define OASYS_VERSION_NUMBER 0
#define OASYS_REV_NUMBER 0
typedef struct oasys_symbol_record {
  oasys_record_header_type header;
  int8e_type relb;
  int8e_type value[4];
  int8e_type refno[2];
  char name[64];
} oasys_symbol_record_type;

typedef int8e_type relocation_byte;

#define RELOCATION_PCREL_BIT 0x80
#define RELOCATION_32BIT_BIT 0x40
#define RELOCATION_TYPE_BITS 0x30
#define RELOCATION_TYPE_ABS 0x00
#define RELOCATION_TYPE_REL 0x10
#define RELOCATION_TYPE_UND 0x20
#define RELOCATION_TYPE_COM 0x30
#define RELOCATION_SECT_BITS 0x0f

typedef struct oasys_section_record {
  oasys_record_header_type header;
  uint8e_type relb;
  int8_type value[4];
  int8_type vma[4];
  int8_type fill[3];
} oasys_section_record_type;

typedef struct oasys_end_record {
  oasys_record_header_type header;
  uint8e_type relb;
  int8e_type entry[4];
  int8e_type fill[2];
  int8e_type zero;
} oasys_end_record_type;

typedef union oasys_record_union {
  oasys_record_header_type header;
  oasys_data_record_type data;
  oasys_section_record_type section;
  oasys_symbol_record_type symbol;
  oasys_header_record_type first;
  oasys_end_record_type end;
  uint8e_type pad[256];
} oasys_record_union_type;

#include <stdio.h>
#define DEFINE_TABLE
#include "z8k-opc.h"

static void fetch_data();
static void fetch_instr();
static unsigned long get_val();
static int is_segmented();
static int lookup_instr();
static void output_instr();
static void unpack_instr();
static void unparse_instr();

typedef struct {
   unsigned char instr_buf[24];
   unsigned long bytes_fetched;
   unsigned long tabl_index;
   char instr_asmsrc[80];
   unsigned long arg_reg[0x0f];
   unsigned long immediate;
   unsigned long displacement;
   unsigned long address;
   unsigned long cond_code;
   unsigned long ctrl_code;
   unsigned long flags;
   unsigned long interrupts;
} instr_data_s;


static char *codes[16] = 
{
  "f",
  "lt",
  "le",
  "ule",
  "ov/pe",
  "mi",
  "eq",
  "c/ult",
  "t",
  "ge",
  "gt",
  "ugt",
  "nov/po",
  "pl",
  "ne",
  "nc/uge"
};


int print_insn_z8k(addr, in_buf, stream)
unsigned long addr;
unsigned char *in_buf;
FILE *stream;
{
  instr_data_s  instr_data;

  fetch_instr( &in_buf, &instr_data );
  if (  lookup_instr( &instr_data )) 
  {
    fetch_data( &in_buf, &instr_data );
    unpack_instr( &instr_data );
    unparse_instr( &instr_data );
    output_instr( &instr_data, addr, stream );
    return instr_data.bytes_fetched;
  }
  else {
    fprintf(stream,".word %02x%02x", in_buf[0], in_buf[1]);
    return 2;
  }

}


static void fetch_data( in_buf, instr_data )
unsigned char **in_buf;
instr_data_s *instr_data;
{
   int bytes_2fetch;

   bytes_2fetch = z8k_table[instr_data->tabl_index].length -
                  instr_data->bytes_fetched;
   while( bytes_2fetch-- )
      instr_data->instr_buf[instr_data->bytes_fetched++] = *(*in_buf)++;
}

static void fetch_instr( in_buf, instr_data )
unsigned char **in_buf;
instr_data_s  *instr_data;
{
   unsigned int loop = 2;

   instr_data->bytes_fetched = 0;
   while( loop-- )
      instr_data->instr_buf[instr_data->bytes_fetched++] = *(*in_buf)++;

}

static unsigned long get_val( instr_buf, start_nibl, nibls_long )
unsigned char (*instr_buf)[];
unsigned int start_nibl, nibls_long;
{
   unsigned long ret_val;
   unsigned char byte_val, nibl_val;
   unsigned int  nibl_index, nibl_lim;
   unsigned int byte_index;
   unsigned int which_nibl;

   ret_val = 0;
   nibl_lim = start_nibl + nibls_long;
   for( nibl_index = start_nibl; nibl_index < nibl_lim; nibl_index++ )
   {
      byte_index = nibl_index / 2;
      which_nibl = nibl_index % 2;
      switch( which_nibl )
      {
         case 0:
            byte_val = (*instr_buf)[byte_index];
            nibl_val = (byte_val >> 4) & 0x0f;
            break;
         case 1:
            nibl_val = byte_val & 0x0f;
            break;
      }
      ret_val = (ret_val << 4) | nibl_val;
   }

   return ret_val;

}

static int is_segmented()
{
   return 1;
}

static 
int lookup_instr( instr_data )
instr_data_s *instr_data;
{

  int            nibl_index, tabl_index;
  int            tablent_found, nibl_matched;
  unsigned short instr_nibl;
  unsigned short tabl_datum, datum_class, datum_value;

  nibl_matched = 0;
  tabl_index = 0;
  while( ! nibl_matched && z8k_table[tabl_index].name)
  {
    nibl_matched = 1;
    for( nibl_index = 0; nibl_index < 4 && nibl_matched; nibl_index++ )
    {
      instr_nibl =  get_val( instr_data->instr_buf, nibl_index, 1 );

      tabl_datum = z8k_table[tabl_index].byte_info[nibl_index];
      datum_class = tabl_datum & CLASS_MASK;
      datum_value = ~CLASS_MASK & tabl_datum;

      switch( datum_class )
      {
       case CLASS_BIT:
	if( datum_value != instr_nibl ) nibl_matched = 0;
	break;
       case CLASS_00II:
	if( ! ((~instr_nibl) & 0x4) ) nibl_matched = 0;
	break;
       case CLASS_01II:
	if( ! (instr_nibl & 0x4) ) nibl_matched = 0;
	break;
       case CLASS_0CCC:
	if( ! ((~instr_nibl) & 0x8) ) nibl_matched = 0;
	break;
       case CLASS_1CCC:
	if( ! (instr_nibl & 0x8) ) nibl_matched = 0;
	break;
       case CLASS_0DISP7:
	if( ! ((~instr_nibl) & 0x8) ) nibl_matched = 0;
	nibl_index += 1;
	break;
       case CLASS_1DISP7:
	if( ! (instr_nibl & 0x8) ) nibl_matched = 0;
	nibl_index += 1;
	break;
       case CLASS_REGN0:
	if( instr_nibl == 0 ) nibl_matched = 0;
	break;
       default:
	break;
      }
    }
    if (nibl_matched) 
    {
      instr_data->tabl_index = tabl_index;
      return 1;
    }

    tabl_index++;
  }
  return 0;


}

static void output_instr( instr_data, addr, stream )
instr_data_s  *instr_data;
unsigned long addr;
FILE *stream;
{
   int            loop, loop_limit;
   unsigned long  word_val;
   char           tmp_str[20];
   char           out_str[100];

   strcpy( out_str, "" );

   loop_limit = z8k_table[instr_data->tabl_index].length / 2;
   for( loop = 0; loop < loop_limit; loop++ )
   {
      word_val = get_val( instr_data->instr_buf, loop * 4, 4 );
      sprintf( tmp_str,  "%04x ", word_val );
      strcat( out_str, tmp_str );
   }

   while( loop++ < 5 )
   {
      strcat( out_str, "     " );
   }

   strcat( out_str, instr_data->instr_asmsrc );

   fprintf( stream, "%s", out_str );
}

static void unpack_instr( instr_data )
instr_data_s  *instr_data;
{
   int            nibl_index, word_index;
   int            nibl_count, loop;
   unsigned short instr_nibl, instr_byte, instr_word, instr_long;
   unsigned short tabl_datum, datum_class, datum_value;

   nibl_count = 0;
   loop = 0;
   while( z8k_table[instr_data->tabl_index].byte_info[loop] != 0 )
   {
      word_index = (int) nibl_count / 4;
      nibl_index = (int) nibl_count % 4;

      switch( nibl_index )
      {
         case 0:
            instr_nibl = get_val( instr_data->instr_buf, nibl_count, 1 );
            instr_byte = get_val( instr_data->instr_buf, nibl_count, 2 );
            instr_word = get_val( instr_data->instr_buf, nibl_count, 4 );
            instr_long = get_val( instr_data->instr_buf, nibl_count, 8 );
            break;
         case 1:
            instr_nibl = get_val( instr_data->instr_buf, nibl_count, 1 );
            break;
         case 2:
            instr_nibl = get_val( instr_data->instr_buf, nibl_count, 1 );
            instr_byte = get_val( instr_data->instr_buf, nibl_count, 2 );
            break;
         case 3:
            instr_nibl = get_val( instr_data->instr_buf, nibl_count, 1 );
            break;
         default:
            break;
      }

      tabl_datum = z8k_table[instr_data->tabl_index].byte_info[loop];
      datum_class = tabl_datum & CLASS_MASK;
      datum_value = tabl_datum & ~CLASS_MASK;

      switch( datum_class )
      {
         case CLASS_X:
            instr_data->address = instr_nibl;
            break;
         case CLASS_BA:
            instr_data->displacement = instr_nibl;
            break;
         case CLASS_BX:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_DISP:
            switch( datum_value )
            {
               case ARG_DISP16:
                  instr_data->displacement = instr_word;
                  nibl_count += 3;
                  break;
               case ARG_DISP12:
                  instr_data->displacement = instr_word & 0x0fff;
                  nibl_count += 2;
                  break;
               default:
                  break;
            }
            break;
         case CLASS_IMM:
            switch( datum_value )
            {
               case ARG_IMM4:
                  instr_data->immediate = instr_nibl;
                  break;
               case ARG_IMM8:
                  instr_data->immediate = instr_byte;
                  nibl_count += 1;
                  break;
               case ARG_IMM16:
                  instr_data->immediate = instr_word;
                  nibl_count += 3;
                  break;
               case ARG_IMM32:
                  instr_data->immediate = instr_long;
                  nibl_count += 7;
                  break;
               case ARG_IMMN:
                  instr_data->immediate = instr_nibl -1;
                  break;
               /* ????? */
               /* missing ARG_IMMNMINUS1 */
               case ARG_IMM_1:
                  instr_data->immediate = 1;
                  break;
               case ARG_IMM_2:
                  instr_data->immediate = 2;
                  break;
               case ARG_NIM16:
                  instr_data->immediate = (- instr_word);
                  nibl_count += 3;
                  break;
               case ARG_IMM2:
                  instr_data->immediate = instr_nibl & 0x3;
                  break;
               default:
                  break;
            }
            break;
         case CLASS_CC:
            instr_data->cond_code = instr_nibl;
            break;
         case CLASS_CTRL:
            instr_data->ctrl_code = instr_nibl;
            break;
         case CLASS_DA:
         case CLASS_ADDRESS:
            if( is_segmented() )
            {
               if( instr_nibl & 0x8 )
               {
                  instr_data->address = ((instr_word & 0x7f00) << 8) +
                                        (instr_long & 0xffff);
                  nibl_count += 7;
               }
               else
               {
                  instr_data->address = ((instr_word & 0x7f00) << 8) +
                                        (instr_word  & 0x00ff);
                  nibl_count += 3;
               }
            }
            else
            {
               instr_data->address = instr_word;
               nibl_count += 3;
            }
            break;
         case CLASS_0CCC:
            instr_data->cond_code = instr_nibl & 0x7;
            break;
         case CLASS_1CCC:
            instr_data->cond_code = instr_nibl & 0x7;
            break;
         case CLASS_0DISP7:
            instr_data->displacement = instr_byte & 0x7f;
            nibl_count += 1;
            break;
         case CLASS_1DISP7:
            instr_data->displacement = instr_byte & 0x7f;
            nibl_count += 1;
            break;
         case CLASS_01II:
            instr_data->interrupts = instr_nibl & 0x3;
            break;
         case CLASS_00II:
            instr_data->interrupts = instr_nibl & 0x3;
            break;
         case CLASS_BIT:
            /* do nothing */
            break;
         case CLASS_IR:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_FLAGS:
            instr_data->flags = instr_nibl;
            break;
         case CLASS_REG:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_REG_BYTE:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_REG_WORD:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_REG_QUAD:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_REG_LONG:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         case CLASS_REGN0:
            instr_data->arg_reg[datum_value] = instr_nibl;
            break;
         default:
            break;
      }

      loop += 1;
      nibl_count += 1;
   }
}

static void unparse_instr( instr_data )
instr_data_s  *instr_data;
{
   unsigned short tabl_datum, datum_class, datum_value;
   int            loop, loop_limit;
   char           out_str[80], tmp_str[25];

   sprintf( out_str, "\t%s\t", z8k_table[instr_data->tabl_index].name );

   loop_limit = z8k_table[instr_data->tabl_index].noperands;
   for( loop = 0; loop < loop_limit; loop++ )
   {
      if( loop )
         strcat( out_str, "," );

      tabl_datum = z8k_table[instr_data->tabl_index].arg_info[loop];
      datum_class = tabl_datum & CLASS_MASK;
      datum_value = tabl_datum & ~CLASS_MASK;

      switch( datum_class )
      {
         case CLASS_X:
            sprintf( tmp_str, "0x%0x(R%d)", instr_data->address,
                     instr_data->arg_reg[datum_value] );
            strcat( out_str, tmp_str );
            break;
         case CLASS_BA:
            sprintf( tmp_str, "r%d(#%x)", instr_data->arg_reg[datum_value],
                     instr_data->displacement);
            strcat( out_str, tmp_str );
            break;
         case CLASS_BX:
            sprintf( tmp_str, "r%d(R%d)", instr_data->arg_reg[datum_value],
                     instr_data->arg_reg[ARG_RX] );
            strcat( out_str, tmp_str );
            break;
         case CLASS_DISP:
            sprintf( tmp_str, "#0x%0x", instr_data->displacement );
            strcat( out_str, tmp_str );
            break;
         case CLASS_IMM:
            sprintf( tmp_str, "#0x%0x", instr_data->immediate );
            strcat( out_str, tmp_str );
            break;
         case CLASS_CC:
            sprintf( tmp_str, "%s", codes[instr_data->cond_code] );
            strcat( out_str, tmp_str );
            break;
         case CLASS_CTRL:
            sprintf( tmp_str, "0x%0x", instr_data->ctrl_code );
            strcat( out_str, tmp_str );
            break;
         case CLASS_DA:
         case CLASS_ADDRESS:
            sprintf( tmp_str, "#0x%0x", instr_data->address );
            strcat( out_str, tmp_str );
            break;
         case CLASS_IR:
            sprintf( tmp_str, "@R%d", instr_data->arg_reg[datum_value] );
            strcat( out_str, tmp_str );
            break;
         case CLASS_FLAGS:
            sprintf( tmp_str, "0x%0x", instr_data->flags );
            strcat( out_str, tmp_str );
            break;
         case CLASS_REG_BYTE:
            if( instr_data->arg_reg[datum_value] >= 0x8 )
            {
               sprintf( tmp_str, "rl%d",
                        instr_data->arg_reg[datum_value] - 0x8 );
            }
            else
            {
               sprintf( tmp_str, "rh%d", instr_data->arg_reg[datum_value] );
            } 
            strcat( out_str, tmp_str );
            break;
         case CLASS_REG_WORD:
            sprintf( tmp_str, "r%d", instr_data->arg_reg[datum_value] );
            strcat( out_str, tmp_str );
            break;
         case CLASS_REG_QUAD:
            sprintf( tmp_str, "rq%d", instr_data->arg_reg[datum_value] );
            strcat( out_str, tmp_str );
            break;
         case CLASS_REG_LONG:
            sprintf( tmp_str, "rr%d", instr_data->arg_reg[datum_value] );
            strcat( out_str, tmp_str );
            break;
         default:
            break;
      }
   }

   strcpy( instr_data->instr_asmsrc, out_str );
}

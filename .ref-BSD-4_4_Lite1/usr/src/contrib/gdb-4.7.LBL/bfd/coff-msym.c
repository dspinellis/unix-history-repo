/* BFD support to byte-swap a symbol table in MIPS (Third-Eye, `ecoff') format.
   Copyright 1992 Free Software Foundation, Inc.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* These routines are not yet called from BFD.  They are called from
   the MIPS symbol reading code in GDB.  However, they are in the BFD
   library because they will eventually be useful if and when BFD
   supports reading or writing of MIPS symbol tables.

   The routines in this file convert the external representation of
   ECOFF symbol tables to the internal (usual struct) representation.
   On a machine with the same byte-order and the same basic type
   sizes and alignments as a MIPS machine, this is a no-op.  
   If the symbol TEST is defined when this file is compiled, a comparison
   is made to ensure that, in fact, the output is bit-for-bit the same as
   the input.  Of course, this symbol should only be defined when
   deliberately testing the code on a machine with the proper byte sex
   and such.  */

#include "bfd.h"
#include "coff/sym.h"		/* MIPS symbols */
#include "coff/symconst.h"	/* MIPS symbols */
#include "coff/ecoff-ext.h"	/* ECOFF external struct defns */

void
ecoff_swap_hdr_in (abfd, ext_copy, intern)
     bfd *abfd;
     struct hdr_ext *ext_copy;
     HDRR *intern;
{
  struct hdr_ext ext[1];

  *ext = *ext_copy;

  intern->magic         = bfd_h_get_16 (abfd, (bfd_byte *)ext->h_magic);
  intern->vstamp        = bfd_h_get_16 (abfd, (bfd_byte *)ext->h_vstamp);
  intern->ilineMax      = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_ilineMax);
  intern->cbLine        = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbLine);
  intern->cbLineOffset  = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbLineOffset);
  intern->idnMax        = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_idnMax);
  intern->cbDnOffset    = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbDnOffset);
  intern->ipdMax        = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_ipdMax);
  intern->cbPdOffset    = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbPdOffset);
  intern->isymMax       = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_isymMax);
  intern->cbSymOffset   = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbSymOffset);
  intern->ioptMax       = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_ioptMax);
  intern->cbOptOffset   = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbOptOffset);
  intern->iauxMax       = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_iauxMax);
  intern->cbAuxOffset   = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbAuxOffset);
  intern->issMax        = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_issMax);
  intern->cbSsOffset    = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbSsOffset);
  intern->issExtMax     = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_issExtMax);
  intern->cbSsExtOffset = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbSsExtOffset);
  intern->ifdMax        = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_ifdMax);
  intern->cbFdOffset    = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbFdOffset);
  intern->crfd          = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_crfd);
  intern->cbRfdOffset   = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbRfdOffset);
  intern->iextMax       = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_iextMax);
  intern->cbExtOffset   = bfd_h_get_32 (abfd, (bfd_byte *)ext->h_cbExtOffset);

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}

/* Swap in the file descriptor record.  */

void
ecoff_swap_fdr_in (abfd, ext_copy, intern)
     bfd *abfd;
     struct fdr_ext *ext_copy;
     FDR *intern;
{
  struct fdr_ext ext[1];

  *ext = *ext_copy;		/* Make it reasonable to do in-place.  */
  
  intern->adr           = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_adr);
  intern->rss           = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_rss);
  intern->issBase       = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_issBase);
  intern->cbSs          = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_cbSs);
  intern->isymBase      = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_isymBase);
  intern->csym          = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_csym);
  intern->ilineBase     = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_ilineBase);
  intern->cline         = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_cline);
  intern->ioptBase      = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_ioptBase);
  intern->copt          = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_copt);
  intern->ipdFirst      = bfd_h_get_16 (abfd, (bfd_byte *)ext->f_ipdFirst);
  intern->cpd           = bfd_h_get_16 (abfd, (bfd_byte *)ext->f_cpd);
  intern->iauxBase      = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_iauxBase);
  intern->caux          = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_caux);
  intern->rfdBase       = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_rfdBase);
  intern->crfd          = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_crfd);

  /* now the fun stuff... */
  if (abfd->xvec->header_byteorder_big_p != false) {
    intern->lang        = (ext->f_bits1[0] & FDR_BITS1_LANG_BIG)
					>> FDR_BITS1_LANG_SH_BIG;
    intern->fMerge      = 0 != (ext->f_bits1[0] & FDR_BITS1_FMERGE_BIG);
    intern->fReadin     = 0 != (ext->f_bits1[0] & FDR_BITS1_FREADIN_BIG);
    intern->fBigendian  = 0 != (ext->f_bits1[0] & FDR_BITS1_FBIGENDIAN_BIG);
    intern->glevel      = (ext->f_bits2[0] & FDR_BITS2_GLEVEL_BIG)
					>> FDR_BITS2_GLEVEL_SH_BIG;
 /* intern->reserved  we ignore.  */
  } else {
    intern->lang        = (ext->f_bits1[0] & FDR_BITS1_LANG_LITTLE)
					>> FDR_BITS1_LANG_SH_LITTLE;
    intern->fMerge      = 0 != (ext->f_bits1[0] & FDR_BITS1_FMERGE_LITTLE);
    intern->fReadin     = 0 != (ext->f_bits1[0] & FDR_BITS1_FREADIN_LITTLE);
    intern->fBigendian  = 0 != (ext->f_bits1[0] & FDR_BITS1_FBIGENDIAN_LITTLE);
    intern->glevel      = (ext->f_bits2[0] & FDR_BITS2_GLEVEL_LITTLE)
					>> FDR_BITS2_GLEVEL_SH_LITTLE;
 /* intern->reserved  we ignore.  */
  }

  intern->cbLineOffset  = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_cbLineOffset);
  intern->cbLine        = bfd_h_get_32 (abfd, (bfd_byte *)ext->f_cbLine);

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}



/* Swap in the procedure descriptor record.  */

void
ecoff_swap_pdr_in (abfd, ext_copy, intern)
     bfd *abfd;
     struct pdr_ext *ext_copy;
     PDR *intern;
{
  struct pdr_ext ext[1];

  *ext = *ext_copy;		/* Make it reasonable to do in-place.  */
  
  intern->adr           = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_adr);
  intern->isym          = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_isym);
  intern->iline         = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_iline);
  intern->regmask       = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_regmask);
  intern->regoffset     = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_regoffset);
  intern->iopt          = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_iopt);
  intern->fregmask      = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_fregmask);
  intern->fregoffset    = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_fregoffset);
  intern->frameoffset   = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_frameoffset);
  intern->framereg      = bfd_h_get_16 (abfd, (bfd_byte *)ext->p_framereg);
  intern->pcreg         = bfd_h_get_16 (abfd, (bfd_byte *)ext->p_pcreg);
  intern->lnLow         = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_lnLow);
  intern->lnHigh        = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_lnHigh);
  intern->cbLineOffset  = bfd_h_get_32 (abfd, (bfd_byte *)ext->p_cbLineOffset);

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}


/* Swap in a symbol record.  */

void
ecoff_swap_sym_in (abfd, ext_copy, intern)
     bfd *abfd;
     struct sym_ext *ext_copy;
     SYMR *intern;
{
  struct sym_ext ext[1];

  *ext = *ext_copy;		/* Make it reasonable to do in-place.  */
  
  intern->iss           = bfd_h_get_32 (abfd, (bfd_byte *)ext->s_iss);
  intern->value         = bfd_h_get_32 (abfd, (bfd_byte *)ext->s_value);

  /* now the fun stuff... */
  if (abfd->xvec->header_byteorder_big_p != false) {
    intern->st          =  (ext->s_bits1[0] & SYM_BITS1_ST_BIG)
					   >> SYM_BITS1_ST_SH_BIG;
    intern->sc          = ((ext->s_bits1[0] & SYM_BITS1_SC_BIG)
					   << SYM_BITS1_SC_SH_LEFT_BIG)
			| ((ext->s_bits2[0] & SYM_BITS2_SC_BIG)
					   >> SYM_BITS2_SC_SH_BIG);
    intern->reserved    = 0 != (ext->s_bits2[0] & SYM_BITS2_RESERVED_BIG);
    intern->index       = ((ext->s_bits2[0] & SYM_BITS2_INDEX_BIG)
					   << SYM_BITS2_INDEX_SH_LEFT_BIG)
			| (ext->s_bits3[0] << SYM_BITS3_INDEX_SH_LEFT_BIG)
			| (ext->s_bits4[0] << SYM_BITS4_INDEX_SH_LEFT_BIG);
  } else {
    intern->st          =  (ext->s_bits1[0] & SYM_BITS1_ST_LITTLE)
					   >> SYM_BITS1_ST_SH_LITTLE;
    intern->sc          = ((ext->s_bits1[0] & SYM_BITS1_SC_LITTLE)
					   >> SYM_BITS1_SC_SH_LITTLE)
			| ((ext->s_bits2[0] & SYM_BITS2_SC_LITTLE)
					   << SYM_BITS2_SC_SH_LEFT_LITTLE);
    intern->reserved    = 0 != (ext->s_bits2[0] & SYM_BITS2_RESERVED_LITTLE);
    intern->index       = ((ext->s_bits2[0] & SYM_BITS2_INDEX_LITTLE)
					   >> SYM_BITS2_INDEX_SH_LITTLE)
			| (ext->s_bits3[0] << SYM_BITS3_INDEX_SH_LEFT_LITTLE)
			| (ext->s_bits4[0] << SYM_BITS4_INDEX_SH_LEFT_LITTLE);
  }

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}


/* Swap in an external symbol record.  */

void
ecoff_swap_ext_in (abfd, ext_copy, intern)
     bfd *abfd;
     struct ext_ext *ext_copy;
     EXTR *intern;
{
  struct ext_ext ext[1];

  *ext = *ext_copy;		/* Make it reasonable to do in-place.  */
  
  /* now the fun stuff... */
  if (abfd->xvec->header_byteorder_big_p != false) {
    intern->jmptbl      = 0 != (ext->es_bits1[0] & EXT_BITS1_JMPTBL_BIG);
    intern->cobol_main  = 0 != (ext->es_bits1[0] & EXT_BITS1_COBOL_MAIN_BIG);
    intern->weakext     = 0 != (ext->es_bits1[0] & EXT_BITS1_WEAKEXT_BIG);
  } else {
    intern->jmptbl      = 0 != (ext->es_bits1[0] & EXT_BITS1_JMPTBL_LITTLE);
    intern->cobol_main  = 0 != (ext->es_bits1[0] & EXT_BITS1_COBOL_MAIN_LITTLE);
    intern->weakext     = 0 != (ext->es_bits1[0] & EXT_BITS1_WEAKEXT_LITTLE);
  }

  intern->ifd           = bfd_h_get_16 (abfd, (bfd_byte *)ext->es_ifd);
  ecoff_swap_sym_in (abfd, &ext->es_asym, &intern->asym);

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}

/* Swap in a type information record.
   BIGEND says whether AUX symbols are big-endian or little-endian; this
   info comes from the file header record (fh-fBigendian).  */

void
ecoff_swap_tir_in (bigend, ext_copy, intern)
     int bigend;
     struct tir_ext *ext_copy;
     TIR *intern;
{
  struct tir_ext ext[1];

  *ext = *ext_copy;		/* Make it reasonable to do in-place.  */
  
  /* now the fun stuff... */
  if (bigend) {
    intern->fBitfield   = 0 != (ext->t_bits1[0] & TIR_BITS1_FBITFIELD_BIG);
    intern->continued   = 0 != (ext->t_bits1[0] & TIR_BITS1_CONTINUED_BIG);
    intern->bt          = (ext->t_bits1[0] & TIR_BITS1_BT_BIG)
			>>		    TIR_BITS1_BT_SH_BIG;
    intern->tq4         = (ext->t_tq45[0] & TIR_BITS_TQ4_BIG)
			>>		    TIR_BITS_TQ4_SH_BIG;
    intern->tq5         = (ext->t_tq45[0] & TIR_BITS_TQ5_BIG)
			>>		    TIR_BITS_TQ5_SH_BIG;
    intern->tq0         = (ext->t_tq01[0] & TIR_BITS_TQ0_BIG)
			>>		    TIR_BITS_TQ0_SH_BIG;
    intern->tq1         = (ext->t_tq01[0] & TIR_BITS_TQ1_BIG)
			>>		    TIR_BITS_TQ1_SH_BIG;
    intern->tq2         = (ext->t_tq23[0] & TIR_BITS_TQ2_BIG)
			>>		    TIR_BITS_TQ2_SH_BIG;
    intern->tq3         = (ext->t_tq23[0] & TIR_BITS_TQ3_BIG)
			>>		    TIR_BITS_TQ3_SH_BIG;
  } else {
    intern->fBitfield   = 0 != (ext->t_bits1[0] & TIR_BITS1_FBITFIELD_LITTLE);
    intern->continued   = 0 != (ext->t_bits1[0] & TIR_BITS1_CONTINUED_LITTLE);
    intern->bt          = (ext->t_bits1[0] & TIR_BITS1_BT_LITTLE)
			>>		    TIR_BITS1_BT_SH_LITTLE;
    intern->tq4         = (ext->t_tq45[0] & TIR_BITS_TQ4_LITTLE)
			>>		    TIR_BITS_TQ4_SH_LITTLE;
    intern->tq5         = (ext->t_tq45[0] & TIR_BITS_TQ5_LITTLE)
			>>		    TIR_BITS_TQ5_SH_LITTLE;
    intern->tq0         = (ext->t_tq01[0] & TIR_BITS_TQ0_LITTLE)
			>>		    TIR_BITS_TQ0_SH_LITTLE;
    intern->tq1         = (ext->t_tq01[0] & TIR_BITS_TQ1_LITTLE)
			>>		    TIR_BITS_TQ1_SH_LITTLE;
    intern->tq2         = (ext->t_tq23[0] & TIR_BITS_TQ2_LITTLE)
			>>		    TIR_BITS_TQ2_SH_LITTLE;
    intern->tq3         = (ext->t_tq23[0] & TIR_BITS_TQ3_LITTLE)
			>>		    TIR_BITS_TQ3_SH_LITTLE;
  }

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}

/* Swap in a relative symbol record.  BIGEND says whether it is in
   big-endian or little-endian format.*/

void
ecoff_swap_rndx_in (bigend, ext_copy, intern)
     int bigend;
     struct rndx_ext *ext_copy;
     RNDXR *intern;
{
  struct rndx_ext ext[1];

  *ext = *ext_copy;		/* Make it reasonable to do in-place.  */
  
  /* now the fun stuff... */
  if (bigend) {
    intern->rfd   = (ext->r_bits[0] << RNDX_BITS0_RFD_SH_LEFT_BIG)
		  | ((ext->r_bits[1] & RNDX_BITS1_RFD_BIG)
		    		    >> RNDX_BITS1_RFD_SH_BIG);
    intern->index = ((ext->r_bits[1] & RNDX_BITS1_INDEX_BIG)
		    		    << RNDX_BITS1_INDEX_SH_LEFT_BIG)
		  | (ext->r_bits[2] << RNDX_BITS2_INDEX_SH_LEFT_BIG)
		  | (ext->r_bits[3] << RNDX_BITS3_INDEX_SH_LEFT_BIG);
  } else {
    intern->rfd   = (ext->r_bits[0] << RNDX_BITS0_RFD_SH_LEFT_LITTLE)
		  | ((ext->r_bits[1] & RNDX_BITS1_RFD_LITTLE)
		    		    << RNDX_BITS1_RFD_SH_LEFT_LITTLE);
    intern->index = ((ext->r_bits[1] & RNDX_BITS1_INDEX_LITTLE)
		    		    >> RNDX_BITS1_INDEX_SH_LITTLE)
		  | (ext->r_bits[2] << RNDX_BITS2_INDEX_SH_LEFT_LITTLE)
		  | (ext->r_bits[3] << RNDX_BITS3_INDEX_SH_LEFT_LITTLE);
  }

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}

/* Swap in a relative file descriptor.  */

void
ecoff_swap_rfd_in (abfd, ext, intern)
     bfd *abfd;
     struct rfd_ext *ext;
     RFDT *intern;
{
  
  *intern = bfd_h_get_32 (abfd, (bfd_byte *)ext->rfd);

#ifdef TEST
  if (memcmp ((char *)ext, (char *)intern, sizeof (*intern)) != 0)
    abort();
#endif
}

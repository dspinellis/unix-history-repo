//===- ARCInstPrinter.h - Convert ARC MCInst to assembly syntax -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file contains the declaration of the ARCInstPrinter class,
/// which is used to print ARC MCInst to a .s file.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_ARC_INSTPRINTER_ARCINSTPRINTER_H
#define LLVM_LIB_TARGET_ARC_INSTPRINTER_ARCINSTPRINTER_H

#include "llvm/MC/MCInstPrinter.h"

namespace llvm {

class ARCInstPrinter : public MCInstPrinter {
public:
  ARCInstPrinter(const MCAsmInfo &MAI, const MCInstrInfo &MII,
                 const MCRegisterInfo &MRI)
      : MCInstPrinter(MAI, MII, MRI) {}

  // Autogenerated by tblgen.
  void printInstruction(const MCInst *MI, raw_ostream &O);
  static const char *getRegisterName(unsigned RegNo);

  void printRegName(raw_ostream &OS, unsigned RegNo) const override;
  void printInst(const MCInst *MI, raw_ostream &O, StringRef Annot,
                 const MCSubtargetInfo &STI) override;

private:
  void printMemOperandRI(const MCInst *MI, unsigned OpNum, raw_ostream &O);
  void printOperand(const MCInst *MI, unsigned OpNum, raw_ostream &O);
  void printPredicateOperand(const MCInst *MI, unsigned OpNum, raw_ostream &O);
  void printBRCCPredicateOperand(const MCInst *MI, unsigned OpNum,
                                 raw_ostream &O);
};
} // end namespace llvm

#endif // LLVM_LIB_TARGET_ARC_INSTPRINTER_ARCINSTPRINTER_H

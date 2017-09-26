//===----- X86CallFrameOptimization.cpp - Optimize x86 call sequences -----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a pass that optimizes call sequences on x86.
// Currently, it converts movs of function parameters onto the stack into
// pushes. This is beneficial for two main reasons:
// 1) The push instruction encoding is much smaller than a stack-ptr-based mov.
// 2) It is possible to push memory arguments directly. So, if the
//    the transformation is performed pre-reg-alloc, it can help relieve
//    register pressure.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/X86BaseInfo.h"
#include "X86FrameLowering.h"
#include "X86InstrInfo.h"
#include "X86MachineFunctionInfo.h"
#include "X86RegisterInfo.h"
#include "X86Subtarget.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCDwarf.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>

using namespace llvm;

#define DEBUG_TYPE "x86-cf-opt"

static cl::opt<bool>
    NoX86CFOpt("no-x86-call-frame-opt",
               cl::desc("Avoid optimizing x86 call frames for size"),
               cl::init(false), cl::Hidden);

namespace {

class X86CallFrameOptimization : public MachineFunctionPass {
public:
  X86CallFrameOptimization() : MachineFunctionPass(ID) {}

  bool runOnMachineFunction(MachineFunction &MF) override;

private:
  // Information we know about a particular call site
  struct CallContext {
    CallContext() : FrameSetup(nullptr), MovVector(4, nullptr) {}

    // Iterator referring to the frame setup instruction
    MachineBasicBlock::iterator FrameSetup;

    // Actual call instruction
    MachineInstr *Call = nullptr;

    // A copy of the stack pointer
    MachineInstr *SPCopy = nullptr;

    // The total displacement of all passed parameters
    int64_t ExpectedDist = 0;

    // The sequence of movs used to pass the parameters
    SmallVector<MachineInstr *, 4> MovVector;

    // True if this call site has no stack parameters
    bool NoStackParams = false;

    // True if this call site can use push instructions
    bool UsePush = false;
  };

  typedef SmallVector<CallContext, 8> ContextVector;

  bool isLegal(MachineFunction &MF);

  bool isProfitable(MachineFunction &MF, ContextVector &CallSeqMap);

  void collectCallInfo(MachineFunction &MF, MachineBasicBlock &MBB,
                       MachineBasicBlock::iterator I, CallContext &Context);

  void adjustCallSequence(MachineFunction &MF, const CallContext &Context);

  MachineInstr *canFoldIntoRegPush(MachineBasicBlock::iterator FrameSetup,
                                   unsigned Reg);

  enum InstClassification { Convert, Skip, Exit };

  InstClassification classifyInstruction(MachineBasicBlock &MBB,
                                         MachineBasicBlock::iterator MI,
                                         const X86RegisterInfo &RegInfo,
                                         DenseSet<unsigned int> &UsedRegs);

  StringRef getPassName() const override { return "X86 Optimize Call Frame"; }

  const X86InstrInfo *TII;
  const X86FrameLowering *TFL;
  const X86Subtarget *STI;
  MachineRegisterInfo *MRI;
  unsigned SlotSize;
  unsigned Log2SlotSize;
  static char ID;
};

char X86CallFrameOptimization::ID = 0;

} // end anonymous namespace

// This checks whether the transformation is legal.
// Also returns false in cases where it's potentially legal, but
// we don't even want to try.
bool X86CallFrameOptimization::isLegal(MachineFunction &MF) {
  if (NoX86CFOpt.getValue())
    return false;

  // Work around LLVM PR30879 (bad interaction between CFO and libunwind)
  if (STI->isTargetFreeBSD() && STI->is32Bit() &&
      STI->getTargetTriple().getOSMajorVersion() >= 12)
    return false;

  // We can't encode multiple DW_CFA_GNU_args_size or DW_CFA_def_cfa_offset
  // in the compact unwind encoding that Darwin uses. So, bail if there
  // is a danger of that being generated.
  if (STI->isTargetDarwin() &&
      (!MF.getLandingPads().empty() ||
       (MF.getFunction()->needsUnwindTableEntry() && !TFL->hasFP(MF))))
    return false;

  // It is not valid to change the stack pointer outside the prolog/epilog
  // on 64-bit Windows.
  if (STI->isTargetWin64())
    return false;

  // You would expect straight-line code between call-frame setup and
  // call-frame destroy. You would be wrong. There are circumstances (e.g.
  // CMOV_GR8 expansion of a select that feeds a function call!) where we can
  // end up with the setup and the destroy in different basic blocks.
  // This is bad, and breaks SP adjustment.
  // So, check that all of the frames in the function are closed inside
  // the same block, and, for good measure, that there are no nested frames.
  unsigned FrameSetupOpcode = TII->getCallFrameSetupOpcode();
  unsigned FrameDestroyOpcode = TII->getCallFrameDestroyOpcode();
  for (MachineBasicBlock &BB : MF) {
    bool InsideFrameSequence = false;
    for (MachineInstr &MI : BB) {
      if (MI.getOpcode() == FrameSetupOpcode) {
        if (InsideFrameSequence)
          return false;
        InsideFrameSequence = true;
      } else if (MI.getOpcode() == FrameDestroyOpcode) {
        if (!InsideFrameSequence)
          return false;
        InsideFrameSequence = false;
      }
    }

    if (InsideFrameSequence)
      return false;
  }

  return true;
}

// Check whether this transformation is profitable for a particular
// function - in terms of code size.
bool X86CallFrameOptimization::isProfitable(MachineFunction &MF,
                                            ContextVector &CallSeqVector) {
  // This transformation is always a win when we do not expect to have
  // a reserved call frame. Under other circumstances, it may be either
  // a win or a loss, and requires a heuristic.
  bool CannotReserveFrame = MF.getFrameInfo().hasVarSizedObjects();
  if (CannotReserveFrame)
    return true;

  unsigned StackAlign = TFL->getStackAlignment();

  int64_t Advantage = 0;
  for (auto CC : CallSeqVector) {
    // Call sites where no parameters are passed on the stack
    // do not affect the cost, since there needs to be no
    // stack adjustment.
    if (CC.NoStackParams)
      continue;

    if (!CC.UsePush) {
      // If we don't use pushes for a particular call site,
      // we pay for not having a reserved call frame with an
      // additional sub/add esp pair. The cost is ~3 bytes per instruction,
      // depending on the size of the constant.
      // TODO: Callee-pop functions should have a smaller penalty, because
      // an add is needed even with a reserved call frame.
      Advantage -= 6;
    } else {
      // We can use pushes. First, account for the fixed costs.
      // We'll need a add after the call.
      Advantage -= 3;
      // If we have to realign the stack, we'll also need a sub before
      if (CC.ExpectedDist % StackAlign)
        Advantage -= 3;
      // Now, for each push, we save ~3 bytes. For small constants, we actually,
      // save more (up to 5 bytes), but 3 should be a good approximation.
      Advantage += (CC.ExpectedDist >> Log2SlotSize) * 3;
    }
  }

  return Advantage >= 0;
}

bool X86CallFrameOptimization::runOnMachineFunction(MachineFunction &MF) {
  STI = &MF.getSubtarget<X86Subtarget>();
  TII = STI->getInstrInfo();
  TFL = STI->getFrameLowering();
  MRI = &MF.getRegInfo();

  const X86RegisterInfo &RegInfo =
      *static_cast<const X86RegisterInfo *>(STI->getRegisterInfo());
  SlotSize = RegInfo.getSlotSize();
  assert(isPowerOf2_32(SlotSize) && "Expect power of 2 stack slot size");
  Log2SlotSize = Log2_32(SlotSize);

  if (skipFunction(*MF.getFunction()) || !isLegal(MF))
    return false;

  unsigned FrameSetupOpcode = TII->getCallFrameSetupOpcode();

  bool Changed = false;

  ContextVector CallSeqVector;

  for (auto &MBB : MF)
    for (auto &MI : MBB)
      if (MI.getOpcode() == FrameSetupOpcode) {
        CallContext Context;
        collectCallInfo(MF, MBB, MI, Context);
        CallSeqVector.push_back(Context);
      }

  if (!isProfitable(MF, CallSeqVector))
    return false;

  for (auto CC : CallSeqVector) {
    if (CC.UsePush) {
      adjustCallSequence(MF, CC);
      Changed = true;
    }
  }

  return Changed;
}

X86CallFrameOptimization::InstClassification
X86CallFrameOptimization::classifyInstruction(
    MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
    const X86RegisterInfo &RegInfo, DenseSet<unsigned int> &UsedRegs) {
  if (MI == MBB.end())
    return Exit;

  // The instructions we actually care about are movs onto the stack
  int Opcode = MI->getOpcode();
  if (Opcode == X86::MOV32mi   || Opcode == X86::MOV32mr ||
      Opcode == X86::MOV64mi32 || Opcode == X86::MOV64mr)
    return Convert;

  // Not all calling conventions have only stack MOVs between the stack
  // adjust and the call.

  // We want to tolerate other instructions, to cover more cases.
  // In particular:
  // a) PCrel calls, where we expect an additional COPY of the basereg.
  // b) Passing frame-index addresses.
  // c) Calling conventions that have inreg parameters. These generate
  //    both copies and movs into registers.
  // To avoid creating lots of special cases, allow any instruction
  // that does not write into memory, does not def or use the stack
  // pointer, and does not def any register that was used by a preceding
  // push.
  // (Reading from memory is allowed, even if referenced through a
  // frame index, since these will get adjusted properly in PEI)

  // The reason for the last condition is that the pushes can't replace
  // the movs in place, because the order must be reversed.
  // So if we have a MOV32mr that uses EDX, then an instruction that defs
  // EDX, and then the call, after the transformation the push will use
  // the modified version of EDX, and not the original one.
  // Since we are still in SSA form at this point, we only need to
  // make sure we don't clobber any *physical* registers that were
  // used by an earlier mov that will become a push.

  if (MI->isCall() || MI->mayStore())
    return Exit;

  for (const MachineOperand &MO : MI->operands()) {
    if (!MO.isReg())
      continue;
    unsigned int Reg = MO.getReg();
    if (!RegInfo.isPhysicalRegister(Reg))
      continue;
    if (RegInfo.regsOverlap(Reg, RegInfo.getStackRegister()))
      return Exit;
    if (MO.isDef()) {
      for (unsigned int U : UsedRegs)
        if (RegInfo.regsOverlap(Reg, U))
          return Exit;
    }
  }

  return Skip;
}

void X86CallFrameOptimization::collectCallInfo(MachineFunction &MF,
                                               MachineBasicBlock &MBB,
                                               MachineBasicBlock::iterator I,
                                               CallContext &Context) {
  // Check that this particular call sequence is amenable to the
  // transformation.
  const X86RegisterInfo &RegInfo =
      *static_cast<const X86RegisterInfo *>(STI->getRegisterInfo());

  // We expect to enter this at the beginning of a call sequence
  assert(I->getOpcode() == TII->getCallFrameSetupOpcode());
  MachineBasicBlock::iterator FrameSetup = I++;
  Context.FrameSetup = FrameSetup;

  // How much do we adjust the stack? This puts an upper bound on
  // the number of parameters actually passed on it.
  unsigned int MaxAdjust = TII->getFrameSize(*FrameSetup) >> Log2SlotSize;

  // A zero adjustment means no stack parameters
  if (!MaxAdjust) {
    Context.NoStackParams = true;
    return;
  }

  // Skip over DEBUG_VALUE.
  // For globals in PIC mode, we can have some LEAs here. Skip them as well.
  // TODO: Extend this to something that covers more cases.
  while (I->getOpcode() == X86::LEA32r || I->isDebugValue())
    ++I;

  unsigned StackPtr = RegInfo.getStackRegister();
  // SelectionDAG (but not FastISel) inserts a copy of ESP into a virtual
  // register here.  If it's there, use that virtual register as stack pointer
  // instead.
  if (I->isCopy() && I->getOperand(0).isReg() && I->getOperand(1).isReg() &&
      I->getOperand(1).getReg() == StackPtr) {
    Context.SPCopy = &*I++;
    StackPtr = Context.SPCopy->getOperand(0).getReg();
  }

  // Scan the call setup sequence for the pattern we're looking for.
  // We only handle a simple case - a sequence of store instructions that
  // push a sequence of stack-slot-aligned values onto the stack, with
  // no gaps between them.
  if (MaxAdjust > 4)
    Context.MovVector.resize(MaxAdjust, nullptr);

  InstClassification Classification;
  DenseSet<unsigned int> UsedRegs;

  while ((Classification = classifyInstruction(MBB, I, RegInfo, UsedRegs)) !=
         Exit) {
    if (Classification == Skip) {
      ++I;
      continue;
    }

    // We know the instruction has a supported store opcode.
    // We only want movs of the form:
    // mov imm/reg, k(%StackPtr)
    // If we run into something else, bail.
    // Note that AddrBaseReg may, counter to its name, not be a register,
    // but rather a frame index.
    // TODO: Support the fi case. This should probably work now that we
    // have the infrastructure to track the stack pointer within a call
    // sequence.
    if (!I->getOperand(X86::AddrBaseReg).isReg() ||
        (I->getOperand(X86::AddrBaseReg).getReg() != StackPtr) ||
        !I->getOperand(X86::AddrScaleAmt).isImm() ||
        (I->getOperand(X86::AddrScaleAmt).getImm() != 1) ||
        (I->getOperand(X86::AddrIndexReg).getReg() != X86::NoRegister) ||
        (I->getOperand(X86::AddrSegmentReg).getReg() != X86::NoRegister) ||
        !I->getOperand(X86::AddrDisp).isImm())
      return;

    int64_t StackDisp = I->getOperand(X86::AddrDisp).getImm();
    assert(StackDisp >= 0 &&
           "Negative stack displacement when passing parameters");

    // We really don't want to consider the unaligned case.
    if (StackDisp & (SlotSize - 1))
      return;
    StackDisp >>= Log2SlotSize;

    assert((size_t)StackDisp < Context.MovVector.size() &&
           "Function call has more parameters than the stack is adjusted for.");

    // If the same stack slot is being filled twice, something's fishy.
    if (Context.MovVector[StackDisp] != nullptr)
      return;
    Context.MovVector[StackDisp] = &*I;

    for (const MachineOperand &MO : I->uses()) {
      if (!MO.isReg())
        continue;
      unsigned int Reg = MO.getReg();
      if (RegInfo.isPhysicalRegister(Reg))
        UsedRegs.insert(Reg);
    }

    ++I;
  }

  // We now expect the end of the sequence. If we stopped early,
  // or reached the end of the block without finding a call, bail.
  if (I == MBB.end() || !I->isCall())
    return;

  Context.Call = &*I;
  if ((++I)->getOpcode() != TII->getCallFrameDestroyOpcode())
    return;

  // Now, go through the vector, and see that we don't have any gaps,
  // but only a series of MOVs.
  auto MMI = Context.MovVector.begin(), MME = Context.MovVector.end();
  for (; MMI != MME; ++MMI, Context.ExpectedDist += SlotSize)
    if (*MMI == nullptr)
      break;

  // If the call had no parameters, do nothing
  if (MMI == Context.MovVector.begin())
    return;

  // We are either at the last parameter, or a gap.
  // Make sure it's not a gap
  for (; MMI != MME; ++MMI)
    if (*MMI != nullptr)
      return;

  Context.UsePush = true;
}

void X86CallFrameOptimization::adjustCallSequence(MachineFunction &MF,
                                                  const CallContext &Context) {
  // Ok, we can in fact do the transformation for this call.
  // Do not remove the FrameSetup instruction, but adjust the parameters.
  // PEI will end up finalizing the handling of this.
  MachineBasicBlock::iterator FrameSetup = Context.FrameSetup;
  MachineBasicBlock &MBB = *(FrameSetup->getParent());
  TII->setFrameAdjustment(*FrameSetup, Context.ExpectedDist);

  DebugLoc DL = FrameSetup->getDebugLoc();
  bool Is64Bit = STI->is64Bit();
  // Now, iterate through the vector in reverse order, and replace the movs
  // with pushes. MOVmi/MOVmr doesn't have any defs, so no need to
  // replace uses.
  for (int Idx = (Context.ExpectedDist >> Log2SlotSize) - 1; Idx >= 0; --Idx) {
    MachineBasicBlock::iterator MOV = *Context.MovVector[Idx];
    MachineOperand PushOp = MOV->getOperand(X86::AddrNumOperands);
    MachineBasicBlock::iterator Push = nullptr;
    unsigned PushOpcode;
    switch (MOV->getOpcode()) {
    default:
      llvm_unreachable("Unexpected Opcode!");
    case X86::MOV32mi:
    case X86::MOV64mi32:
      PushOpcode = Is64Bit ? X86::PUSH64i32 : X86::PUSHi32;
      // If the operand is a small (8-bit) immediate, we can use a
      // PUSH instruction with a shorter encoding.
      // Note that isImm() may fail even though this is a MOVmi, because
      // the operand can also be a symbol.
      if (PushOp.isImm()) {
        int64_t Val = PushOp.getImm();
        if (isInt<8>(Val))
          PushOpcode = Is64Bit ? X86::PUSH64i8 : X86::PUSH32i8;
      }
      Push = BuildMI(MBB, Context.Call, DL, TII->get(PushOpcode)).add(PushOp);
      break;
    case X86::MOV32mr:
    case X86::MOV64mr: {
      unsigned int Reg = PushOp.getReg();

      // If storing a 32-bit vreg on 64-bit targets, extend to a 64-bit vreg
      // in preparation for the PUSH64. The upper 32 bits can be undef.
      if (Is64Bit && MOV->getOpcode() == X86::MOV32mr) {
        unsigned UndefReg = MRI->createVirtualRegister(&X86::GR64RegClass);
        Reg = MRI->createVirtualRegister(&X86::GR64RegClass);
        BuildMI(MBB, Context.Call, DL, TII->get(X86::IMPLICIT_DEF), UndefReg);
        BuildMI(MBB, Context.Call, DL, TII->get(X86::INSERT_SUBREG), Reg)
            .addReg(UndefReg)
            .add(PushOp)
            .addImm(X86::sub_32bit);
      }

      // If PUSHrmm is not slow on this target, try to fold the source of the
      // push into the instruction.
      bool SlowPUSHrmm = STI->isAtom() || STI->isSLM();

      // Check that this is legal to fold. Right now, we're extremely
      // conservative about that.
      MachineInstr *DefMov = nullptr;
      if (!SlowPUSHrmm && (DefMov = canFoldIntoRegPush(FrameSetup, Reg))) {
        PushOpcode = Is64Bit ? X86::PUSH64rmm : X86::PUSH32rmm;
        Push = BuildMI(MBB, Context.Call, DL, TII->get(PushOpcode));

        unsigned NumOps = DefMov->getDesc().getNumOperands();
        for (unsigned i = NumOps - X86::AddrNumOperands; i != NumOps; ++i)
          Push->addOperand(DefMov->getOperand(i));

        DefMov->eraseFromParent();
      } else {
        PushOpcode = Is64Bit ? X86::PUSH64r : X86::PUSH32r;
        Push = BuildMI(MBB, Context.Call, DL, TII->get(PushOpcode))
                   .addReg(Reg)
                   .getInstr();
      }
      break;
    }
    }

    // For debugging, when using SP-based CFA, we need to adjust the CFA
    // offset after each push.
    // TODO: This is needed only if we require precise CFA.
    if (!TFL->hasFP(MF))
      TFL->BuildCFI(
          MBB, std::next(Push), DL,
          MCCFIInstruction::createAdjustCfaOffset(nullptr, SlotSize));

    MBB.erase(MOV);
  }

  // The stack-pointer copy is no longer used in the call sequences.
  // There should not be any other users, but we can't commit to that, so:
  if (Context.SPCopy && MRI->use_empty(Context.SPCopy->getOperand(0).getReg()))
    Context.SPCopy->eraseFromParent();

  // Once we've done this, we need to make sure PEI doesn't assume a reserved
  // frame.
  X86MachineFunctionInfo *FuncInfo = MF.getInfo<X86MachineFunctionInfo>();
  FuncInfo->setHasPushSequences(true);
}

MachineInstr *X86CallFrameOptimization::canFoldIntoRegPush(
    MachineBasicBlock::iterator FrameSetup, unsigned Reg) {
  // Do an extremely restricted form of load folding.
  // ISel will often create patterns like:
  // movl    4(%edi), %eax
  // movl    8(%edi), %ecx
  // movl    12(%edi), %edx
  // movl    %edx, 8(%esp)
  // movl    %ecx, 4(%esp)
  // movl    %eax, (%esp)
  // call
  // Get rid of those with prejudice.
  if (!TargetRegisterInfo::isVirtualRegister(Reg))
    return nullptr;

  // Make sure this is the only use of Reg.
  if (!MRI->hasOneNonDBGUse(Reg))
    return nullptr;

  MachineInstr &DefMI = *MRI->getVRegDef(Reg);

  // Make sure the def is a MOV from memory.
  // If the def is in another block, give up.
  if ((DefMI.getOpcode() != X86::MOV32rm &&
       DefMI.getOpcode() != X86::MOV64rm) ||
      DefMI.getParent() != FrameSetup->getParent())
    return nullptr;

  // Make sure we don't have any instructions between DefMI and the
  // push that make folding the load illegal.
  for (MachineBasicBlock::iterator I = DefMI; I != FrameSetup; ++I)
    if (I->isLoadFoldBarrier())
      return nullptr;

  return &DefMI;
}

FunctionPass *llvm::createX86CallFrameOptimization() {
  return new X86CallFrameOptimization();
}

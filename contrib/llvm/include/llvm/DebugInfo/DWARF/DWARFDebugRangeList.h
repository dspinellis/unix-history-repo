//===- DWARFDebugRangeList.h ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_DEBUGINFO_DWARF_DWARFDEBUGRANGELIST_H
#define LLVM_DEBUGINFO_DWARF_DWARFDEBUGRANGELIST_H

#include "llvm/DebugInfo/DWARF/DWARFDataExtractor.h"
#include "llvm/DebugInfo/DWARF/DWARFRelocMap.h"
#include <cassert>
#include <cstdint>
#include <vector>

namespace llvm {

struct BaseAddress;
class raw_ostream;

struct DWARFAddressRange {
  uint64_t LowPC;
  uint64_t HighPC;
  uint64_t SectionIndex;

  DWARFAddressRange() = default;

  /// Used for unit testing.
  DWARFAddressRange(uint64_t LowPC, uint64_t HighPC, uint64_t SectionIndex = 0)
      : LowPC(LowPC), HighPC(HighPC), SectionIndex(SectionIndex) {}

  /// Returns true if LowPC is smaller or equal to HighPC. This accounts for
  /// dead-stripped ranges.
  bool valid() const { return LowPC <= HighPC; }

  /// Returns true if [LowPC, HighPC) intersects with [RHS.LowPC, RHS.HighPC).
  bool intersects(const DWARFAddressRange &RHS) const {
    // Empty ranges can't intersect.
    if (LowPC == HighPC || RHS.LowPC == RHS.HighPC)
      return false;
    return (LowPC < RHS.HighPC) && (HighPC > RHS.LowPC);
  }

  /// Returns true if [LowPC, HighPC) fully contains [RHS.LowPC, RHS.HighPC).
  bool contains(const DWARFAddressRange &RHS) const {
    if (LowPC <= RHS.LowPC && RHS.LowPC <= HighPC)
      return LowPC <= RHS.HighPC && RHS.HighPC <= HighPC;
    return false;
  }
};

static inline bool operator<(const DWARFAddressRange &LHS,
                             const DWARFAddressRange &RHS) {
  return std::tie(LHS.LowPC, LHS.HighPC) < std::tie(RHS.LowPC, RHS.HighPC);
}

raw_ostream &operator<<(raw_ostream &OS, const DWARFAddressRange &R);

/// DWARFAddressRangesVector - represents a set of absolute address ranges.
using DWARFAddressRangesVector = std::vector<DWARFAddressRange>;

class DWARFDebugRangeList {
public:
  struct RangeListEntry {
    /// A beginning address offset. This address offset has the size of an
    /// address and is relative to the applicable base address of the
    /// compilation unit referencing this range list. It marks the beginning
    /// of an address range.
    uint64_t StartAddress;
    /// An ending address offset. This address offset again has the size of
    /// an address and is relative to the applicable base address of the
    /// compilation unit referencing this range list. It marks the first
    /// address past the end of the address range. The ending address must
    /// be greater than or equal to the beginning address.
    uint64_t EndAddress;
    /// A section index this range belongs to.
    uint64_t SectionIndex;

    /// The end of any given range list is marked by an end of list entry,
    /// which consists of a 0 for the beginning address offset
    /// and a 0 for the ending address offset.
    bool isEndOfListEntry() const {
      return (StartAddress == 0) && (EndAddress == 0);
    }

    /// A base address selection entry consists of:
    /// 1. The value of the largest representable address offset
    /// (for example, 0xffffffff when the size of an address is 32 bits).
    /// 2. An address, which defines the appropriate base address for
    /// use in interpreting the beginning and ending address offsets of
    /// subsequent entries of the location list.
    bool isBaseAddressSelectionEntry(uint8_t AddressSize) const {
      assert(AddressSize == 4 || AddressSize == 8);
      if (AddressSize == 4)
        return StartAddress == -1U;
      else
        return StartAddress == -1ULL;
    }
  };

private:
  /// Offset in .debug_ranges section.
  uint32_t Offset;
  uint8_t AddressSize;
  std::vector<RangeListEntry> Entries;

public:
  DWARFDebugRangeList() { clear(); }

  void clear();
  void dump(raw_ostream &OS) const;
  bool extract(const DWARFDataExtractor &data, uint32_t *offset_ptr);
  const std::vector<RangeListEntry> &getEntries() { return Entries; }

  /// getAbsoluteRanges - Returns absolute address ranges defined by this range
  /// list. Has to be passed base address of the compile unit referencing this
  /// range list.
  DWARFAddressRangesVector
  getAbsoluteRanges(llvm::Optional<BaseAddress> BaseAddr) const;
};

} // end namespace llvm

#endif // LLVM_DEBUGINFO_DWARF_DWARFDEBUGRANGELIST_H

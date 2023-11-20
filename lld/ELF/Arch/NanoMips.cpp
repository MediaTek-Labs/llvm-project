#include "InputFiles.h"
#include "OutputSections.h"
#include "Symbols.h"
#include "SyntheticSections.h"
#include "Target.h"
#include "Thunks.h"
#include "lld/Common/ErrorHandler.h"
#include "llvm/Object/ELF.h"

using namespace llvm;
using namespace llvm::object;
using namespace llvm::ELF;
using namespace lld;
using namespace lld::elf;

uint32_t bswap(uint32_t data){
  uint32_t lo = ((data & 0xffff) << 16) & ~0xffff;
  uint32_t hi = ((data & ~0xffff) >> 16) & 0xffff;
  return lo | hi;

}

uint64_t elf::getNanoMipsPage(uint64_t expr) {
  return expr & ~static_cast<uint64_t>(0xFFF);
}

namespace {
class NanoMips final : public TargetInfo {
public:
    NanoMips();
    
    RelExpr getRelExpr(RelType type, const Symbol &s,
                     const uint8_t *loc) const override;

    void relocate(uint8_t *loc, const Relocation &rel,
                uint64_t val) const override;

};
} // namespace

NanoMips::NanoMips(){
  copyRel = R_NANOMIPS_COPY;
  // noneRel Already zero, and is now static constexpr
  // noneRel = R_NANOMIPS_NONE;
  defaultMaxPageSize = 65536;
  pltEntrySize = 0;
}

//used for: R_NANOMIPS_HI20, R_NANOMIPS_PC_HI20 and R_NANOMIPS_GPREL_HI20
static void writeValueHi20(uint8_t *loc, uint64_t val) {
  uint32_t instr = read32(loc);
  instr = bswap(instr);
  uint32_t data = (val & ~1) | ((val >> 31) & 1);
  data = (data & ~0xffc) | ((val >> 19) & 0xffc); 
  uint32_t masked = (instr & ~0x1ffffd) | (data & 0x1ffffd);
  masked = bswap(masked);
  write32(loc, masked);
}

//used for: R_NANOMIPS_PC4_S1 and R_NANOMIPS_GPREL_S2
static void writeValue16(uint8_t *loc, uint64_t val, uint8_t bitsSize,
                       uint8_t shift) {
  uint32_t instr = read16(loc);
  uint32_t mask = (0xffff >> (16 - bitsSize)) << shift;
  uint32_t data = (instr & ~mask) | (val & mask);
  write16(loc, data);
}

//used for: R_NANOMIPS_PC10_S1 and R_NANOMIPS_PC7_S1
static void writePcRel16(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
  uint16_t instr = read16(loc);
  val = (val & ~1) | ((val >> bitsSize) & 1);
  uint16_t mask = (0xffff >> (16 - bitsSize));
  uint16_t data = (instr & ~mask) | (val & mask);
  write16(loc, data);
}

//used for: R_NANOMIPS_PC25_S1, R_NANOMIPS_PC21_S1, R_NANOMIPS_PC14_S1 and R_NANOMIPS_PC11_S1
static void writePcRel32(uint8_t *loc, uint64_t val, uint8_t bitsSize) {
    uint32_t instr = read32(loc); 
    instr = bswap(instr);
    val = (val & ~1) | ((val >> bitsSize) & 1);
    uint32_t mask = (0xffffffff >> (32 - bitsSize));
    uint32_t data = (instr & ~mask) | (val & mask);
    data = bswap(data);
    write32(loc, data);
}

//used for: R_NANOMIPS_LO12, R_NANOMIPS_GPREL19_S2, R_NANOMIPS_GPREL18, R_NANOMIPS_GPREL17_S1 and R_NANOMIPS_GPREL_LO12
static void writeValue32be(uint8_t *loc, uint64_t val, uint8_t bitsSize,
                       uint8_t shift) {
    uint32_t instr = read32(loc); 
    instr = bswap(instr);
    uint32_t mask = (0xffffffff >> (32 - bitsSize)) << shift;
    uint32_t data = (instr & ~mask) | (val & mask);
    data = bswap(data);
    write32(loc, data);
}

void checkIntPcRel(uint8_t *loc, int64_t v, int n, const Relocation &rel, const Symbol &sym) {
  if ((v & 1) != 0)
    error(getErrorLocation(loc) + "\tvalue: \t" + utohexstr(v) + "\tlast bit has to be zero in all PC_REL \n");
  if(sym.isUndefWeak()) //if symbol is weak, then we don't have to check it's range
    return;
  else if (rel.type == R_NANOMIPS_PC4_S1)
    checkUInt(loc, v, n, rel); // R_NANOMIPS_PC4_S1 is unsigned 5-bit integer
  else
    checkInt(loc, v, n, rel); //we have to check if value v fits in signed n-bit integer
}

RelExpr NanoMips::getRelExpr(RelType type, const Symbol &s,
                            const uint8_t *loc) const {
  switch (type){
  case R_NANOMIPS_PC25_S1:
  case R_NANOMIPS_PC21_S1:
  case R_NANOMIPS_PC14_S1:
  case R_NANOMIPS_PC11_S1:
  case R_NANOMIPS_PC10_S1:
  case R_NANOMIPS_PC7_S1:
  case R_NANOMIPS_PC4_S1:
  case R_NANOMIPS_PC_I32:
  case R_NANOMIPS_PC32:
    return R_PC;
  case R_NANOMIPS_NEG:
    return R_NANOMIPS_NEG_COMPOSITE;
  case R_NANOMIPS_32:
  case R_NANOMIPS_UNSIGNED_8:
  case R_NANOMIPS_SIGNED_8:
  case R_NANOMIPS_UNSIGNED_16:
  case R_NANOMIPS_SIGNED_16:
  case R_NANOMIPS_LO12: 
  case R_NANOMIPS_ASHIFTR_1:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_HI20:
    return R_ABS;
  case R_NANOMIPS_PC_HI20:
    return R_NANOMIPS_PAGE_PC;
  case R_NANOMIPS_GPREL19_S2:
  case R_NANOMIPS_GPREL18:
  case R_NANOMIPS_GPREL17_S1:
  case R_NANOMIPS_GPREL_I32:
  case R_NANOMIPS_GPREL_HI20:
  case R_NANOMIPS_GPREL_LO12:
  case R_NANOMIPS_GPREL7_S2:
    return R_NANOMIPS_GPREL;
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_ALIGN:
    return R_NONE;
  default:
    error(getErrorLocation(loc) + "unknown relocation (" + Twine(type) +
          ") against symbol " + toString(s) + " loc: " + utohexstr(uint64_t(loc)) + " file name: " + toString(s.file->getName()) );
    return R_NONE;
  }                   
}

void NanoMips::relocate(uint8_t *loc, const Relocation &rel, uint64_t val) const {
  switch (rel.type) {
  case R_NANOMIPS_PC_I32:
    checkInt(loc, val - 4, 32, rel);
    write32(loc, val - 4);
    break;
  case R_NANOMIPS_PC32:
  case R_NANOMIPS_32:
  case R_NANOMIPS_I32:
  case R_NANOMIPS_GPREL_I32:
    checkInt(loc, val, 32, rel);
    write32(loc, val);
    break;
  case R_NANOMIPS_NEG:
  case R_NANOMIPS_ASHIFTR_1: 
    assert(0 && "Incorrect logic for R_NANOMIPS_NEG and R_NANOMIPS_ASHIFTR_1");
    break;
  case R_NANOMIPS_NONE:
  case R_NANOMIPS_FIXED:
  case R_NANOMIPS_ALIGN:
    break;
  case R_NANOMIPS_UNSIGNED_16:
    checkUInt(loc, val, 16, rel);
    write16(loc, val);
    break;
  case R_NANOMIPS_SIGNED_16:
    checkInt(loc, val, 16, rel);
    write16(loc, val);
    break;
  case R_NANOMIPS_HI20:
  case R_NANOMIPS_PC_HI20:
  case R_NANOMIPS_GPREL_HI20:
    checkInt(loc, val, 32, rel);
    writeValueHi20(loc, val); 
    break;
  case R_NANOMIPS_PC4_S1:{
    checkIntPcRel(loc, val - 2, 5, rel, *rel.sym);
    val = (val - 2) >> 1;
    writeValue16(loc, val, 4, 0);
    break;
  }
  case R_NANOMIPS_SIGNED_8:{
    checkInt(loc, val, 8, rel);
    write8(loc, val);
    break;
  }
  case R_NANOMIPS_UNSIGNED_8:
    checkUInt(loc, val, 8, rel);
    write8(loc, val);
    break;
  case R_NANOMIPS_GPREL7_S2:
    checkUInt(loc, val, 9, rel);
    writeValue16(loc, val, 7, 2);
    break;
  case R_NANOMIPS_PC10_S1:
    checkIntPcRel(loc, val - 2, 11, rel, *rel.sym);
    writePcRel16(loc, val - 2, 10);
    break;
  case R_NANOMIPS_PC7_S1:
    checkIntPcRel(loc, val - 2, 8, rel, *rel.sym);
    writePcRel16(loc, val - 2, 7);
    break;
  case R_NANOMIPS_PC25_S1:
    checkIntPcRel(loc, val - 4, 26, rel, *rel.sym);
    writePcRel32(loc, val - 4, 25);
    break;
  case R_NANOMIPS_PC21_S1:
    checkIntPcRel(loc, val - 4, 22, rel, *rel.sym);
    writePcRel32(loc, val - 4, 21);
    break;
  case R_NANOMIPS_PC14_S1:
    checkIntPcRel(loc, val - 4, 15, rel, *rel.sym);
    writePcRel32(loc, val - 4, 14);
    break;
  case R_NANOMIPS_PC11_S1:
    checkIntPcRel(loc, val - 4, 12, rel, *rel.sym);
    writePcRel32(loc, val - 4, 11);
    break;
  case R_NANOMIPS_LO12:
    writeValue32be(loc, val, 12, 0);
    break;
  case R_NANOMIPS_GPREL19_S2:
    checkUInt(loc, val, 21, rel);
    writeValue32be(loc, val, 19, 2);
    break;
  case R_NANOMIPS_GPREL18:
    checkUInt(loc, val, 18, rel);
    writeValue32be(loc, val, 18, 0);
    break;
  case R_NANOMIPS_GPREL17_S1:
    checkUInt(loc, val, 18, rel);
    writeValue32be(loc, val, 17, 1);
    break;
  case R_NANOMIPS_GPREL_LO12:
    checkInt(loc, val, 12, rel);
    writeValue32be(loc, val, 12, 0);
    break;
  default:
    llvm_unreachable("unknown relocation");
    break;
  }
}

TargetInfo *elf::getNanoMipsTargetInfo() {
    static NanoMips t;
    return &t;
  
}
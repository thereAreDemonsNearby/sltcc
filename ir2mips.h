#ifndef SLTCC_IR2MIPS_H
#define SLTCC_IR2MIPS_H


namespace Tac
{
struct TacIR;
}

namespace mips32
{
struct AsmFile;
}

/// result : mips with virtual registers
mips32::AsmFile ir2mips32(Tac::TacIR const&);

#endif //SLTCC_IR2MIPS_H

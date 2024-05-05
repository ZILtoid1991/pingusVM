module pingus.vm;

public import pingus.types;
import pingus.etc;
import std.math;

class PingusVM {
	struct Instance {
		ulong[]		stack;
		ulong[16]	registers;
		uint		instanceNum;
		uint[]		posContext;
		uint[]		fileContext;
	}
	PingusBinFile[]	files;
	FunctionEntry[]	userFuncs;
	MetaTable[]		userMetatables;

	private int decodeNextInstr(ref Instance inst) {
		/// Reads stack/register based on the number.
		ulong readStReg(int num) {
			if (num <= 15) return inst.registers[num];
			else return inst.stack[limit(cast(int)inst.stack.length - (num - 16), 0, cast(int)inst.stack.length)];
		}
		void shiftIn(ref ulong target, ulong val) {
			target = (target<<1) | (val & 1);
		}
		VMInstruction cmd = VMInstruction(files[inst.fileContext[$-1]].bin[inst.posContext[$-1]]);
		inst.posContext[$-1]++;
		if (cmd.isRegInstr) {
			ulong rA = readStReg(cmd.rA), rB = readStReg(cmd.rB);
			switch (cmd.getRegInstNum) {
			case RegInstr.MOV:
				inst.registers[cmd.rD] = rA;
				break;
			case RegInstr.ADD:
				inst.registers[cmd.rD] = cast(long)rA + cast(long)rB;
				break;
			case RegInstr.SUB:
				inst.registers[cmd.rD] = cast(long)rA - cast(long)rB;
				break;
			case RegInstr.MUL:
				inst.registers[cmd.rD] = cast(long)rA * cast(long)rB;
				break;
			case RegInstr.DIV:
				inst.registers[cmd.rD] = cast(long)rA / cast(long)rB;
				break;
			case RegInstr.ADDU:
				inst.registers[cmd.rD] = rA + rB;
				break;
			case RegInstr.SUBU:
				inst.registers[cmd.rD] = rA - rB;
				break;
			case RegInstr.MULU:
				inst.registers[cmd.rD] = rA * rB;
				break;
			case RegInstr.DIVU:
				inst.registers[cmd.rD] = rA / rB;
				break;
			case RegInstr.MOD:
				inst.registers[cmd.rD] = cast(long)rA % cast(long)rB;
				break;
			case RegInstr.SHL:
				inst.registers[cmd.rD] = rA << rB;
				break;
			case RegInstr.SHLI:
				inst.registers[cmd.rD] = rA << cmd.rB;
				break;
			case RegInstr.SHR:
				inst.registers[cmd.rD] = rA >> rB;
				break;
			case RegInstr.SHRI:
				inst.registers[cmd.rD] = rA >> cmd.rB;
				break;
			case RegInstr.SHA:
				inst.registers[cmd.rD] = cast(long)rA >> cast(long)rB;
				break;
			case RegInstr.SHAI:
				inst.registers[cmd.rD] = cast(long)rA >> cast(long)cmd.rB;
				break;
			case RegInstr.AND:
				inst.registers[cmd.rD] = rA & rB;
				break;
			case RegInstr.OR:
				inst.registers[cmd.rD] = rA | rB;
				break;
			case RegInstr.XOR:
				inst.registers[cmd.rD] = rA ^ rB;
				break;
			case RegInstr.NOT:
				inst.registers[cmd.rD] = ~rA;
				break;
			case RegInstr.MAX:
				inst.registers[cmd.rD] = max(rA, rB);
				break;
			case RegInstr.MIN:
				inst.registers[cmd.rD] = min(rA, rB);
				break;
			case RegInstr.LIMIT:
				inst.registers[cmd.rD] = limit(cmd.rD, rA, rB);
				break;
			case RegInstr.POWI:
				inst.registers[cmd.rD] = pow(rA, rB);
				break;
			case RegInstr.MAC:
				inst.registers[cmd.rD] += rA * rB;
				break;
			case RegInstr.MNC:
				inst.registers[cmd.rD] -= rA * rB;
				break;
			case RegInstr.FADD:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) + getFloat(rB));
				break;
			case RegInstr.FSUB:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) - getFloat(rB));
				break;
			case RegInstr.FMUL:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) * getFloat(rB));
				break;
			case RegInstr.FDIV:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) / getFloat(rB));
				break;
			case RegInstr.FMOD:
				double fA = getFloat(rA), fB = getFloat(rB);
				double fD = fmod(fA, fB);
				inst.registers[cmd.rD] = forceFloat(fD);
				break;
			case RegInstr.FIADD:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) + rB);
				break;
			case RegInstr.FISUB:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) - rB);
				break;
			case RegInstr.FIMUL:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) * rB);
				break;
			case RegInstr.FIDIV:
				inst.registers[cmd.rD] = forceFloat(getFloat(rA) / rB);
				break;
			case RegInstr.IFDIV:
				inst.registers[cmd.rD] = forceFloat(rA / getFloat(rB));
				break;
			case RegInstr.IFSUB:
				inst.registers[cmd.rD] = forceFloat(rA - getFloat(rB));
				break;
			case RegInstr.FMAC:
				inst.registers[cmd.rD] = forceFloat(getFloat(cmd.rD) + (getFloat(rA) * getFloat(rB)));
				break;
			case RegInstr.FMNC:
				inst.registers[cmd.rD] = forceFloat(getFloat(cmd.rD) - (getFloat(rA) * getFloat(rB)));
				break;
			case RegInstr.ROOT:
				inst.registers[cmd.rD] = forceFloat(pow(getFloat(rA), 1 / getFloat(rB)));
				break;
			case RegInstr.POW:
				inst.registers[cmd.rD] = forceFloat(pow(getFloat(rA), getFloat(rB)));
				break;
			case RegInstr.LOG:
				inst.registers[cmd.rD] = forceFloat(log(getFloat(rA)));
				break;
			case RegInstr.CVIF:
				inst.registers[cmd.rD] = forceFloat(cast(double)rA);
				break;
			case RegInstr.CVRFI:
				inst.registers[cmd.rD] = cast(long)round(getFloat(rA));
				break;
			case RegInstr.ROUND:
				inst.registers[cmd.rD] = forceFloat(cast(double)round(getFloat(rA)));
				break;
			case RegInstr.CVTFI:
				inst.registers[cmd.rD] = cast(long)trunc(getFloat(rA));
				break;
			case RegInstr.TRNC:
				inst.registers[cmd.rD] = forceFloat(cast(double)trunc(getFloat(rA)));
				break;
			case RegInstr.RNDWB:
				double fA = getFloat(rA), fB = getFloat(rB);
				double fC = trunc(fA);
				double fM = fA - fC;
				inst.registers[cmd.rD] = forceFloat(cast(double)(fC >= fB ? fA + 1 : fA));
				break;
			case RegInstr.FMAX:
				inst.registers[cmd.rD] = forceFloat(max(getFloat(rA), getFloat(rB)));
				break;
			case RegInstr.FMIN:
				inst.registers[cmd.rD] = forceFloat(min(getFloat(rA), getFloat(rB)));
				break;
			case RegInstr.FLMT:
				inst.registers[cmd.rD] = forceFloat(limit(getFloat(cmd.rD), getFloat(rA), getFloat(rB)));
				break;
			case RegInstr.SIN:
				inst.registers[cmd.rD] = forceFloat(sin(getFloat(rA)));
				break;
			case RegInstr.COS:
				inst.registers[cmd.rD] = forceFloat(cos(getFloat(rA)));
				break;
			case RegInstr.TAN:
				inst.registers[cmd.rD] = forceFloat(tan(getFloat(rA)));
				break;
			case RegInstr.COT:
				//i.registers[instr.rD] = forceFloat(ct(getFloat(rA)));
				break;
			case RegInstr.CMPEQ:
				shiftIn(inst.registers[cmd.rD], rA == rB ? 1U : 0U);
				break;
			case RegInstr.CMPGT:
				shiftIn(inst.registers[cmd.rD], rA > rB ? 1U : 0U);
				break;
			case RegInstr.CMPGE:
				shiftIn(inst.registers[cmd.rD], rA >= rB ? 1U : 0U);
				break;
			case RegInstr.CMPNE:
				shiftIn(inst.registers[cmd.rD], rA != rB ? 1U : 0U);
				break;
			case RegInstr.FCMEQ:
				shiftIn(inst.registers[cmd.rD], getFloat(rA) == getFloat(rB) ? 1U : 0U);
				break;
			case RegInstr.FCMGT:
				shiftIn(inst.registers[cmd.rD], getFloat(rA) > getFloat(rB) ? 1U : 0U);
				break;
			case RegInstr.FCMGE:
				shiftIn(inst.registers[cmd.rD], getFloat(rA) >= getFloat(rB) ? 1U : 0U);
				break;
			case RegInstr.FCMNE:
				shiftIn(inst.registers[cmd.rD], getFloat(rA) != getFloat(rB) ? 1U : 0U);
				break;
			case RegInstr.FCMCL:
				double fA = getFloat(rA), fB = getFloat(rB);
				double fD = getFloat(inst.registers[cmd.rD]);
				if (fA + fD > fB && fA - fD < fB) inst.registers[cmd.rD] = 1;
				else inst.registers[cmd.rD] = 0;
				break;
			case RegInstr.ISNAN:
				shiftIn(inst.registers[cmd.rD], isNaN(getFloat(rA)) ? 1U : 0U);
				break;
			default:
				break;
			}
		} else {
			switch (cmd.bytes[0]) {
			case InstrList.PUSH:
				if (cmd.stack_isDirectData) {
					if (cmd.stack_is32BitData) {
						for (long i = cmd.stack_getAmount ; i >= 0 ; i--) {
							inst.stack ~= files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
							inst.posContext[$-1]++;
						}
					} else {
						for (long i = cmd.stack_getAmount ; i >= 0 ; i--) {
							inst.stack ~= files[inst.fileContext[$-1]].bin[inst.posContext[$-1]] | 
									(cast(long)(files[inst.fileContext[$-1]].bin[inst.posContext[$-1] + 1]) << 32L);
							inst.posContext[$-1]+=2;
						}
					}
				} else {
					inst.stack ~= inst.registers[cmd.rR];
				}
				break;
			case InstrList.POP:
				if (cmd.stack_isDirectData) {
					if (inst.stack.length > cmd.stack_getAmount) inst.stack.length -= cmd.stack_getAmount;
					else return -1;
				} else {
					if (inst.stack.length) inst.registers[cmd.rR] = inst.stack[$-1];
					else return -1;
				}
				break;
			case InstrList.PEEK:
				uint pos;
				if (cmd.stack_getType) pos = cast(uint)readStReg(cmd.rA);
				else if (cmd.stack_is32BitData) pos = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
				else pos = cmd.stack_getAmount;

				if (pos >= inst.stack.length) return -1;
				ulong data = inst.stack[$-1-pos];
				if (cmd.stack_isDirectData) inst.stack ~= data;
				else inst.registers[cmd.rR] = data;
				
				break;
			case InstrList.POKE:
				uint pos = cmd.stack_getAmount;
				if (pos >= inst.stack.length) return -1;
				if (cmd.stack_isDirectData) {
					if (cmd.stack_is32BitData) {
						inst.stack[$-1-pos] = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
						inst.posContext[$-1]++;
					} else {
						inst.stack[$-1-pos] = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]] | 
								(cast(long)(files[inst.fileContext[$-1]].bin[inst.posContext[$-1] + 1]) << 32L);
						inst.posContext[$-1]+=2;
					}
				} else {
					inst.stack[$-1-pos] = inst.registers[cmd.rR];
				}
				break;
			case InstrList.HEAP:
				break;
			case InstrList.JMP:
				ulong rA = readStReg(cmd.rA), rB = readStReg(cmd.rB);
				uint newPos = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
				inst.posContext[$-1]++;
				if (cmd.getJmpMode) { 
					rB = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]] | 
							(cast(long)(files[inst.fileContext[$-1]].bin[inst.posContext[$-1] + 1]) << 32L);
					inst.posContext[$-1]+=2;
				} 
				switch (cmd.condCode) {
				case JmpCode.EQ:
					if (rA == rB) goto default;
					break;
				case JmpCode.GE:
					if (rA >= rB) goto default;
					break;
				case JmpCode.GT:
					if (rA == rB) goto default;
					break;
				case JmpCode.LE:
					if (rA <= rB) goto default;
					break;
				case JmpCode.LT:
					if (rA <= rB) goto default;
					break;
				case JmpCode.SE:
					if ((rA | rB) != 1) goto default;
					break;
				case JmpCode.NE:
					if (rA != rB) goto default;
					break;
				case JmpCode.NH:
					if ((rA ^ rB) != 0) goto default;
					break;
				default:
					inst.posContext[$-1] = newPos;
					break;
				}
				break;
			case InstrList.CALL:
				//function search convetion: first try the current file, if fails try all other files, then try user functions
				uint pkgHash, mtHash, fnHash, ccHash;
				if (cmd.hasPkgHash) {
					pkgHash = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
					inst.posContext[$-1]++;
				}
				if (cmd.isMetatableFunc) {
					mtHash = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
					inst.posContext[$-1]++;
				}
				fnHash = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
				inst.posContext[$-1]++;
				if (!cmd.isVarargFunc) {
					ccHash = files[inst.fileContext[$-1]].bin[inst.posContext[$-1]];
					inst.posContext[$-1]++;
				}
				if (cmd.isMetatableFunc) {
					if (files[inst.fileContext[$-1]].header.nameHash == pkgHash || !cmd.hasPkgHash) {
						foreach (MetaTable mt ; files[inst.fileContext[$-1]].metatables) {
							if (mt.nameHash == mtHash) {
								foreach (FunctionEntry fe ; mt.entries) {
									if (fe.callConvHash == ccHash && fe.nameHash == fnHash) {
										inst.fileContext ~= inst.fileContext[$-1];
										inst.posContext ~= fe.entryPos;
										return 0;
									}
								}
								return -4;
							}
						}
					}
					foreach (size_t fileContext, PingusBinFile pbf ; files) {
						if (fileContext != inst.fileContext[$-1] && (pbf.header.nameHash == pkgHash || !cmd.hasPkgHash)) {
							foreach (MetaTable mt ; pbf.metatables) {
								if (mt.nameHash == mtHash) {
									foreach (FunctionEntry fe ; mt.entries) {
										if (fe.callConvHash == ccHash && fe.nameHash == fnHash) {
											inst.fileContext ~= cast(uint)fileContext;
											inst.posContext ~= fe.entryPos;
											return 0;
										}
									}
									return -4;
								}
							}
						}
					}
					foreach (MetaTable mt ; userMetatables) {
						if (mt.nameHash == mtHash) {
							foreach (FunctionEntry fe ; mt.entries) {
								if (fe.callConvHash == ccHash && fe.nameHash == fnHash && (fe.pkgHash == pkgHash || !cmd.hasPkgHash)) {
									ulong[] retVals = fe.hostDeleg(Var(&inst.stack[$-2-fe.nOfStackArgs]), 
											inst.registers, inst.stack[$-fe.nOfStackArgs-1..$]);
									inst.stack.length = inst.stack.length - fe.nOfStackArgs;
									inst.stack ~= retVals;
									return 0;
								}
							}
							return -4;
						}	
					}
					
				} else {
					if (files[inst.fileContext[$-1]].header.nameHash == pkgHash || !cmd.hasPkgHash) {
						foreach (FunctionEntry fe ; files[inst.fileContext[$-1]].functions) {
							if (fe.callConvHash == ccHash && fe.nameHash == fnHash) {
								inst.fileContext ~= inst.fileContext[$-1];
								inst.posContext ~= fe.entryPos;
								return 0;
							}
						}
					}
					foreach (size_t fileContext, PingusBinFile pbf ; files) {
						if (fileContext != inst.fileContext[$-1] && (pbf.header.nameHash == pkgHash || !cmd.hasPkgHash)) {
							foreach (FunctionEntry fe ; pbf.functions) {
								if (fe.callConvHash == ccHash && fe.nameHash == fnHash) {
									inst.fileContext ~= cast(uint)fileContext;
									inst.posContext ~= fe.entryPos;
									return 0;
								}
							}
						}
					}
					foreach (FunctionEntry fe ; userFuncs) {
						if (fe.callConvHash == ccHash && fe.nameHash == fnHash && (fe.pkgHash == pkgHash || !cmd.hasPkgHash)) {
							ulong[] retVals = fe.hostFunc(inst.registers, inst.stack[$-fe.nOfStackArgs-1..$]);
							inst.stack.length = inst.stack.length - fe.nOfStackArgs;
							inst.stack ~= retVals;
							return 0;
						}
					}	
				}
				return -3;
				//break;
			case InstrList.RET:
				inst.posContext.length--;
				inst.fileContext.length--;
				break;
			case InstrList.INT:
				break;
			case InstrList.INTREG:
				break;
			case InstrList.INTCLR:
				break;
			default:
				break;
			}
		}
		return 0;
	}
}
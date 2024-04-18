module pingus.types;

import std.algorithm.searching : countUntil;
import std.typecons : BitFlags;
import pingus.hash;
import bitleveld.reinterpret;

version (PINGUS_UTF32) {
	alias PCHR = dchar;
	alias PSTR = dstring;
} else {
	alias PCHR = char;
	alias PSTR = string;
}

///Defines potential variable types.
enum VarTypeID : ubyte {
	nulltype		=	0x00,		///Null type/nil/not associated
	signedInt		=	0x01,
	unsignedInt		=	0x02,
	floatingPoint	=	0x03,
	sBlob			=	0x04,		///Binary blob of small size (64 bits or smaller)
	blob			=	0x05,		///Binary blob, arbitrary size
	str				=	0x06,
	hashmap			=	0x07,
	array			=	0x08,
	intarray		=	0x09,
	uintarray		=	0x0a,
	floatarray		=	0x0b,
	strarray		=	0x0c,
	blobarray		=	0x0d,
	bitarray		=	0x0e,
	reftype			=	0x0f,
	func			=	0x10,
	fiber			=	0x11,
	deleg			=	0x12,
	delegfiber		=	0x13,
}

enum VarFlags_Protect : ubyte {
	Protect			=	1 << 0,
	Constant		=	1 << 1,
}

enum RegInstr : ushort {
	MOV				=	0X000,		///Move
	ADD				=	0X001,		///Signed integer add
	SUB				=	0X002,		///Signed integer subtract
	MUL				=	0X003,		///Signed integer multiply
	DIV				=	0X004,		///Signed integer division
	ADDU			=	0X005,		///Unsigned integer add
	SUBU			=	0X006,		///Unsigned integer subtract
	MULU			=	0X007,		///Unsigned integer multiply
	DIVU			=	0X008,		///Unsigned integer division
	MOD				=	0X009,		///Singed modulo
	SHL				=	0X00A,		///Logical shift left by register value
	SHLI			=	0X00B,		///Logical shift left by immediate value
	SHR				=	0X00C,		///Logical shift right by register value
	SHRI			=	0X00D,		///Logical shift right by immediate value
	SHA				=	0X00E,		///Arithmetic shift right by register value
	SHAI			=	0X00F,		///Arithmetic shift right by immediate value
	AND				=	0X010,		///Logical AND
	OR				=	0X011,		///Logical OR
	XOR				=	0X012,		///Logical XOR
	NOT				=	0X013,		///Logical NOT
	MAX				=	0X014,		///Move the greater value to D(integer)
	MIN				=	0X015,		///Move the lesser value to D(integer)
	LIMIT			=	0X016,		///Limit D's value between A and B(integer)
	POWI			=	0X017,		///Integer power
	MAC				=	0X020,		///Multiply-accumulate(integer)
	MNC				=	0X021,		///Multiply-subtract(integer)
	FADD			=	0X030,		///Floating-point add
	FSUB			=	0X031,		///Floating-point subtract
	FMUL			=	0X032,		///Floating-point multiply
	FDIV			=	0X033,		///Floating-point division
	FMOD			=	0X034,		///Floating-point modulo
	FIADD			=	0X035,		///Add integer to floating-pont
	FISUB			=	0X036,		///Subtract integer from floating-pont
	FIMUL			=	0X037,		///Multiply integer with floating-pont
	FIDIV			=	0X038,		///Divide floating-pont by integer
	IFDIV			=	0X039,		///Divide integer by floating-point
	IFSUB			=	0X03A,		///Subtract floating-point from integer
	FMAC			=	0X03B,		///Multiply-accumulate(floating point)
	FMNC			=	0X03C,		///Multiply-subtract(floating point)
	ROOT			=	0X03D,		///Return the Bth root of A(FP)
	POW				=	0X03E,		///Return the Bth exponent of A(FP)
	LOG				=	0X03F,		///Return the B-based logarithm of A(FP)
	CVIF			=	0X040,		///Convert A integer into a floating point number
	CVRFI			=	0X041,		///Convert A into integer by rounding
	ROUND			=	0X042,		///Round A
	CVTFI			=	0X043,		///Convert A into integer by truncating
	TRNC			=	0X044,		///Truncate A
	RNDWB			=	0X045,		///Round A with B as point of reference
	FMAX			=	0X046,		///Move the greater value to D(floating-pont)
	FMIN			=	0X047,		///Move the lesser value to D(floating-pint)
	FLMT			=	0X048,		///Limit D's value between A and B(FP)
	SIN				=	0X049,		///Returns the sine of A
	COS				=	0X04A,		///Returns the cosine of A
	TAN				=	0X04B,		///Returns the tangent of A
	COT				=	0X04C,		///Returns the cotangent of A
	CMPEQ			=	0X060,		///Shifts 1 into D if equal, 0 otherwise(int)
	CMPGT			=	0X061,		///Shifts 1 into D if A > B, 0 otherwise(int)
	CMPGE			=	0X062,		///Shifts 1 into D if A >= B, 0 otherwise(int)
	CMPNE			=	0X063,		///Shifts 1 into D if not equal, 0 otherwise(int)
	FCMEQ			=	0X064,		///Shifts 1 into D if equal, 0 otherwise(FP)
	FCMGT			=	0X065,		///Shifts 1 into D if A > B, 0 otherwise(FP)
	FCMGE			=	0X066,		///Shifts 1 into D if A >= B, 0 otherwise(FP)
	FCMNE			=	0X067,		///Shifts 1 into D if not equal, 0 otherwise(FP)
	FCMCL			=	0X068,		///Sets D to 1 if A and B are within D(FP)
	ISNAN			=	0X069,		///Shifts 1 into D if A is NaN
}

enum InstrList : ubyte {
	NONE			=	0x00,
	PUSH			=	0x01,
	POP				=	0x02,
	PEEK			=	0x03,
	POKE			=	0x04,
	HEAP			=	0x05,
	JMP				=	0x06,
	CALL			=	0x08,
	RET				=	0x09,
	INT				=	0x0A,
	INTREG			=	0x0B,
	INTCLR			=	0x0C,
}

enum HeapInstr : ubyte {
	CRT				=	0x00,		///Creates a var, then pushes it to the stack
	DSTR			=	0x01,		///Destroys a var's reference, and sets it to all zeros
	RESIZE			=	0x02,		///Resizes a var if it's an array, etc.
	READ			=	0x03,		///Reads index A from the selected array into B
	WRITE			=	0x04,		///Writes B into selected array at index A
	CRTFR			=	0x05,		///Create var from register
	CHKTP			=	0x06,		///Returns the type ID of the given var into A
	READD			=	0x07,		///Reads locally stored data into B
	READB			=	0x08,		///Byte read from binary blob/array (unaligned)
	READHW			=	0x09,		///Halfword read from binary blob/array (unaligned)
	READW			=	0x0A,		///Word read from binary blob/array (unaligned)
	READDW			=	0x0B,		///Doubleword read from binary blob/array (unaligned)
	WRTB			=	0x0C,		///Byte write to binary blob/array (unaligned)
	WRTHW			=	0x0D,		///Halfword write to binary blob/array (unaligned)
	WRTW			=	0x0E,		///Word write to binary blob/array (unaligned)
	WRTDW			=	0x0F,		///Doubleword write to binary blob/array (unaligned)
	SLICE			=	0x10,		///Creates a slice of an array between indexes a and B, puts new array on top of the stack
	VADD			=	0x11,		///Adds variant A and B, stores in D
	VSUB			=	0x12,		///Subtracts variant B from A, stores in D
	VMUL			=	0x13,		///Multiplies variant A and B, stores in D
	VDIV			=	0x14,		///Divides variant A by B, stores in D
	VMOD			=	0x15,		///Stores the modulo of A by B in D
	VLSH			=	0x16,		///Left shifts variant A by B, stores in D
	VRSH			=	0x17,		///Right shifts variant A by B, stores in D
	VRASH			=	0x18,		///Right arithmetic shifts variant A by B stores in D
	VAND			=	0x19,		///Logically ands A and B, stores in D
	VOR				=	0x1A,		///Logically ors A and B, stores in D
	VXOR			=	0x1B,		///Logically xors A and B, stores in D
	VNOT			=	0x1C,		///Logically nots A, stores in D
	VCMP			=	0x1D,		///Compares A and B for C, stores in register D
}
/**
 * Defines a single entry in the heap.
 */
struct HeapEntry {
	uint			id;			///Identifier of the heap entry
	uint			refcount;	///Reference counter
	ubyte[]			data;		///Data held by the heap
}

/** 
 * Implements Variable types, also handles heap management and metatables.
 */
struct Var {
	static MetaTable[] metaTables;	///Associated metatables
	static HeapEntry[] heap;		///Heap management table
	static uint		heapCnt;		///Heap counter
	ubyte			typeID;			///Denotes type
	BitFlags!VarFlags_Protect	flags;///Protection flags
	ubyte 			pad2;			///Currently unused, it's there for 32 bit padding
	ubyte			pad3;			///Currently unused, it's there for 32 bit padding
	uint			metatableRef;	///Selects associated metatable if there's any.
	union {
		double		valF;			///Holds floating-point value
		long		valI;			///Holds integer value
		ulong		valU;			///Holds unsigned integer/small blob value
		uint[2]		valR;			///Holds reference value (reference to object/function selection)
	}
	this(HostFunc f, string funcName, string callConv, bool varArg, bool noReturn) {
		FunctionEntry fe = FunctionEntry(xxhash32(cast(ubyte[])funcName), xxhash32(cast(ubyte[])callConv), f, varArg, noReturn);
		valR = createHeapEntry(reinterpretAsArray!ubyte(fe));
		typeID = VarTypeID.func;
		//metatableRef = xxhash32(cast(ubyte[])T.stringof);
	}
	this(HostDeleg d, string funcName, string callConv, bool varArg, bool noReturn) {
		FunctionEntry fe = FunctionEntry(xxhash32(cast(ubyte[])funcName), xxhash32(cast(ubyte[])callConv), d, varArg, noReturn);
		valR = createHeapEntry(reinterpretAsArray!ubyte(fe));
		typeID = VarTypeID.deleg;
		//metatableRef = xxhash32(cast(ubyte[])T.stringof);
	}
	this(T)(T val) {
		static if (is(T == ulong) || is(T == uint) || is(T == ushort) || is(T == ubyte)) {
			valU = val;
			typeID = VarTypeID.unsignedInt;
			metatableRef = xxhash32(cast(ubyte[])"UINT");
		} else static if (is(T == long) || is(T == int) || is(T == short) || is(T == byte)) {
			valI = val;
			typeID = VarTypeID.signedInt;
			metatableRef = xxhash32(cast(ubyte[])"INT");
		} else static if (is(T == double) || is(T == float) || is(T == real)){
			valF = val;
			typeID = VarTypeID.floatingPoint;
			metatableRef = xxhash32(cast(ubyte[])"FLOAT");
		} else static if (is(T == PSTR)) {
			valR = createHeapEntry(reinterpretCast!ubyte(val));
			typeID = VarTypeID.str;
			metatableRef = xxhash32(cast(ubyte[])"STR");
		} else static if (T.sizeof <= 8) {
			valU = *cast(ulong*)(cast(void*)&val);
			typeID = VarTypeID.sBlob;
			metatableRef = xxhash32(cast(ubyte[])T.stringof);
		} else {
			valR = createHeapEntry(reinterpretAsArray!ubyte(val));
			typeID = VarTypeID.blob;
			metatableRef = xxhash32(cast(ubyte[])T.stringof);
		}
	}
	/// Creates an entry on the on the heav for the supplied data, returns the ID.
	package static uint createHeapEntry(ubyte[] variable) {
		foreach (ref HeapEntry h ; heap) {
			if (!h.refcount) {
				h.data = variable;
				h.refcount++;
			}
		}
		heapCnt++;
		heap ~= HeapEntry(heapCnt, 1, variable);
		return heapCnt;
	}
	/**
	 * Shrinks the heap by removing any dereferenced types.
	 */
	package static void shrink() {
		HeapEntry[] newHeap;
		foreach (HeapEntry key; heap) {
			if (key.refcount) newHeap ~= key;
		}
		heap = newHeap;
	}
	///Dereference any array or similar heap allocated things
	~this() {
		if (typeID >= VarTypeID.hashmap && typeID <= VarTypeID.reftype) {
			foreach (ref HeapEntry h ; heap) {
				if (valR[0] == h.id) {
					h.refcount--;
					if (!h.refcount) {
						h.data.length = 0;
					}
				}
			}
		}
	}
	///Tries to cast the content of the Var to the requested type.
	T opCast(T)() const {
		import std.math : nearbyint;
		static if (is(T == ulong) || is(T == uint) || is(T == ushort) || is(T == ubyte)) {
			switch (typeID) {
				case VarTypeID.signedInt:
					return cast(T)valI;
				case VarTypeID.unsignedInt:
					return cast(T)valU;
				case VarTypeID.floatingPoint:
					return cast(T)nearbyint(valF);
				default:
					throw new VarException("Cannot be implicitly converted to selected type!");
			}
		} else static if (is(T == long) || is(T == int) || is(T == short) || is(T == byte)) {
			switch (typeID) {
				case VarTypeID.signedInt:
					return cast(T)valI;
				case VarTypeID.unsignedInt:
					return cast(T)valU;
				case VarTypeID.floatingPoint:
					return cast(T)nearbyint(valF);
				default:
					throw new VarException("Cannot be implicitly converted to selected type!");
			}
		} else static if (is(T == real) || is(T == double) || is(T == float)) {
			switch (typeID) {
				case VarTypeID.signedInt:
					return valI;
				case VarTypeID.unsignedInt:
					return valU;
				case VarTypeID.floatingPoint:
					return valF;
				default:
					throw new VarException("Cannot be implicitly converted to selected type!");
			}
		}
		//else static assert(0, "Casting for type is not supported");
	}
	Var opBinary(string op)(const Var rhs) const {
		switch (this.typeID) {
			case VarTypeID.unsignedInt:
				switch (rhs.typeID) {
					case VarTypeID.unsignedInt:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valU " ~ op ~ "rhs.valU)");
						} else {
							break;
						}
					case VarTypeID.signedInt:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valU " ~ op ~ "rhs.valI)");
						} else {
							break;
						}
					case VarTypeID.floatingPoint:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valU " ~ op ~ "rhs.valF)");
						} else {
							break;
						}
					default: break;
				}
				break;
			case VarTypeID.signedInt:
				switch (rhs.typeID) {
					case VarTypeID.unsignedInt:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valI " ~ op ~ "rhs.valU)");
						} else {
							break;
						}
					case VarTypeID.signedInt:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valI " ~ op ~ "rhs.valI)");
						} else {
							break;
						}
					case VarTypeID.floatingPoint:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valI " ~ op ~ "rhs.valF)");
						} else {
							break;
						}
					default: break;
				}
				break;
			case VarTypeID.floatingPoint:
				switch (rhs.typeID) {
					case VarTypeID.unsignedInt:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valF " ~ op ~ "rhs.valU)");
						} else {
							break;
						}
					case VarTypeID.signedInt:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valF " ~ op ~ "rhs.valI)");
						} else {
							break;
						}
					case VarTypeID.floatingPoint:
						static if (countUntil(op, "+", "-", "*", "/") != -1) {
							mixin("return Var(this.valF " ~ op ~ "rhs.valF)");
						} else {
							break;
						}
					default: break;
				}
				break;
			default: break;
		}
		throw new VarException("Inpomatible types!");
		
	}
}

enum FunctionEntryFlags : ubyte {
	isConst			=	1 << 0,		///Function guaranteed to not modify globals and/or associated binary blob
	noReturn		=	1 << 1,		///Function does not return anything (can become a fiber)
	varArg			=	1 << 2,		///Arbitrary amount of arguments (calling convention hashing won't be done)
	memberFunc		=	1 << 3,		///Is a member function (delegate)
	ext				=	1 << 4,		///External function (paired from host)
	exp				=	1 << 5,		///Export function (visible to host)
}

alias HostFunc = Var[] function(ref ulong[16], Var[]);
alias HostDeleg = Var[] function(Var, ref ulong[16], Var[]);

struct FunctionEntry {
	uint			nameHash;		///XXHash32 of the name
	uint			callConvHash;	///XXHash32 of the calling convention
	uint			entryPos;		///Function entry point
	ushort			fileNumL;		///File number identifier (lower two bytes)
	ubyte			fileNumH;		///File number identifier (upper one byte)
	BitFlags!FunctionEntryFlags	flags;
	union {
		HostFunc	hostFunc;
		HostDeleg	hostDeleg;
	}
	this (uint nameHash, uint callConvHash, HostFunc hostFunc, bool varArg, bool noReturn) {
		this.nameHash = nameHash;
		this.callConvHash = callConvHash;
		this.hostFunc = hostFunc;
		flags.ext = true;
		flags.varArg = varArg;
		flags.noReturn = noReturn;
	}
	this (uint nameHash, uint callConvHash, HostDeleg hostDeleg, bool varArg, bool noReturn) {
		this.nameHash = nameHash;
		this.callConvHash = callConvHash;
		this.hostDeleg = hostDeleg;
		flags.ext = true;
		flags.varArg = varArg;
		flags.noReturn = noReturn;
	}
	bool opEquals(const FunctionEntry other) const @nogc @safe pure nothrow {
		return this.nameHash == other.nameHash && this.callConvHash == other.callConvHash;
	}
	size_t toHash() const @nogc @safe pure nothrow {
		static if (size_t.sizeof == 8)
			return nameHash | cast(ulong)callConvHash<<32L;
		else
			return nameHash ^ (callConvHash<<17) ^ (callConvHash>>15);
	}
}

struct MetaTable {
	uint			nameHash;
	uint			blobSize;
	FunctionEntry[]	entries;
}

struct HashMap {
	struct HashMapEntry {
		ulong[2]	key;
		Var			value;
	}
	uint			id;
	private HashMapEntry[] entries;
}

struct VMInstruction {
	union {
		uint		base;
		ubyte[4]	bytes;
	}
	this (uint base) {
		this.base = base;
	}
	public bool isRegInstr() @nogc @safe pure nothrow const {
		return (bytes[0] & 1);
	}
	public uint getRegInstNum() @nogc @safe pure nothrow const {
		return (bytes[0]>>1) | ((bytes[1] & 0xF0)<<3);
	}
	public ubyte rD() @nogc @safe pure nothrow const {
		return bytes[1] & 0x0F;
	}
	public ubyte rA() @nogc @safe pure nothrow const {
		return bytes[2];
	}
	public ubyte rB() @nogc @safe pure nothrow const {
		return bytes[3];
	}
	public uint getMiscInstNum() @nogc @safe pure nothrow const {
		return bytes[0]>>1;
	}
	public ubyte rR() @nogc @safe pure nothrow const {
		return bytes[1]>>4;
	}
	public bool stack_isDirectData() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x08) != 0;
	}
	public bool stack_is32BitData() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x04) != 0;
	}
	public uint stack_getAmount() @nogc @safe pure nothrow const {
		return bytes[2] | (bytes[3]<<8);
	}
	public bool isHeapInstr() @nogc @safe pure nothrow const {
		return bytes[0] == 0x05;
	}
	public ubyte getHeapInstrNum() @nogc @safe pure nothrow const {
		return bytes[1];
	}
	public uint heap_getT() @nogc @safe pure nothrow const {
		return bytes[2] | ((bytes[3] & 0x3F)<<8);
	}
	public ubyte heapT2_rA() @nogc @safe pure nothrow const {
		return bytes[2]>>4;
	}
	public ubyte heapT2_rB() @nogc @safe pure nothrow const {
		return bytes[3] & 0x0F;
	}
	alias heapT2_rC = rB;
	public bool isJMP() @nogc @safe pure nothrow const {
		return bytes[0] == 0x0B;
	}
	public ubyte condCode() @nogc @safe pure nothrow const {
		return bytes[1] & 0x3F;
	}
	public bool getJmpMode() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x40) != 0;
	}
	public bool isCall() @nogc @safe pure nothrow const {
		return bytes[0] == 0x10;
	}
	alias call_varargNum = rA;
	alias call_stackPos = rB;
	public bool fromVar() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x01);
	}
	public bool isFiberCall() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x02) != 0;
	}
	public bool isVarargFunc() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x04) != 0;
	}
	public bool isMetatableFunc() @nogc @safe pure nothrow const {
		return (bytes[1] & 0x08) != 0;
	}
	public bool isReturn() @nogc @safe pure nothrow const {
		return bytes[0] == 0x12;
	}
	public bool isInterrupt() @nogc @safe pure nothrow const {
		return bytes[0] == 0x14;
	}
	alias intr_group = getHeapInstrNum;
	alias intr_getCode = stack_getAmount;
	public bool isIntrReg() @nogc @safe pure nothrow const {
		return bytes[0] == 0x16;
	}
	public bool isIntrClr() @nogc @safe pure nothrow const {
		return bytes[0] == 0x18;
	}
	alias isSysIntr = fromVar;
}

public class PingusException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null) pure nothrow @nogc @safe {
		super(msg, file, line, nextInChain);
	}
}
public class VarException : PingusException {
	this(string msg, string file = __FILE__, size_t line = __LINE__, Throwable nextInChain = null) pure nothrow @nogc @safe {
		super(msg, file, line, nextInChain);
	}
}

module pingus.types;

import std.typecons : BitFlags;
import pingus.hash;

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

struct HeapEntry {
	uint			id;
	uint			refcount;
	ubyte[]			data;
}

/** 
 * Implements Variable types, also handles heap management and metatables.
 */
struct Var {
	static MetaTable[] metaTables;	///Associated metatables
	static HeapEntry[] heap;		
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

		} else static if (T.sizeof <= 8) {
			valU = *cast(ulong*)(cast(void*)&val);
			typeID = VarTypeID.sBlob;
			metatableRef = xxhash32(cast(ubyte[])T.stringof);
		}
	}
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
		} 
		//else static assert(0, "Casting for type is not supported");
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

alias HostFunc = Var[] function(ulong[8], Var[]);
alias HostDeleg = Var[] function(Var, ref ulong[8], Var[]);

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

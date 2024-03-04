module pingus.types;

import std.typecons : BitFlags;
import pingus.hash;

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

struct Var {
	ubyte			typeID;			///Denotes type
	BitFlags!VarFlags_Protect	flags;///Protection flags
	ubyte 			pad2;			///Currently unused, it's there for 32 bit padding
	ubyte			pad3;			///Currently unused, it's there for 32 bit padding
	uint			metatableRef;	///Selects associated metatable if there's any/selects function from metatable.
	union {
		double		valF;			///Holds floating-point value
		long		valI;			///Holds integer value
		ulong		valU;			///Holds unsigned integer/small blob value
		uint[2]		valR;			///Holds reference value
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
alias HostDeleg = Var[] function(Var, ulong[8], Var[]);

struct FunctionEntry {
	uint			nameHash;		///XXHash32 of the name
	uint			callConvHash;	///XXHash32 of the calling convention
	uint			entryPos;		
	ushort			fileNumL;		///File number identifier (lower two bytes)
	ubyte			fileNumH;		///File number identifier (upper one byte)
	BitFlags!FunctionEntryFlags	flags;
	union {
		HostFunc	hostFunc;
		HostDeleg	hostDeleg;
	}
}

struct MetaTable {
	uint			nameHash;
	uint			blobSize;
	FunctionEntry[]	entries;
}
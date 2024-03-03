module pingus.types;

import std.typecons : BitFlags;

enum VarTypeID : ubyte {
	nulltype		=	0x00,		///Null type/nil/not associated
	signedInt		=	0x01,
	unsignedInt		=	0x02,
	floatingPoint	=	0x03,
	sBlob			=	0x04,		///Binary blob of small size (64 bits or smaller)
}

enum VarFlags_Protect : ubyte {
	Protect			=	1 << 0,
	Constant		=	1 << 1,
}

struct Var {
	ubyte			typeID;			///
	BitFlags!VarFlags_Protect	flags;
	ubyte 			pad2;
	ubyte			pad3;
}
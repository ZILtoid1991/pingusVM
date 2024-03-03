module pingus.hash;

import std.bitmanip : swapEndian;

/** 
 * Calculates the 32 bit xxhash value of the given source, which is used for metatables and the likes.
 * Params:
 *   source = The data to be hashed.
 *   seed = Seed for
 * Returns: The 32 bit xxhash value.
 */
uint xxhash32(ubyte[] source, uint seed = 0) @nogc pure nothrow {
	auto srcPtr = cast(const(uint)*)source.ptr;
	auto srcEnd = cast(const(uint)*)(source.ptr + source.length);
	uint result = void;

	if (source.length >= 16) {
		auto limit = srcEnd - 4;
		uint v1 = seed + PRIME32_1 + PRIME32_2;
		uint v2 = seed + PRIME32_2;
		uint v3 = seed;
		uint v4 = seed - PRIME32_1;

		//Let's hope the compiler will be smart enough to optimize it for SSE/AVX/NEON
		for ( ; srcPtr <= limit ; srcPtr+=4) {
			v1 += srcPtr[0] * PRIME32_2; v1 = (v1<<13) | (v1>>>19); v1 *= PRIME32_1;
    		v2 += srcPtr[1] * PRIME32_2; v2 = (v2<<13) | (v2>>>19); v2 *= PRIME32_1;
    		v3 += srcPtr[2] * PRIME32_2; v3 = (v3<<13) | (v3>>>19); v3 *= PRIME32_1;
    		v4 += srcPtr[3] * PRIME32_2; v4 = (v4<<13) | (v4>>>19); v4 *= PRIME32_1;
		}

		result = ((v1<<1)|(v1>>>31)) + ((v2<<7)|(v2>>>25)) + ((v3<<12)|(v3>>>20)) + ((v4<<18)|(v4>>>14));
	} else {
		result = seed + PRIME32_5;
	}

	result += source.length;

	while (srcPtr <= srcEnd - 1) {
		result += srcPtr[0] * PRIME32_3;
		result = ((result<<17)|(result>>>15)) * PRIME32_4;
		srcPtr++;
	}

	auto ptr = cast(const(ubyte)*)srcPtr;
	auto end = cast(const(ubyte)*)srcEnd;

	while (ptr < end) {
		result += *ptr * PRIME32_5;
		result = ((result<<11)|(result>>>21)) * PRIME32_1 ;
		ptr++;
	}

	result ^= result >> 15;
	result *= PRIME32_2;
	result ^= result >> 13;
	result *= PRIME32_3;
	result ^= result >> 16;

	return result;
}

ulong[2] xxhash3_128(ubyte[] source, const ulong seed) @nogc pure nothrow {
	switch (source.length) {
		case 0:
			return xxh3_128_empty(seed, defaultSecret);
		case 1: .. case 3:
			return xxh3_128_1to3(source, seed, defaultSecret);
		case 4: .. case 8:
			return xxh3_128_4to8(source, seed, defaultSecret);
		case 9: .. case 16:
			return xxh3_128_9to16(source, seed, defaultSecret);
		case 17: .. case 128:
			return xxh3_128_17to128(source, seed, defaultSecret);
		case 129: .. case 240:
			return xxh3_128_129to240(source, seed, defaultSecret);
		default:
			return xxh3_128_large(source, seed, defaultSecret);
	}
}

package:
ulong[2] xxh3_128_empty(const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	const ulong[4] secretWords = [*cast(const(ulong)*)(secret.ptr + 64), *cast(const(ulong)*)(secret.ptr + 64 + 8), 
			*cast(const(ulong)*)(secret.ptr + 64 + 16), *cast(const(ulong)*)(secret.ptr + 64 + 24)];
	return [
		avalanche_XXH64(seed ^ secretWords[0] ^ secretWords[1]),
		avalanche_XXH64(seed ^ secretWords[2] ^ secretWords[3])
	];
}

ulong[2] xxh3_128_1to3(ubyte[] source, const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	const uint[4] secretWords = [*cast(const(uint)*)secret.ptr, *cast(const(uint)*)(secret.ptr + 4), 
			*cast(const(uint)*)(secret.ptr + 8), *cast(const(uint)*)(secret.ptr + 12)];
	const uint combined = xxh3Combine_1to3(source);
	return [
		avalanche_XXH64(((secretWords[0] ^ secretWords[1]) + seed) ^ combined),
		avalanche_XXH64(((secretWords[2] ^ secretWords[3]) - seed) ^ swapEndian(combined) << 13)
	];
}

ulong[2] xxh3_128_4to8(ubyte[] source, const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	const ulong[2] secretWords[2] = [*cast(const(ulong)*)(secret.ptr + 16), *cast(const(ulong)*)(secret.ptr + 24)];
	//const ulong combined = xxh3Combine_4to8(source);
	//const ulong value = ((secretWords[0] ^ secretWords[1]) + modifiedSeed(seed)) ^ xxh3Combine_4to8(source);
	const ulong[2] mulResult = 
			multiply128bitUS(((secretWords[0] ^ secretWords[1]) + modifiedSeed(seed)) ^ xxh3Combine_4to8(source), 
			seed + (source.length<<2));
	ulong high = mulResult[1];
	ulong low = mulResult[0];
	high += low<<1L;
	low ^= high>>3L;
	low ^= low>>35L;
	low *= PRIME_MX2;
	low ^= low>>28L;
	high = avalanche(high);
	return [low, high];
}

ulong[2] xxh3_128_9to16(ubyte[] source, const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	const ulong inputFirst = *cast(const(ulong)*)secret.ptr;
	const ulong inputLast = *cast(const(ulong)*)(secret.ptr + secret.length - 8);
	const ulong[4] secretWords = [*cast(const(ulong)*)(secret.ptr + 32), *cast(const(ulong)*)(secret.ptr + 32 + 8), 
			*cast(const(ulong)*)(secret.ptr + 32 + 16), *cast(const(ulong)*)(secret.ptr + 32 + 24)];
	const ulong val1 = ((secretWords[0] ^ secretWords[1]) - seed) ^ inputFirst ^ inputLast;
	const ulong val2 = ((secretWords[2] ^ secretWords[3]) + seed) ^ inputLast;
	const ulong[2] mulResult = multiply128bitUS(val1, PRIME64_1);
	ulong low = mulResult[0] + ((source.length - 1)<<54);
	ulong high = mulResult[1] + (val2 & 0xFFFF_FFFF_0000_0000) + (val2 & 0xFFFF_FFFF) * PRIME32_2;
	low ^= swapEndian(high);
	const ulong[2] mulResult2 = multiply128bitUS(low, PRIME64_2);
	low = mulResult2[0];
	high = mulResult2[1] + high * PRIME64_2;
	return [avalanche(low), avalanche(high)];
}

ulong[2] xxh3_128_17to128(ubyte[] source, const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	ulong[2] acc = [source.length * PRIME64_1, 0];
	const size_t numRounds = ((source.length - 1)>>5) + 1;
	for (sizediff_t i = numRounds ; i >= 0 ; i--) {
		const offsetStart = i*16;
		const offsetEnd = inputLenght - (i*16) - 16;
		const ulong[2] data1 = read128Bits(source, offsetStart);
		const ulong[2] data2 = read128Bits(source, offsetEnd);
		acc = mixTwoChunks(acc, data1, data2, i*32, seed, secret);
	}
	return [
		avalanche(acc[0] + acc[1]), 
		0L - avalanche((acc[0] * PRIME64_1) + (acc[1] * PRIME64_4) + ((source.length - seed) * PRIME64_2))
	];
}

ulong[2] xxh3_128_129to240(ubyte[] source, const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	ulong[2] acc = [source.length * PRIME64_1, 0];
	ulong numChunks = source.length >> 4;
	for (size_t i ; i < 4 ; i++) {
		acc = mixTwoChunks(acc, read128Bits(source, i*32), read128Bits(source, i*32 + 16), i*32, seed, secret);
	}
	acc[0] = avalanche(acc[0]);
	acc[1] = avalanche(acc[1]);
	for (size_t i = 4; i < numChunks ; i++) {
		acc = mixTwoChunks(acc, read128Bits(source, i*32), read128Bits(source, i*32 + 16), (i - 4)*32 + 3, seed, secret);
	}
	acc = mixTwoChunks(acc, read128Bits(source, source.length-16), read128Bits(source, source.length-32), 103, 0L - seed, 
			secret);
	return [
		avalanche(acc[0] + acc[1]), 
		0L - avalanche((acc[0] * PRIME64_1) + (acc[1] * PRIME64_4) + ((source.length - seed) * PRIME64_2))
	];
}

ulong[2] xxh3_128_large(ubyte[] source, const ulong seed, const ubyte[] secret) @nogc pure nothrow {
	ulong[8] acc = [PRIME32_3, PRIME64_1, PRIME64_2, PRIME64_3, PRIME64_4, PRIME32_2, PRIME64_5, PRIME32_1];
	const stripesPerBlock = (secret.length - 64) / 8;
	const blockSize = 64 * stripesPerBlock;
	void accumulate(ulong[8] stripe, const size_t secretOffset) @nogc pure nothrow {
		const ulong[8] secretWords = readStripe(secret, secretOffset);
		for (int i = 0; i < 8 ; i++) {
			const ulong value = stripe[i] ^ secretWords[i];
			acc[i ^ 1] += stripe[i];
			acc += (value & 0xFFFF_FFFF) * (value>>32L);
		}
	}
	void roundAccumulate(ubyte[] block) @nogc pure nothrow {
		for (int n = 0; n < stripesPerBlock ; n++) {
			ulong[8] stripe = readStripe(block, n*64);
			accumulate(stripe, n * 8);
		}
	}
	void roundScramble() @nogc pure nothrow {
		const ulong[8] secretWords = readStripe(secret, secret.length - 64);
		for (int i = 0; i < 8 ; i++) {
			acc[i] ^= acc[i]>>47;
			acc[i] ^= secretWords[i];
			acc[i] *= PRIME32_1;
		}
	}
	ulong finalMerge(ulong initValue, const size_t secretOffset) @nogc pure nothrow {
		const ulong[8] secretWords = readStripe(secret, secretOffset);
		for (i = 0 ; i < 4 ; i++) {
			ulong[2] mulResult = multiply128bitUS(acc[i*2] ^ secretWords[i*2], acc[i*2 + 1] ^ secretWords[i*2 + 1]);
			initValue += mulResult[0] ^ mulResult[1];
		}
		return avalanche(initValue);
	}
	size_t pos;
	for ( ; pos + 1 < source.length ; pos += blockSize) {
		roundAccumulate(source[pos..pos+blockSize]);
		roundScramble();
	}
	for (size_t n ; n < (source.length - pos - 1) / 64 ; n++) {
		accumulate(readStripe(source[pos..$], n * 64), n* 8);
	}
	accumulate(readStripe(source[$-64..$], 0), secret.length-71);
	return [finalMerge(source.length * PRIME64_1, 11), finalMerge(~(source.length * PRIME64_2), secret.length - 75)];
}

pragma(inline, true)
ulong[2] mixTwoChunks(ref ulong[2] acc, const ulong[2] data1, const ulong[2] data2, size_t secretOffset, ulong seed, 
		const ubyte[] secret) @nogc pure nothrow {
	acc[0] += mixStep(data1, secretOffset, seed, secret);
	acc[1] += mixStep(data2, secretOffset + 16, seed, secret);
	acc[0] ^= data2[0] + data2[1];
	acc[1] ^= data1[0] + data1[1];
	return acc;
}

pragma(inline, true)
ulong mixStep(const ulong[2] dataWords, size_t secretOffset, ulong seed, const ubyte[] secret) @nogc pure nothrow {
	const ulong[2] secretWords = read128Bits(secret, secretOffset);
	ulong[2] mulResult = multiply128bitUS(dataWords[0] ^ (secretWords[0] + seed), dataWords[1] ^ (secretWords[1] - seed));
	return mulResult[0] ^ mulResult[1];
}

pragma(inline, true)
uint xxh3Combine_1to3(ubyte[] source) @nogc @safe pure nothrow {
	uint result = cast(uint)source.length | (source[0]<<16);
	if (source >= 2) result |= source[1]<<24;
	if (source >= 3) result |= source[2];
	return result;
}

pragma(inline, true)
ulong xxh3Combine_4to8(ubyte[] source) @nogc @safe pure nothrow {
	return source[0] | (source[1]<<8L) | (source[2]<<16L) | (source[3]<<24L) | (source[$-4]<<32L) | (source[$-3]<<40L)
			| (source[$-2]<<48L) | (source[$-1]<<56L);
}

pragma(inline, true)
ulong modifiedSeed(const ulong seed) @nogc @safe pure nothrow {
	return seed ^ (cast(ulong)swapEndian(cast(uint)seed)<<32);
}

pragma(inline, true)
ulong[2] read128Bits(ubyte[] source, size_t offset) @nogc pure nothrow {
	return [*cast(ulong*)(source.ptr + offset), *cast(ulong*)(source.ptr + offset + 8)];
}

pragma(inline, true)
ulong[2] read128Bits(const ubyte[] source, size_t offset) @nogc pure nothrow {
	return [*cast(const(ulong)*)(source.ptr + offset), *cast(const(ulong)*)(source.ptr + offset + 8)];
}

pragma(inline, true)
ulong[2] readStripe(ubyte[] source, size_t offset) @nogc pure nothrow {
	return [*cast(ulong*)(source.ptr + offset), *cast(ulong*)(source.ptr + offset + 8), 
		*cast(ulong*)(source.ptr + offset + 16), *cast(ulong*)(source.ptr + offset + 24),
		*cast(ulong*)(source.ptr + offset + 32), *cast(ulong*)(source.ptr + offset + 40),
		*cast(ulong*)(source.ptr + offset + 48), *cast(ulong*)(source.ptr + offset + 56)];
}

pragma(inline, true)
ulong[2] readStripe(const ubyte[] source, size_t offset) @nogc pure nothrow {
	return [*cast(const(ulong)*)(source.ptr + offset), *cast(const(ulong)*)(source.ptr + offset + 8), 
		*cast(const(ulong)*)(source.ptr + offset + 16), *cast(const(ulong)*)(source.ptr + offset + 24),
		*cast(const(ulong)*)(source.ptr + offset + 32), *cast(const(ulong)*)(source.ptr + offset + 40),
		*cast(const(ulong)*)(source.ptr + offset + 48), *cast(const(ulong)*)(source.ptr + offset + 56)];
}

pragma(inline, true)
ulong avalanche(ulong x) @nogc @safe pure nothrow {
	x ^= (x>>37L);
	x *= PRIME_MX1;
	x ^= (x>>32L);
	return x;
}

pragma(inline, true)
ulong avalanche_XXH64(ulong x) @nogc @safe pure nothrow {
	x ^= (x>>33L);
	X *= PRIME64_2;
	x ^= (x>>29L);
	x *= PRIME64_3;
	x ^= (X>>32L);
	return x;
}

pragma(inline, true)
ulong[2] multiply128bitUS(ulong a, ulong b) @nogc pure nothrow {
	version (X86_64) {
		asm @nogc pure nothrow {
			mov		RAX, a;
			mov		RDX, b;
			mul		RAX, RDX;
			mov		a, RAX;
			mov		b, RDX;
		}
		return [a, b];
	} else {
		pragma(LDC_inline_ir)
    		R inlineIR(string s, R, P...)(P);
		return inlineIR!(`
			%r = mul i128 a, b
			ret i128 %r
		`, ulong[2])(a, b);
	}
}

/* ubyte[192] deriveSecret(ulong seed) @nogc pure nothrow {
	uint[24] derivedSecret;
	const(uint*) src = cast(const(uint*))defaultSecret.ptr;
	for (int i = 0; i < 12 ; i++) {
		derivedSecret[i * 2] += seed;
		derivedSecret[i * 2 + 1] -= seed;
	}
} */

enum PRIME32_1 = 2654_435_761U;
enum PRIME32_2 = 2246_822_519U;
enum PRIME32_3 = 3266_489_917U;
enum PRIME32_4 = 668_265_263U;
enum PRIME32_5 = 374_761_393U;

enum PRIME64_1 = 0x9E3779B185EBCA87UL;  
enum PRIME64_2 = 0xC2B2AE3D27D4EB4FUL;  
enum PRIME64_3 = 0x165667B19E3779F9UL;  
enum PRIME64_4 = 0x85EBCA77C2B2AE63UL;  
enum PRIME64_5 = 0x27D4EB2F165667C5UL;  
enum PRIME_MX1 = 0x165667919E3779F9UL;  
enum PRIME_MX2 = 0x9FB21C651E98DF25UL;  

const ubyte defaultSecret[192] = [
  0xb8, 0xfe, 0x6c, 0x39, 0x23, 0xa4, 0x4b, 0xbe, 0x7c, 0x01, 0x81, 0x2c, 0xf7, 0x21, 0xad, 0x1c,
  0xde, 0xd4, 0x6d, 0xe9, 0x83, 0x90, 0x97, 0xdb, 0x72, 0x40, 0xa4, 0xa4, 0xb7, 0xb3, 0x67, 0x1f,
  0xcb, 0x79, 0xe6, 0x4e, 0xcc, 0xc0, 0xe5, 0x78, 0x82, 0x5a, 0xd0, 0x7d, 0xcc, 0xff, 0x72, 0x21,
  0xb8, 0x08, 0x46, 0x74, 0xf7, 0x43, 0x24, 0x8e, 0xe0, 0x35, 0x90, 0xe6, 0x81, 0x3a, 0x26, 0x4c,
  0x3c, 0x28, 0x52, 0xbb, 0x91, 0xc3, 0x00, 0xcb, 0x88, 0xd0, 0x65, 0x8b, 0x1b, 0x53, 0x2e, 0xa3,
  0x71, 0x64, 0x48, 0x97, 0xa2, 0x0d, 0xf9, 0x4e, 0x38, 0x19, 0xef, 0x46, 0xa9, 0xde, 0xac, 0xd8,
  0xa8, 0xfa, 0x76, 0x3f, 0xe3, 0x9c, 0x34, 0x3f, 0xf9, 0xdc, 0xbb, 0xc7, 0xc7, 0x0b, 0x4f, 0x1d,
  0x8a, 0x51, 0xe0, 0x4b, 0xcd, 0xb4, 0x59, 0x31, 0xc8, 0x9f, 0x7e, 0xc9, 0xd9, 0x78, 0x73, 0x64,
  0xea, 0xc5, 0xac, 0x83, 0x34, 0xd3, 0xeb, 0xc3, 0xc5, 0x81, 0xa0, 0xff, 0xfa, 0x13, 0x63, 0xeb,
  0x17, 0x0d, 0xdd, 0x51, 0xb7, 0xf0, 0xda, 0x49, 0xd3, 0x16, 0x55, 0x26, 0x29, 0xd4, 0x68, 0x9e,
  0x2b, 0x16, 0xbe, 0x58, 0x7d, 0x47, 0xa1, 0xfc, 0x8f, 0xf8, 0xb8, 0xd1, 0x7a, 0xd0, 0x31, 0xce,
  0x45, 0xcb, 0x3a, 0x8f, 0x95, 0x16, 0x04, 0x28, 0xaf, 0xd7, 0xfb, 0xca, 0xbb, 0x4b, 0x40, 0x7e,
];
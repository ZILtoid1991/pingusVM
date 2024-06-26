# General rules for bytecode

* All bytecode is little endian (starting with the least significant byte).
* All data mixed in with the bytecode is little endian.

Within the layout diagrams each byte's least significant bit is on the left hand side, unless noted otherwise.

# Register-based operations

Register based operations indicate operations done on registers, but also can use the top 240 stack entries as sources.
Least significant bit is always zero.

## Layout diagram of register-based operations

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|1IIIIIII|IIIIDDDD|AAAAAAAA|BBBBBBBB|
```

In case of register operations, the least significant bit of the least significant byte is always 1.

### Instruction code 

* Identified by I in the layout diagram.
* 11 bits, top seven bits of byte 0 are the least significant bits, lowest 4 bits of byte 1 are the most significant 
bits.
* Identifies the instruction by code.

### Destination register

* Identified by D in the layout diagram.
* 4 bits, top 4 bits of byte 1.
* Usually identifies the target of the operation, sometime can also read from it.

### Source A

* Identified by A in the layout diagram.
* 8 bits, byte 2 of it's entirety.
* 0-15: Register as source, 16-255: Top of stack 0-239.
* Identifies the A source of the operation.

### Source B/Immediate value within 0-255

* Identified by B in the layout diagram.
* 8 bits, byte 2 of it's entirety.
* 0-15: Register as source, 16-255: Top of stack 0-239.
* Identifies the B source of the operation, or an immediate value for bit shifts, etc.

## Instruction list

Code is hexanumeric number.                             

|Code|ASM  |Description                                     |Formula                                |
|----|-----|------------------------------------------------|---------------------------------------|
|000 |MOV  |Move                                            |`D = A`                                |
|001 |ADD  |Signed integer add                              |`D = A + B`                            |
|002 |SUB  |Signed integer subtract                         |`D = A - B`                            |
|003 |MUL  |Signed integer multiply                         |`D = A * B`                            |
|004 |DIV  |Signed integer division                         |`D = A / B`                            |
|005 |ADDU |Unsigned integer add                            |`D = A + B`                            |
|006 |SUBU |Unsigned integer subtract                       |`D = A - B`                            |
|007 |MULU |Unsigned integer multiply                       |`D = A * B`                            |
|008 |DIVU |Unsigned integer division                       |`D = A / B`                            |
|009 |MOD  |Singed modulo                                   |`D = A % B`                            |
|00A |SHL  |Logical shift left by register value            |`D = A<<B`                             |
|00B |SHLI |Logical shift left by immediate value           |`D = A<<(B)`                           |
|00C |SHR  |Logical shift right by register value           |`D = A>>>B`                            |
|00D |SHRI |Logical shift right by immediate value          |`D = A>>>(B)`                          |
|00E |SHA  |Arithmetic shift right by register value        |`D = A>>B`                             |
|00F |SHAI |Arithmetic shift right by immediate value       |`D = A>>(B)`                           |
|010 |AND  |Logical AND                                     |`D = A AND B`                          |
|011 |OR   |Logical OR                                      |`D = A OR B`                           |
|012 |XOR  |Logical XOR                                     |`D = A XOR B`                          |
|013 |NOT  |Logical NOT                                     |`D = NOT A`                            |
|014 |MAX  |Move the greater value to D(integer)            |`D = A > B ? A : B`                    |
|015 |MIN  |Move the lesser value to D(integer)             |`D = A < B ? A : B`                    |
|016 |LIMIT|Limit D's value between A and B(integer)        |`D = MIN(MAX(D, A), B)`                |
|017 |POWI |Integer power                                   |`D = A^B`                              |
|020 |MAC  |Multiply-accumulate(integer)                    |`D += A * B`                           |
|021 |MNC  |Multiply-subtract(integer)                      |`D -= A * B`                           |
|030 |FADD |Floating-point add                              |`D = A + B`                            |
|031 |FSUB |Floating-point subtract                         |`D = A - B`                            |
|032 |FMUL |Floating-point multiply                         |`D = A * B`                            |
|033 |FDIV |Floating-point division                         |`D = A / B`                            |
|034 |FMOD |Floating-point modulo                           |`D = A % B`                            |
|035 |FIADD|Add integer to floating-pont                    |`D[FP] = A[FP] + B[I]`                 |
|036 |FISUB|Subtract integer from floating-pont             |`D[FP] = A[FP] - B[I]`                 |
|037 |FIMUL|Multiply integer with floating-pont             |`D[FP] = A[FP] * B[I]`                 |
|038 |FIDIV|Divide floating-pont by integer                 |`D[FP] = A[FP] / B[I]`                 |
|039 |IFDIV|Divide integer by floating-point                |`D[FP] = A[I] / B[FP]`                 |
|03A |IFSUB|Subtract floating-point from integer            |`D[FP] = A[I] - B[FP]`                 |
|03B |FMAC |Multiply-accumulate(floating point)             |`D += A * B`                           |
|03C |FMNC |Multiply-subtract(floating point)               |`D -= A * B`                           |
|03D |ROOT |Return the Bth root of A(FP)                    |`D = ROOT(A, B)`                       |
|03E |POW  |Return the Bth exponent of A(FP)                |`D = A^B`                              |
|03F |LOG  |Return the logarithm of A(FP)                   |`D = log(A)`                           |
|040 |CVIF |Convert A integer into a floating point number  |`D[FP] = A[I]`                         |
|041 |CVRFI|Convert A into integer by rounding              |`D[I] = round(A[FP])`                  |
|042 |ROUND|Round A                                         |`D[FP] = round(A[FP])`                 |
|043 |CVTFI|Convert A into integer by truncating            |`D[I] = truncate(A[FP])`               |
|044 |TRNC |Truncate A                                      |`D[FP] = truncate(A[FP])`              |
|045 |RNDWB|Round A with B as point of reference            |`D = round(A, B)`                      |
|046 |FMAX |Move the greater value to D(floating-pont)      |`D = A > B ? A : B`                    |
|047 |FMIN |Move the lesser value to D(floating-pint)       |`D = A < B ? A : B`                    |
|048 |FLMT |Limit D's value between A and B(FP)             |`D = MIN(MAX(D, A), B)`                |
|049 |SIN  |Returns the sine of A                           |`D = SIN(A)`                           |
|04A |COS  |Returns the cosine of A                         |`D = COS(A)`                           |
|04B |TAN  |Returns the tangent of A                        |`D = TAN(A)`                           |
|060 |CMPEQ|Shifts 1 into D if equal, 0 otherwise(int)      |`D = (D<<1) OR A == B ? 1 : 0`         |
|061 |CMPGT|Shifts 1 into D if A > B, 0 otherwise(int)      |`D = (D<<1) OR A > B ? 1 : 0`          |
|062 |CMPGE|Shifts 1 into D if A >= B, 0 otherwise(int)     |`D = (D<<1) OR A >= B ? 1 : 0`         |
|063 |CMPNE|Shifts 1 into D if not equal, 0 otherwise(int)  |`D = (D<<1) OR A != B ? 1 : 0`         |
|064 |FCMEQ|Shifts 1 into D if equal, 0 otherwise(FP)       |`D = (D<<1) OR A == B ? 1 : 0`         |
|065 |FCMGT|Shifts 1 into D if A > B, 0 otherwise(FP)       |`D = (D<<1) OR A > B ? 1 : 0`          |
|066 |FCMGE|Shifts 1 into D if A >= B, 0 otherwise(FP)      |`D = (D<<1) OR A >= B ? 1 : 0`         |
|067 |FCMNE|Shifts 1 into D if not equal, 0 otherwise(FP)   |`D = (D<<1) OR A != B ? 1 : 0`         |
|068 |FCMCL|Sets D to 1 if A and B are within D(FP)         |`D = A + D > B AND A - D < B ? 1 : 0`  |
|069 |ISNAN|Shifts 1 into D if A is NaN                     |`D = (D<<1) OR ISNAN(A) ? 1 : 0`       |
|0A0 |BPRD |Reads 64 bit value into D from base pointer B with offset A|`D = bp(B)[A]`|
|0A1 |BPRDW|Reads 32 bit unsigned value into D from base pointer B with offset A|`D = (WORD)bp(B)[A]`|
|0A2 |BPRDHW|Reads 16 bit unsigned value into D from base pointer B with offset A|`D = (HALFWORD)bp(B)[A]`|
|0A3 |BPRDB|Reads 8 bit unsigned value into D from base pointer B with offset A|`D = (BYTE)bp(B)[A]`|
|0A4 |BPRDSW|Reads 32 bit signed value into D from base pointer B with offset A|`D = (SWORD)bp(B)[A]`|
|0A5 |BPRDSHW|Reads 16 bit signed value into D from base pointer B with offset A|`D = (SHWORD)bp(B)[A]`|
|0A6 |BPRDSB|Reads 8 bit signed value into D from base pointer B with offset A|`D = (SBYTE)bp(B)[A]`|
|0A7 |BPRDBI|Reads a bit into D from base pointer B with offset A|`D = (D<<1) | (BIT)bp(B)[A]`|
|0A8 |BPRDSF|Reads a single-precision floating point value into D from base pointer B with offset A|`D = (SINGLE)bp(B)[A]`|
|0B0 |BPWR |Writes 64 bit value from D into base pointer B with offset A|`bp(B)[A] = D`|
|0B1 |BPWRW|Writes 32 bit value from D into base pointer B with offset A|`(WORD)bp(B)[A] = D`|
|0B2 |BPWRHW|Writes 16 bit value from D into base pointer B with offset A|`(HALFWORD)bp(B)[A] = D`|
|0B3 |BPWRB|Writes 8 bit value from D into base pointer B with offset A|`(BYTE)bp(B)[A] = D`|
|0B7 |BPWRBI|Writes a bit into base pointer B with offset A depending on least significant bit of D|`(BIT)bp(B)[A] = D[0]`|
|0B8 |BPWRSF|Writes a single-precision floating point value from D into base pointer B with offset A|`(SINGLE)bp(B)[A] = D`|

## Assembly examples

* `ADD RA R4 R5`
* `FMAC R0 R4 S20`

# Other operations

Other operations include stack, conditional jump, interrupts, internal and external function calls, etc.

Least significant bit of least significant byte is always zero. Most operations of this kind use 7 bits
for instruction codes, but some use extra bits for multiplexing.

## Stack operations

### Layout diagram

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|0IIIIIII|RRRRDST0|AAAAAAAA|AAAAAAAA|
```

* R indicates the register number if register is used by the operations.
* D usually switches between register or direct data, otherwise it's zero.
* S is usually zero, otherwise indicates 32 bit integer values for PUSHD and POKED operations.
* A indicates the amount for the given operation, or the lower 8 bits can serve as .
* T is used for PEEKR so far.

### Instruction code

The most significant 7 bits of the least significant byte identifies the instruction to be done, and are 
identified by I in the layout diagram.

|Code|D|ASM  |Description                                           |ASM example                    |
|----|-|-----|------------------------------------------------------|-------------------------------|
|00  |0|NONE |Null operator, does nothing                           |`NONE`                         |
|01  |0|PUSH |Pushes the register to the top of the stack           |`PUSH R`                       |
|01  |1|PUSHD|Pushes an arbitrary amount of data to the stack       |`PUSHD [F] [...]`              |
|02  |0|POP  |Pops the top of the stack into the specified register |`POP R`                        |
|02  |1|DSCRD|Discards the specified amount of data from the stack  |`DSCRD ####`                   |
|03  |0|PEEK |Gets a previous element from the stack without pop    |`PEEK R ####`                  |
|03  |1|PEEKD|Pushes the specified previous element on the stack to the top|`PEEKD ####`            |
|03|0/T|PEEKR|Gets the element to R by register/stack value of A    |`PEEKR R A`                    |
|03|1/T|PKDR |Pushes the element specified by register/stack value of A on the top of the stack|`PKDR A`|
|04  |0|POKE |Sets a previous element in the stack without push     |`POKE R ####`                  |
|04  |1|POKED|Sets a previous element in the stack to the supplied data|`POKED [val] ####`          |

**Note on PUSHD binary representation:** A PUSHD operation is followed by a given amount of data to be pushed
to the stack, and is created with the purpose of making it possible to easily create static arrays on the
stack. The data can be either 64 bit (S = 0) or 32 bit (S = 1).

**Note on POKED binary representation:** A POKED operation is followed by either a 64 bit (S = 0) or 32 bit
(S = 1) data.

## Heap/variable operations

The VM has a variable type system, as a lot of scripting languages use dynamic type systems, also these are
used for any heap allocations. See `var.md` for further information.

### Layout diagram (Type 1)

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|01010000|IIIIIIII|TTTTTTTT|TTTTTTSD|
```

* Heap operations use byte 1 for instruction codes.
* If S is not zero, it indicates the next 32 bit word is a size indicator.
* If D is not zero, it indicates there's a data blob following the instruction and the size indicator.
* T is either a type indicator where it's applicable (see `var.md` for further information on type codes),
but also can indicate which element of the stack contains the var structure related to the heap-allocated
object to be removed.

### Layout diagram (Type 2)

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|01010000|IIIIIIII|AAAABBBB|CCCCCCCC|
```

* Heap operations use byte 1 for instruction codes.
* A and B indicate registers to work with.
* C indicates the position of the var in the stack, or the type with `CRTFR` (only limited number of types
are valid).

### Layout diagram (Type 3)

```
|Byte0   |Byte1   |Byte2   |Byte3   |Byte4   |Byte5   |Byte6   |Byte7   |
|01010000|IIIIIIII|AAAAAAAA|BBBBBBBB|CCCCCCCC|DDDDDDDD|M0000000|00000000|
```

* Does operations on variants allocated on the stack.
* If binary blob has an associated metatable, it'll try to call instruction overrides from that.
* A and B are always stack positions, C is compare code or immediate value, D is usually stack but is a register number for `VCMP`.
* If D used as a 
* M enables immediate value for shift commands.

### Instruction codes

|Code|ASM   |T|Description                                          |ASM example                    |
|----|------|-|-----------------------------------------------------|-------------------------------|
|00  |CRT   |1|Creates a var, then pushes it to the stack       |`CRT [type] {size} {PC} {data=...}`|
|01  |DSTR  |1|Destroys a var's reference, and sets it to all zeros |`DSTR ####`                    |
|02  |RESIZE|1|Resizes a var if it's an array, etc.                 |`RESIZE #### [newsize]`        |
|03  |READ  |2|Reads index A from the selected array into B         |`READ a b ##`                  |
|04  |WRITE |2|Writes B into selected array at index A              |`WRITE a b ##`                 |
|05  |CRTFR |2|Create var from register                             |`CRTFR [type] a`               |
|06  |CHKTP |2|Returns the type ID of the given var into A          |`CHKTP a ##`                   |
|07  |READD |2|Reads locally stored data into B                     |`READD b ##`                   |
|08  |READB |2|Byte read from binary blob/array (unaligned)         |`READB a b ##`                 |
|09  |READHW|2|Halfword read from binary blob/array (unaligned)     |`READHW a b ##`                |
|0A  |READW |2|Word read from binary blob/array (unaligned)         |`READW a b ##`                 |
|0B  |READDW|2|Doubleword read from binary blob/array (unaligned)   |`READDW a b ##`                |
|0C  |WRTB  |2|Byte write to binary blob/array (unaligned)          |`WRTB a b ##`                  |
|0D  |WRTHW |2|Halfword write to binary blob/array (unaligned)      |`WRTHW a b ##`                 |
|0E  |WRTW  |2|Word write to binary blob/array (unaligned)          |`WRTW a b ##`                  |
|0F  |WRTDW |2|Doubleword write to binary blob/array (unaligned)    |`WRTDW a b ##`                 |
|10  |SLICE |2|Creates a slice of an array between indexes a and B, puts new array on top of the stack|`SLICE a b ##`|
|11  |VADD  |3|Adds variant A and B, stores in D                    |`VADD a b d`                   |
|12  |VSUB  |3|Subtracts variant B from A, stores in D              |`VSUB a b d`                   |
|13  |VMUL  |3|Multiplies variant A and B, stores in D              |`VMUL a b d`                   |
|14  |VDIV  |3|Divides variant A by B, stores in D                  |`VDIV a b d`                   |
|15  |VMOD  |3|Stores the modulo of A by B in D                     |`VMOD a b d`                   |
|16  |VLSH  |3|Left shifts variant A by B, stores in D              |`VLSH a b d` or `VLSH a [c] d` |
|17  |VRSH  |3|Right shifts variant A by B, stores in D             |`VRSH a b d` or `VRSH a [c] d` |
|18  |VRASH |3|Right arithmetic shifts variant A by B stores in D  |`VRASH a b d` or `VRASH a [c] d`|
|19  |VAND  |3|Logically ands A and B, stores in D                  |`VAND a b d`                   |
|1A  |VOR   |3|Logically ors A and B, stores in D                   |`VOR a b d`                    |
|1B  |VXOR  |3|Logically xors A and B, stores in D                  |`VXOR a b d`                   |
|1C  |VNOT  |3|Logically nots A, stores in D                        |`VNOT a d`                     |
|1D  |VCMP  |3|Compares A and B for C, stores in register D         |`VCMP a b cmpcode d{register}` |

## Conditional jump operations

### Layout diagram

```
|Byte0   |Byte1   |Byte2   |Byte3   |Byte4   |Byte5   |Byte6   |Byte7   |
|00110000|CCCCCCM0|AAAAAAAA|BBBBBBBB|PPPPPPPP|PPPPPPPP|PPPPPPPP|PPPPPPPP|
```

* C designates a condition.
* M toggles mode. If 0, A and B will be compared. If 1, a 64 bit value follows it, which is compared against A.
* P designates the position of jump.

### Assembly

`JMPxx a b [jumpPosition]`

### Condition codes

|Code|ASM|Description                                               |
|----|---|----------------------------------------------------------|
|00  |   |Jump always                                               |
|01  |EQ |Jump if A is equals with B                                |
|02  |GE |Jump if A is greater than or equals with B                |
|03  |GT |Jump if A is greater than B                               |
|04  |LE |Jump if A is less than or equals with B                   |
|05  |LT |Jump if A is less than B                                  |
|06  |SE |Jump if at least one bit high in A and B at the same position|
|07  |NE |Jump if A isn't equal with B                              |
|08  |NH |Jump if none of the bits high in A and B at the same position|

## Function call operations

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|00001000|VFTMP000|NNNNNNNN|OOOOOOOO|
|Package name hash                  |
|Metatable name hash                |
|Function name hash                 |
|Calling convetion hash             |
```

* If V is 1, the function will be called from a variable type, then the extra words won't be present, and instead O will indicate on the stack which variable holds the function.
* If T is 1, the function is vararg type, and calling convention hash is not present.
* If F is 1, the function is a fiber.
* If M is 1, the function is a member of a metatable, and metatable hash is present, which otherwise is not.
* If P is 1, the function call has a package name hash.
* N indicate the number of varargs passed to the function.

### Assembly

`CALL [functionName] {callingConv}/{varargNum}` for calling regular functions.

`CALL [stackPos] {varargNum}` for calling functions from variables.

Replacing `CALL` with `THREAD` will call the function as a fiber if possible.

Examples:

`CALL exampleFunc $INT $INT $FLOAT $UserDefinedType`

`CALL UserDefinedType.exampleFunc $UserDefinedType $INT $INT`

`THREAD exampleFunc 3`

### Calling conventions

Registers 0-7 can hold INT, UINT, FLOAT arguments, and are caller saved. Registers 8-F are callee saved.

Stack holds any further arguments if needed, especially variable type arguments. Top is first.

For return values, both registers and the stack can hold them. Top is first.

## Function/interrupt return

#### Layout diagram

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|01001000|00000000|00000000|00000000|
```

There's no further arguments for this instruction.

#### Assembly

`RET`

## Interrupt operations

In code, only user interrupts can be generated.

### Interrupt generation

#### Layout diagram

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|00101000|GGGGGGGG|IIIIIIII|IIIIIIII|
```

* I designates the interrupt code.
* G designates the group code (0-127= non-returning interrupts, 128-255= returning interrupts).

#### Assembly

`INT [interruptGroup] [interruptNumber]`

### Interrupt registration

Registers an interrupt entry point.

#### Layout diagram

```
|Byte0   |Byte1   |Byte2   |Byte3   |Byte4   |Byte5   |Byte6   |Byte7   |
|01101000|GGGGGGGG|G0000000|00000000|PPPPPPPP|PPPPPPPP|PPPPPPPP|PPPPPPPP|
```

* G designates the group code (0-127= non-returning interrupts, 128-255= returning interrupts, 256= interrupts from interpreter, 257-511= currently undefined).
* P designates entry point.

#### Assembly

`INTREG [interruptGroup] [entryPointLabel]`

### Interrupt clear

Clears an interrupt entry point.

#### Layout diagram

```
|Byte0   |Byte1   |Byte2   |Byte3   |
|00011000|GGGGGGGG|G0000000|00000000|
```

* G designates the group code (0-127= non-returning interrupts, 128-255= returning interrupts, 256= interrupts from interpreter, 257-511= currently undefined).

#### Assembly

`INTCLR [interruptGroup]`

## Metatable and function registration

While it's advised to use the binary formats own function and metatable registration systems.

## Chain operation

Loads a binary file by the given name, and runs any initialization function it has.
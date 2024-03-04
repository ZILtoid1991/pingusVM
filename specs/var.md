# Binary layout diagram

```
|Word|Byte0   |Byte1    |Byte2    |Byte3    |
|----|--------|---------|---------|---------|
|0   |TTTTTTTT|PC0000000|000000000|000000000|
|1   |Blob size/Function ID                 |
|2/3 |Locally storable data, or reference ID|
```

* P indicates array protection bit. If not zero, slices generated from it will be always copies of the original.
* C indicates constant status. If not zero, memory protection is engaged on the type.

# Type identifiers

8 least significant bits:

|ID|Description                                                                         |ASM       |
|--|------------------------------------------------------------------------------------|----------|
|00|Null type                                                                           |NULL      |
|01|Signed integer                                                                      |INT       |
|02|Unsigned integer                                                                    |UINT      |
|03|Floating-point                                                                      |FLOAT     |
|04|Small binary blob able to fit into 64 bit                                           |SBLOB     |
|05|Large binary blob stored on the heap                                                |BLOB      |
|06|String                                                                              |STR       |
|07|Hashmap                                                                             |HASHMAP   |
|08|Array                                                                               |ARRAY     |
|09|Signed integer array                                                                |INTARRAY  |
|0A|Unsigned integer array                                                              |UINTARRAY |
|0B|Floating-point array                                                                |FLOATARRAY|
|0C|Array of strings                                                                    |STRATTAY  |
|0D|Array of binary blobs                                                               |BLOBARRAY |
|0E|Bitarray                                                                            |BITARRAY  |
|0F|Reference type                                                                      |REF       |
|10|Function                                                                            |FUNC      |
|11|Fiber                                                                               |FIBER     |
|12|Delegate                                                                            |DELEG     |
|13|Fiber of a delegate                                                                 |DELEGFIBER|

# Best practices

* If a language implements enumerators, then they should be of integer types if possible.
* Binary blobs are primarily meant for implementing an easy and fast interfacing 
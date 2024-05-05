module pingus.etc;

T min(T)(T a, T b) @nogc @safe pure nothrow {
    return a < b ? a : b;
}

T max(T)(T a, T b) @nogc @safe pure nothrow {
    return a > b ? a : b;
}

T limit(T)(T a, T b, T c) @nogc @safe pure nothrow {
    return min(max(a, b), c);
}

double getFloat(ulong input) @nogc pure nothrow {
    return *cast(double*)(cast(void*)&input);
}

ulong forceFloat(double input) @nogc pure nothrow {
    return *cast(ulong*)(cast(void*)&input);
}
module pingus.vm;

public import pingus.types;

class PingusVM {
	struct Instance {
		ulong[]		stack;
		ulong[16]	registers;
		uint		instanceNum;
		uint		pos;
		uint[]		stackContext;
	}
	PingusBinFile[]	files;

	private int decodeNextInstr(ref Instance i) {
		return 0;
	}
}
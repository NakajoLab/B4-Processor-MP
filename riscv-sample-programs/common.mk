TOOL_PREFIX := riscv64-none-elf-
export CC := $(TOOL_PREFIX)gcc
export OBJCOPY := $(TOOL_PREFIX)objcopy
export OBJDUMP := $(TOOL_PREFIX)objdump
export CFLAGS := -nodefaultlibs -nostdlib -march=rv64imac_zicsr -mabi=lp64 -no-pie -static -g -O3
export LINKER_SCRIPT := ../linker.ld

CFLAGS += -T$(LINKER_SCRIPT)

.PHONY:clean
all: $(PROGRAMNAME).hex $(PROGRAMNAME).o $(PROGRAMNAME).dump $(PROGRAMNAME).64.hex


$(PROGRAMNAME).o: $(SOURCES) $(LINKER_SCRIPT)
	$(CC) $(CFLAGS) $(SOURCES) -o $@

$(PROGRAMNAME).text.binary: $(PROGRAMNAME).o
	$(OBJCOPY) -O binary $< $@

$(PROGRAMNAME).hex: $(PROGRAMNAME).text.binary
	od -An -t x1 $< -w1 -v | tr -d " " > $@

$(PROGRAMNAME).64.hex: $(PROGRAMNAME).text.binary
	od -An -t x8 $< -w8 -v | tr -d " " > $@

$(PROGRAMNAME).dump: $(PROGRAMNAME).o
	$(OBJDUMP) -d -j .text -j .data -j .rodata -j .bss  $< -M no-aliases -M numeric > $@

clean:
	$(RM) *.hex *.binary *.o *.dump
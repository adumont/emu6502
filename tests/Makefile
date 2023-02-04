SRCS = \
	6502_functional_test.s \
	65C02_extended_opcodes_test.s

# SRCS = \
# 	6502_decimal_test.s \
# 	6502_functional_test.s \
# 	65C02_extended_opcodes_test.s

OBJS = $(SRCS:%.s=%.bin)

all: $(OBJS)

%.bin: %.s
	ca65 -l $*.lst $<
	ld65 $*.o -o $@ -m $*.map -C example.cfg

clean:
	rm *.o *.lst *.map *.bin
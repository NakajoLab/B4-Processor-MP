SUBDIRS :=  matmul_send_2_2_c \
            matmul_send_2_c \
			matmul_send_4_2_c \
			matmul_send_8_2_c \
			matmul_send_8_c \
			matmul_send_2a_c \
#            matmul_amo_2_2_c \
#			loadtest_c \
			matmul_amo_4_2_c \
			matmul_amo_8_2_c \
#			matmul_amoadd_2_c \
			matmul_amoadd_4_c \
			matmul_amoadd_8_c \
			matmul_send_2_2_c \
			matmul_send_4_2_c \
			matmul_send_8_2_c \
#			matmul_c \
#			matmul_send_2_2_c \
			matmul_send_4_2_c \
			matmul_send_8_2_c \
#			sum_c \
			send \
#			Mtype_c \
   			send_2_c \
#			sum_send_2_c \
			sum_send_4_c \
			sum_send_8_c \
			sum_amo_2_c \
#			sum_amo_4_c \
#			sum_amo_8_c \
#			sum_amo_16_c \
#			sum_send_16_c \
#			simple_loader_uart \
			fibonacci \
			branch \
			call_ret \
			many_add \
			many_add_out_of_order \
			load_store \
			fibonacci_c \
			many_load_store \
			load_plus_arithmetic \
			load_after_store \
			enter_c \
			calculation_c \
			loop_c \
			load_store_cross \
			led_show \
			csrtest \
			simple_loader_hello \
			illegal_inst \
			io_test \
			pext_test \

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

SUBDIRS_CLEAN = $(addsuffix .clean, $(SUBDIRS))
$(SUBDIRS_CLEAN):
	$(MAKE) -C $(basename $@) clean


.PHONY: all clean $(SUBDIRS) $(SUBDIRS_CLEAN)

clean: $(SUBDIRS_CLEAN)

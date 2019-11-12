# Clear enviroment
clear -all

# analyze the design
analyze -vhdl ../../riscv/core_rv32i/control.vhd \
			  ../../riscv/core_rv32i/bshifter.vhd \
			  ../../riscv/core_rv32i/alu.vhd \
			  ../../riscv/core_rv32i/reg_bank.vhd \
			  ../../riscv/core_rv32i/int_control.vhd \
			  ../../riscv/core_rv32i/datapath.vhd \
			  ../../riscv/core_rv32i/cpu.vhd;

# Analyze property files
analyze -sva bindings.sva v_alu.sva v_datapath.sva ;

# elaborate the design, point to the design top level
elaborate -vhdl -top {processor}

# Set up Clocks and Resets
clock clk_i -factor 1 -phase 1
reset -expression {rst_i = '1'};

# get designs statistics
get_design_info
prove -all

# Report proof results
report 

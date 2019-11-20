# Clear enviroment
clear -all

# Configure grid engine
set_proofgrid_manager on
set_cache_proof_simplification on
set_prove_orchestration on
set_prove_cache on
set_prove_cache_traces true
set_proofgrid_per_engine_max_jobs 4

# analyze the design
analyze -vhdl {
    ../../riscv/core_rv32im_nodiv/control.vhd
    ../../riscv/core_rv32im_nodiv/bshifter.vhd
    ../../riscv/core_rv32im_nodiv/mul.vhd
    ../../riscv/core_rv32im_nodiv/alu.vhd
    ../../riscv/core_rv32im_nodiv/reg_bank.vhd
    ../../riscv/core_rv32im_nodiv/int_control.vhd
    ../../riscv/core_rv32im_nodiv/datapath.vhd
    ../../riscv/core_rv32im_nodiv/cpu.vhd }

# Analyze property files
analyze -sva bindings.sva v_alu_mul.sva v_datapath_mul.sv v_regbank.sv

# elaborate the design, point to the design top level
elaborate -vhdl -top {processor} -bbox_mul 128

# Set up Clocks and Resets
clock clk_i -factor 1 -phase 1
reset -expression {rst_i = '1'};

# get designs statistics
get_design_info
prove -all

# Report proof results
report

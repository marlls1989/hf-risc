// RISCV instruction set: https://rv8.io/isa.html
// Repository: https://github.com/sjohann81/hf-risc

module v_datapath
  (clock,
   reset,
   stall_reg,
   irq,
   irq_ack_s,
   except,
   branch_taken,
   jump_taken,
   pc,
   pc_plus4,
   stall_sig,
   mwait,
   mem_write_ctl_r,
   mem_read_ctl_r,
   funct3,
   funct7,
   alu_src1,
   alu_src2,
   write_data,
   opcode,
   alu_op_ctl_r,
   imm_uj_r,
   imm_i_r,
   read_data1,
   imm_sb_r,
   data_access,
   data_in,
   data_out,
   address,
   data_w,
   exception,
   alu_wait,
   wreg);

   // RISC-V signals
   input clock, reset, stall_reg, irq, irq_ack_s, except, branch_taken, jump_taken, stall_sig, mwait, wreg, exception, alu_wait;
   input [31:0] pc, pc_plus4, alu_src1, alu_src2, write_data, imm_uj_r, imm_i_r, read_data1, imm_sb_r;
   input [1:0]  mem_write_ctl_r, mem_read_ctl_r;
   input [2:0]  funct3;
   input [3:0]  alu_op_ctl_r;
   input [6:0]  opcode, funct7;

   // Memory Interface
   input        data_access;
   input [31:0] data_in, data_out, address;
   input [3:0]  data_w;


   // Assertion internal signals
   logic [31:0] and_result, or_result, xor_result, shiftl_result, shiftr_result, shiftar_result, sltu_reult, slt_result;
   logic [31:0] reordered_data_in;
   assign reordered_data_in = {data_in[7:0], data_in[15:8], data_in[23:16], data_in[31:24]};

   // Default clock edge
   default clocking @(posedge clock); endclocking

   // Default reset state
   default disable iff reset;

   // Auxiliar variables to verification logic operations
   assign and_result = alu_src1 & alu_src2;
   assign  or_result = alu_src1 | alu_src2;
   assign xor_result = alu_src1 ^ alu_src2;
   assign sltu_result = $unsigned(alu_src1) < $unsigned(alu_src2) ? 1 : 0;
   assign slt_result = $signed(alu_src1) < $signed(alu_src2) ? 1 : 0;

   // Auxiliar variables to verification shift operations
   assign shiftl_result = alu_src1 << alu_src2[4:0];
   assign shiftr_result = alu_src1 >> alu_src2[4:0];
   assign shiftar_result = $signed(alu_src1) >>> alu_src2[4:0];

   ///////////////////////////
   // Define opcodes values //
   ///////////////////////////

   // Opcode to immediate type instructions
`define IMM_INST 19

   // Opcode to register type instructions
`define REG_INST 51

   // Opcode to branch type instructions
`define BRANCH_INST 99

   ///////////////////////
   // Define CPU states //
   ///////////////////////

   // Not memory operation and not jump instruction
`define NOT_MEM_J ((mem_read_ctl_r==0) and (jump_taken==0))

   // Not pipeline stall
`define NOT_STALL ((stall_sig==0) and (mwait==0))

   ///////////////////////////////////////
   //       Verifying CPU features      //
   ///////////////////////////////////////

   // PC + 4 incr verification
   property pc_incr;
      `NOT_MEM_J and `NOT_STALL and (stall_reg==0) and (irq==0) and (irq_ack_s==0) and (except==0) and (branch_taken==0) |=> (pc==$past(pc)+4);
   endproperty
   a_pc_incr: assert property (pc_incr);
   c_pc_incr: cover property (pc_incr);

   // NOP verification
   property no_op;
      (opcode==0) and `NOT_MEM_J and `NOT_STALL and (stall_reg==0) and (irq==0) and (irq_ack_s==0) and (except==0) and (branch_taken==0) |=> (pc==$past(pc)+4) ##[0:3](wreg==0);
   endproperty
   a_no_op: assert property (no_op);
   c_no_op: cover property (no_op);

   // Stall verification
   property stall_on;
      (stall_sig==1) |=> (pc==$past(pc));
   endproperty
   a_stall_on: assert property (stall_on);
   c_stall_on: cover property (stall_on);

   ///////////////////////////////////////
   // Verifying arithmetic instructions //
   ///////////////////////////////////////

   // AUIPC
   property auipc_inst;
      (opcode==23) and `NOT_MEM_J and `NOT_STALL |-> ##[1:3](wreg && write_data==$past(pc,2)+alu_src2);
   endproperty
   a_auipc_inst: assert property (auipc_inst);
   c_auipc_inst: cover property (auipc_inst);

   // ADDIU instruction
   property addi_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==0) |-> ##[1:3](wreg && write_data==$unsigned(alu_src1)+$unsigned(alu_src2));
   endproperty
   a_addi_inst: assert property (addi_inst);
   c_addi_inst: cover property (addi_inst);

   // ADDU instruction
   property add_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7==0) |-> ##[1:3](wreg && write_data==$unsigned(alu_src1)+$unsigned(alu_src2));
   endproperty
   a_add_inst: assert property (add_inst);
   c_add_inst: cover property (add_inst);

   // SUBU instruction
   property sub_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0)  and (funct7==32) |-> ##[1:3](wreg && write_data==$unsigned(alu_src1)-$unsigned(alu_src2));
   endproperty
   a_sub_inst: assert property (sub_inst);
   c_sub_inst: cover property (sub_inst);

   //////////////////////////////////
   // Verifying logic instructions //
   //////////////////////////////////

   // XORI instruction
   property xori_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==4) |-> ##[1:3](wreg && write_data==xor_result);
   endproperty
   a_xori_inst: assert property (xori_inst);
   c_xori_inst: cover property (xori_inst);

   // ORI instruction
   property ori_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==6) |-> ##[1:3](wreg && write_data==or_result);
   endproperty
   a_ori_inst: assert property (ori_inst);
   c_ori_inst: cover property (ori_inst);

   // ANDI instruction
   property andi_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==7) |-> ##[1:3](wreg && write_data==and_result);
   endproperty
   a_andi_inst: assert property (andi_inst);
   c_andi_inst: cover property (andi_inst);

   // XOR instruction
   property xor_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==4) |-> ##[1:3](wreg && write_data==xor_result);
   endproperty
   a_xor_inst: assert property (xor_inst);
   c_xor_inst: cover property (xor_inst);

   // OR instruction
   property or_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==6) |-> ##[1:3](wreg && write_data==or_result);
   endproperty
   a_or_inst: assert property (or_inst);
   c_or_inst: cover property (or_inst);

   // AND instruction
   property and_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==7) |-> ##[1:3](wreg && write_data==and_result);
   endproperty
   a_and_inst: assert property (and_inst);
   c_and_inst: cover property (and_inst);

   ///////////////////////////////////////
   // Verifying comparison instructions //
   ///////////////////////////////////////

   // SLTI instruction
   property slti_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==2) |-> ##[0:3](wreg && write_data==slt_result);
   endproperty
   a_slti_inst: assert property (slti_inst);
   c_slti_inst: cover property (slti_inst);

   // SLTIU instruction
   property sltiu_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==3) |-> ##[0:3](wreg && write_data==sltu_result);
   endproperty
   a_sltiu_inst: assert property (sltiu_inst);
   c_sltiu_inst: cover property (sltiu_inst);

   // SLT instruction
   property slt_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==2) and (funct7==0) |-> ##[0:3](wreg && write_data==slt_result);
   endproperty
   a_slt_inst: assert property (slt_inst);
   c_slt_inst: cover property (slt_inst);

   // SLTU instruction
   property sltu_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==3) and (funct7==0) |-> ##[0:3](wreg && write_data==sltu_result);
   endproperty
   a_sltu_inst: assert property (sltu_inst);
   c_sltu_inst: cover property (sltu_inst);

   ///////////////////////////////////////
   //    Verifying shift instructions   //
   ///////////////////////////////////////

   // SLL instruction
   property sll_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==1) and (funct7==0) |-> ##[0:3](wreg && write_data==shiftl_result);
   endproperty
   a_sll_inst: assert property (sll_inst);
   c_sll_inst: cover property (sll_inst);

   // SLLI instruction
   property slli_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==1) and (funct7==0) |-> ##[0:3](wreg && write_data==shiftl_result);
   endproperty
   a_slli_inst: assert property (slli_inst);
   c_slli_inst: cover property (slli_inst);

   // SRL instruction
   property srl_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==5) and (funct7==0) |-> ##[0:3](wreg && write_data==shiftr_result);
   endproperty
   a_srl_inst: assert property (srl_inst);
   c_srl_inst: cover property (srl_inst);

   // SRLI instruction
   property srli_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==5) and (funct7==0) |-> ##[0:3](wreg && write_data==shiftr_result);
   endproperty
   a_srli_inst: assert property (srli_inst);
   c_srli_inst: cover property (srli_inst);

   // SRA instruction
   property sra_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==5) and (funct7==32) |-> ##[0:3](wreg && write_data==shiftar_result);
   endproperty
   a_sra_inst: assert property (sra_inst);
   c_sra_inst: cover property (sra_inst);

   // SRAI instruction
   property srai_inst;
      `NOT_MEM_J and `NOT_STALL and (opcode==`IMM_INST) and (funct3==5) and (funct7==32) |-> ##[0:3](wreg && write_data==shiftar_result);
   endproperty
   a_srai_inst: assert property (srai_inst);
   c_srai_inst: cover property (srai_inst);

   ///////////////////////////////////////
   //    Verifying branch instructions  //
   ///////////////////////////////////////

   // BEQ instruction
   property beq_inst;
      (opcode==`BRANCH_INST) and (funct3==0) ##1 ((branch_taken==1) and (alu_src1==alu_src2) and `NOT_STALL) |=> ##[0:3](pc==$past(pc,3)+$past(imm_sb_r));
   endproperty
   a_beq_inst: assert property (beq_inst);
   c_beq_inst: cover property (beq_inst);

   // BNE instruction
   property bne_inst;
      (opcode==`BRANCH_INST) and (funct3==1) ##1 ((branch_taken==1) and (alu_src1!=alu_src2) and `NOT_STALL) |=> ##[0:3](pc==$past(pc,3)+$past(imm_sb_r));
   endproperty
   a_bne_inst: assert property (bne_inst);
   c_bne_inst: cover property (bne_inst);

   // BLT instruction
   property blt_inst;
      (opcode==`BRANCH_INST) and (funct3==4) ##1 ((branch_taken==1) and (alu_src1<alu_src2) and `NOT_STALL) |=> ##[0:3](pc==$past(pc,3)+$past(imm_sb_r));
   endproperty
   a_blt_inst: assert property (blt_inst);
   c_blt_inst: cover property (blt_inst);

   // BGE instruction
   property bge_inst;
      (opcode==`BRANCH_INST) and (funct3==5) ##1 ((branch_taken==1) and (alu_src1>=alu_src2) and `NOT_STALL) |=> ##[0:3](pc==$past(pc,3)+$past(imm_sb_r));
   endproperty
   a_bge_inst: assert property (bge_inst);
   c_bge_inst: cover property (bge_inst);

   // BLTU instruction
   property bltu_inst;
      (opcode==`BRANCH_INST) and (funct3==6) ##1 ((branch_taken==1) and ($unsigned(alu_src1)<$unsigned(alu_src2)) and `NOT_STALL) |=> ##[0:3](pc==$past(pc,3)+$past(imm_sb_r));
   endproperty
   a_bltu_inst: assert property (bltu_inst);
   c_bltu_inst: cover property (bltu_inst);

   // BGEU instruction
   property bgeu_inst;
      (opcode==`BRANCH_INST) and (funct3==7) ##1 ((branch_taken==1) and ($unsigned(alu_src1)>=$unsigned(alu_src2)) and `NOT_STALL) |=> ##[0:3](pc==$past(pc,3)+$past(imm_sb_r));
   endproperty
   a_bgeu_inst: assert property (bgeu_inst);
   c_bgeu_inst: cover property (bgeu_inst);

   ///////////////////////////////////////
   //    Verifying jump instructions   //
   ///////////////////////////////////////

   // JAL instruction
   property jal_inst;
      // Ver parametro past $past(signal,3)
      (opcode==111) ##1 ((jump_taken==1) and `NOT_STALL) |=> ##[0:3] (pc==$past(pc,3)+$past(imm_uj_r));
   endproperty
   a_jal_inst: assert property (jal_inst);
   c_jal_inst: cover property (jal_inst);

   // JALR instruction
   property jalr_inst;
      (opcode==103) ##1 ((jump_taken==1) and `NOT_STALL) |=> (pc==$past(read_data1)+$past(imm_i_r));
   endproperty
   a_jalr_inst: assert property (jalr_inst);
   c_jalr_inst: cover property (jalr_inst);

   //////////////////////////
   // Instruction Fetching //
   //////////////////////////

   property fetch_pc;
      !data_access |-> address == pc;
   endproperty // fetch_pc
   a_fetch_pc: assert property (fetch_pc);
   c_fetch_pc: cover property (fetch_pc);

   property fetch;
      (!exception and !branch_taken and !jump_taken and !data_access and !irq and `NOT_STALL)[*3] |->
        (opcode == reordered_data_in[6:0] && funct3 == reordered_data_in[14:12] && funct7 == reordered_data_in[31:25]);
   endproperty // fetch
   a_fetch: assert property (fetch);
   c_fetch: cover property (fetch);

   ////////////////
   // Multiplier //
   ////////////////

   function logic [63:0] muls
     (input signed [31:0] a, b);
      return {{32{a[31]}},a}*{{32{b[31]}},b};
   endfunction // muls

   function logic [63:0] mulu
     (input unsigned [31:0] a, b);
      return {{32{1'b0}},a}*{{32{1'b0}},b};
   endfunction // mulu

   function logic [63:0] mulsu
     (input signed [31:0] a,
      input unsigned [31:0] b);
      return {{32{a[31]}},a}*{{32{1'b0}},b};
   endfunction // mulsu

   // MUL instruction
   property mul_inst;
      logic [63:0]          res;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7 == 1))
        ##1 ($signed(alu_src1) > -8 && $signed(alu_src1) < 8 && $signed(alu_src2) > -8 && $signed(alu_src2) < 8,
             res = muls(alu_src1, alu_src2))
                            |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $past(write_data)==res[31:0];
   endproperty
   a_mul_inst: assert property (mul_inst);
   c_mul_inst: cover property (mul_inst);
/*
   property mul_inst_negneg;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7 == 1))
             ##1 ($signed(alu_src1) <= 0 && $signed(alu_src2) <= 0)
                   |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $signed($past(write_data)) >= 0;
   endproperty
   a_mul_inst_negneg: assert property (mul_inst_negneg);
   c_mul_inst_negneg: cover property (mul_inst_negneg);
   property mul_inst_pospos;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7 == 1))
     ##1 ($signed(alu_src1) >= 0 && $signed(alu_src2) >= 0)
       |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $signed($past(write_data)) >= 0;
   endproperty
   a_mul_inst_pospos: assert property (mul_inst_pospos);
   c_mul_inst_pospos: cover property (mul_inst_pospos);
   property mul_inst_negpos;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7 == 1))
     ##1 ($signed(alu_src1) <= 0 && $signed(alu_src2) >= 0)
       |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $signed($past(write_data)) <= 0;
   endproperty
   a_mul_inst_negpos: assert property (mul_inst_negpos);
   c_mul_inst_negpos: cover property (mul_inst_negpos);
   property mul_inst_posneg;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7 == 1))
     ##1 ($signed(alu_src1) >= 0 && $signed(alu_src2) <= 0)
       |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $signed($past(write_data)) <= 0;
   endproperty
   a_mul_inst_posneg: assert property (mul_inst_posneg);
   c_mul_inst_posneg: cover property (mul_inst_posneg);
   property mul_inst_posneg;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==0) and (funct7 == 1))
     ##1 ($signed(alu_src1) >= 0 && $signed(alu_src2) <= 0)
       |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $signed($past(write_data)) <= 0;
   endproperty
   a_mul_inst_posneg: assert property (mul_inst_posneg);
   c_mul_inst_posneg: cover property (mul_inst_posneg);
*/
   // MULH instruction
   property mulh_inst;
      logic [63:0] res;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==1) and (funct7 == 1))
        ##1 ($signed(alu_src1) > -8 && $signed(alu_src1) < 8 && $signed(alu_src2) > -8 && $signed(alu_src2) < 8,
             res = muls(alu_src1, alu_src2))
                   |=> ##[0:33] $fell(alu_wait) && $past(wreg) && $past(write_data)==res[63:32];
   endproperty
   a_mulh_inst: assert property (mulh_inst);
   c_mulh_inst: cover property (mulh_inst);

   // MULHUS instruction
   property mulhsu_inst;
      logic [63:0] res;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==2) and (funct7 == 1))
        ##1 ($signed(alu_src1) > -8 && $signed(alu_src1) < 8 && $signed(alu_src2) > -8 && $signed(alu_src2) < 8,
             res = mulsu(alu_src1, alu_src2))
                   |=> ##[0:33] !alu_wait && $past(wreg) && $past(write_data)==res[63:32];
   endproperty
   a_mulhsu_inst: assert property (mulhsu_inst);
   c_mulhsu_inst: cover property (mulhsu_inst);

   // MULHU instruction
   property mulhu_inst;
      logic [63:0] res;
      (`NOT_MEM_J and `NOT_STALL and (opcode==`REG_INST) and (funct3==3) and (funct7 == 1))
        ##1 ($signed(alu_src1) > -8 && $signed(alu_src1) < 8 && $signed(alu_src2) > -8 && $signed(alu_src2) < 8,
             res = mulu(alu_src1, alu_src2))
                   |=> ##[0:33] !alu_wait && $past(wreg) && $past(write_data)==res[63:32];
   endproperty
   a_mulhu_inst: assert property (mulhu_inst);
   c_mulhu_inst: cover property (mulhu_inst);

endmodule // v_datapath

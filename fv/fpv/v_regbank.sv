module v_regbank
  (input wire clock, reset, wreg,
   input wire [4:0]  read_reg1, read_reg2, write_reg,
   input wire [31:0] read_data1, read_data2, write_data);

   // Default clock edge
   default clocking @(posedge clock); endclocking

   // Default reset state
   default disable iff reset;

   property read1_zero;
     read_reg1 == '0 |-> read_data1 == '0;
   endproperty // read1_zero
   a_read1_zero: assert property(read1_zero);
   c_read1_zero: cover property(read1_zero);

   property read2_zero;
     read_reg2 == '0 |-> read_data2 == '0;
   endproperty // read2_zero
   a_read2_zero: assert property(read2_zero);
   c_read2_zero: cover property(read2_zero);

   property write_read1;
      bit [4:0]      addr;
      bit [31:0]     data;
      (wreg && write_reg, addr = write_reg, data = write_data)
	##1 (!wreg || (write_reg != addr))[*0:$]
	##1 read_reg1 == addr |-> read_data1 == data;
   endproperty // write_read1
   a_write_read1: assert property(write_read1);
   c_write_read1: cover property(write_read1);

   property write_read2;
      bit [4:0]      addr;
      bit [31:0]     data;
      (wreg && write_reg, addr = write_reg, data = write_data)
	##1 (!wreg || (write_reg != addr))[*0:$]
	##1 read_reg2 == addr |-> read_data2 == data;
   endproperty // write_read2
   a_write_read2: assert property(write_read2);
   c_write_read2: cover property(write_read2);
endmodule // v_regbank

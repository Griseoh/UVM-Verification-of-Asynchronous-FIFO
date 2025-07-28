//GLOBAL INCLUDE FILES
`include "uvm_macros.svh"
import uvm_pkg::*;

//SEQUENCE ITEM CLASS -------------------------------------------------------------------------
class sequence_item extends uvm_sequence_item;
  
  rand bit rd_en, wr_en = 1'b0;
  rand bit [7:0] wr_dat = 8'b00000000;
  bit [7:0] rd_dat = 8'b00000000;
  bit full, empty = 1'b0;
  bit overflow, underflow = 1'b0;
  
  function new(string name = "sequence_item");
    super.new(name);
  endfunction
  
  `uvm_object_utils_begin(sequence_item);
  	`uvm_field_int(rd_en, UVM_ALL_ON)
  	`uvm_field_int(wr_en, UVM_ALL_ON)
  	`uvm_field_int(rd_dat, UVM_ALL_ON)
  	`uvm_field_int(wr_dat, UVM_ALL_ON)
  	`uvm_field_int(full, UVM_ALL_ON)
  	`uvm_field_int(empty, UVM_ALL_ON)
  `uvm_object_utils_end
  
  constraint rd_wr_stress0{rd_en == 1; wr_en == 0; wr_dat == 8'b00000000;}
  constraint rd_wr_stress1{rd_en == 0; wr_en == 1; wr_dat >= 8'b00000000; wr_dat <= 8'b11111110;}
  constraint rd_wr_stress2{rd_en == 1; wr_en == 1; wr_dat >= 8'b00000000; wr_dat <= 8'b11111110;}
  
endclass

//SEQUENCE CLASSES -----------------------------------------------------------------------------
class base_stress_seq extends uvm_sequence #(sequence_item);
  
  `uvm_object_utils(base_stress_seq)
  
  typedef enum {READ, WRITE, READWRITE} operation_switch;
  sequence_item seq_item;
  operation_switch swtch;
 
  function new(string name = "base_stress_seq");
    super.new(name);
  endfunction
  
  task body();
    $display("-----------------------------------------------------------------------------------------[SEQ REPORT START]-----------------------------------------------------------------------------------------");
    `uvm_info(get_type_name(), "base_stress_seq : Inside Body Task", UVM_LOW);
    seq_item = sequence_item::type_id::create("seq_item");
    case (swtch)
      READ : begin
        seq_item.rd_wr_stress0.constraint_mode(1);
        seq_item.rd_wr_stress1.constraint_mode(0);
        seq_item.rd_wr_stress2.constraint_mode(0);
      end
      WRITE : begin
        seq_item.rd_wr_stress0.constraint_mode(0);
        seq_item.rd_wr_stress1.constraint_mode(1);
        seq_item.rd_wr_stress2.constraint_mode(0);
      end
      READWRITE : begin
        seq_item.rd_wr_stress0.constraint_mode(0);
        seq_item.rd_wr_stress1.constraint_mode(0);
        seq_item.rd_wr_stress2.constraint_mode(1);
      end
      default : begin
        seq_item.rd_wr_stress0.constraint_mode(0);
        seq_item.rd_wr_stress1.constraint_mode(0);
        seq_item.rd_wr_stress2.constraint_mode(0);
      end
    endcase
    start_item(seq_item);
  	if (!seq_item.randomize()) begin
      `uvm_error(get_type_name(), "Randomization failed in base_stress_seq")
  	end
  	finish_item(seq_item);
    $display("-----------------------------------------------------------------------------------------[SEQ REPORT END]-----------------------------------------------------------------------------------------");
  endtask
  
endclass

//SEQUENCER---------------------------------------------------------------------------------------
class seqcr extends uvm_sequencer #(sequence_item);
  
  `uvm_component_utils(seqcr)
  
  function new(string name = "seqcr", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
  endfunction

endclass

//DRIVER------------------------------------------------------------------------------------------
class driver extends uvm_driver #(sequence_item);
  
  `uvm_component_utils(driver)
  virtual interface fifo_if fif;
  sequence_item seq_item_drv;
  
  function new(string name = "driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction
    
  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if(!uvm_config_db #(virtual fifo_if) :: get(this, "", "fif", fif))
      `uvm_fatal(get_type_name(), "Not Set-up at the Top Level");
  endfunction
  
  task run_phase(uvm_phase phase);
    forever begin
      wait(fif.rst == 0);
      seq_item_port.get_next_item(seq_item_drv);
      if(seq_item_drv)begin
        if(seq_item_drv.wr_en == 1 && seq_item_drv.rd_en == 0)begin
      		@(posedge fif.clk_wr);
       		$display("-----------------------------------------------------------------------------------------[DRIVER WRITE REPORT START]-----------------------------------------------------------------------------------------");
      		`uvm_info(get_type_name(), $sformatf("Read-Enable : %0d, Write-Enable : %0d, Data-Read : %0d, Data-to-Write : %0d, Full-Status : %0d, Empty-Status : %0d", seq_item_drv.rd_en, seq_item_drv.wr_en, seq_item_drv.rd_dat, seq_item_drv.wr_dat, seq_item_drv.full, seq_item_drv.empty), UVM_LOW);
      		fif.rd_en <= seq_item_drv.rd_en;
      		fif.wr_en <= seq_item_drv.wr_en;
      		fif.wr_dat <= seq_item_drv.wr_dat;
          `uvm_info(get_type_name(), $sformatf("Stimulus driven for Write-Enable : %0d, Write-Enable : %0d, Data-to-Write : %0d", fif.rd_en, fif.wr_en, fif.wr_dat), UVM_LOW);
      		$display("-----------------------------------------------------------------------------------------[DRIVER WRITE REPORT END]-----------------------------------------------------------------------------------------");
      		seq_item_port.item_done();
        end
        if(seq_item_drv.wr_en == 0 && seq_item_drv.rd_en == 1)begin
        	@(posedge fif.clk_rd);
       		$display("-----------------------------------------------------------------------------------------[DRIVER READ REPORT START]-----------------------------------------------------------------------------------------");
      		`uvm_info(get_type_name(), $sformatf("Read-Enable : %0d, Write-Enable : %0d, Data-Read : %0d, Data-to-Write : %0d, Full-Status : %0d, Empty-Status : %0d", seq_item_drv.rd_en, seq_item_drv.wr_en, seq_item_drv.rd_dat, seq_item_drv.wr_dat, seq_item_drv.full, seq_item_drv.empty), UVM_LOW);
      		fif.rd_en <= seq_item_drv.rd_en;
      		fif.wr_en <= seq_item_drv.wr_en;
          `uvm_info(get_type_name(), $sformatf("Stimulus Driven for Read-Enable : %0d, Write-Enable : %0d, Data-to-Write : %0d", fif.rd_en, fif.wr_en, fif.wr_dat), UVM_LOW);
      		$display("-----------------------------------------------------------------------------------------[DRIVER READ REPORT END]-----------------------------------------------------------------------------------------");
      	 	seq_item_port.item_done();
    	end
        if(seq_item_drv.wr_en == 1 && seq_item_drv.rd_en == 1)begin
          @(posedge fif.clk_wr);
          $display("-----------------------------------------------------------------------------------------[DRIVER READWRITE REPORT START]-----------------------------------------------------------------------------------------");
      		`uvm_info(get_type_name(), $sformatf("Read-Enable : %0d, Write-Enable : %0d, Data-Read : %0d, Data-to-Write : %0d, Full-Status : %0d, Empty-Status : %0d", seq_item_drv.rd_en, seq_item_drv.wr_en, seq_item_drv.rd_dat, seq_item_drv.wr_dat, seq_item_drv.full, seq_item_drv.empty), UVM_LOW);
          fif.wr_en <= seq_item_drv.wr_en;
          fif.wr_dat <= seq_item_drv.wr_dat;
          fif.rd_en <= seq_item_drv.rd_en;
          `uvm_info(get_type_name(), $sformatf("Stimulus Driven for Read-Write-Enable : %0d, Write-Enable : %0d, Data-to-Write : %0d", fif.rd_en, fif.wr_en, fif.wr_dat), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[DRIVER READWRITE REPORT END]-----------------------------------------------------------------------------------------");
      	 	seq_item_port.item_done();
        end
      end
    end
  endtask
endclass

//MONITOR----------------------------------------------------------------------------------------
class monitor extends uvm_monitor;
  
  `uvm_component_utils(monitor)
  
  virtual interface fifo_if fif;
  uvm_analysis_port #(sequence_item) item_collect_port;
  
  function new(string name = "monitor", uvm_component parent = null);
    super.new(name, parent);
    item_collect_port = new("item_collect_port", this);
  endfunction
    
    function void build_phase(uvm_phase phase);
      super.build_phase(phase);
      if(!uvm_config_db #(virtual fifo_if) :: get(this, "", "fif", fif))
        `uvm_fatal(get_type_name(), "Not Set-up at the Top Level");
    endfunction
    
    function void sample_and_send();
      sequence_item seq_item_mon;
      seq_item_mon = sequence_item::type_id::create("seq_item_mon");
      seq_item_mon.wr_en = fif.wr_en;
      seq_item_mon.wr_dat = fif.wr_dat;
      seq_item_mon.full = fif.full;
      seq_item_mon.overflow = fif.overflow;
      seq_item_mon.rd_en = fif.rd_en;
      seq_item_mon.rd_dat = fif.rd_dat;
      seq_item_mon.empty = fif.empty;
      seq_item_mon.underflow = fif.underflow;
      item_collect_port.write(seq_item_mon);
    endfunction
    
    virtual task run_phase(uvm_phase phase);
      fork
        forever begin
          @(posedge fif.clk_wr)
          if(!fif.rst && fif.wr_en)
            sample_and_send();
        end
        forever begin
          @(posedge fif.clk_rd)
          if(!fif.rst && fif.rd_en)
            sample_and_send();
        end
      join_none
    endtask
  
endclass

//AGENT------------------------------------------------------------------------------------------
class agent extends uvm_agent;
  `uvm_component_utils(agent)
  driver drv;
  monitor mon;
  seqcr sqr;
  
  function new(string name = "agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if(get_is_active() == UVM_ACTIVE)begin
      drv = driver::type_id::create("drv", this);
      sqr = seqcr::type_id::create("sqr", this);
    end
    mon = monitor::type_id::create("mon", this);
  endfunction
  
  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    if(get_is_active() == UVM_ACTIVE)begin
      drv.seq_item_port.connect(sqr.seq_item_export);
    end
  endfunction
endclass
    
//SCOREBOARD------------------------------------------------------------------------------------
class scoreboard extends uvm_scoreboard;
  `uvm_component_utils(scoreboard)
  uvm_analysis_imp #(sequence_item, scoreboard) item_collection_export;
  
  typedef struct {
    bit [7:0] expected_data;
    time time_stamp;
  }latent_rd_chk;
  latent_rd_chk rd_chk_queue[$];
  sequence_item item_queue[$];
  bit [7:0] golden_fifo[$];
  
  function new(string name = "scoreboard", uvm_component parent = null);
    super.new(name, parent);
    item_collection_export = new("item_collection_export", this);
  endfunction
  
  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
  endfunction
  
  function void write(sequence_item seq_item);
    item_queue.push_back(seq_item);
  endfunction
 
  task run_phase(uvm_phase phase);
    sequence_item sco_item;
    bit [7:0] rd_data_pipe[$];
    forever begin
      wait(item_queue.size)
      if(item_queue.size() > 0)begin
      	sco_item = item_queue.pop_front();
      	$display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT START]-----------------------------------------------------------------------------------------");
      	`uvm_info(get_type_name(), $sformatf("Checking : time = %0t, wr_en = %0d, wr_dat = %0d, rd_en = %0d, rd_dat = %0d, full = %0d, empty = %0d, overflow = %0d, underflow = %0d", $time, sco_item.wr_en, sco_item.wr_dat, sco_item.rd_en, sco_item.rd_dat, sco_item.full, sco_item.empty, sco_item.overflow, sco_item.underflow), UVM_LOW);
      	//WRITE TRANSACTION-------------------------------------------------------------------------------------------------
      	if(sco_item.wr_en && sco_item.full)begin
          `uvm_error(get_type_name(), $sformatf("WRITE ATTEMPTED BUT Golden FIFO is FULL ! : full = %0d", sco_item.full));
          `uvm_info(get_type_name(), $sformatf("Time is = %0t and Values in the Queue : GOLDEN FIFO = %0p", $time, golden_fifo), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT END]-----------------------------------------------------------------------------------------");
      	end
      	else if(sco_item.wr_en && !sco_item.full)begin
          golden_fifo.push_back(sco_item.wr_dat);
          `uvm_info(get_type_name(), $sformatf("Golden FIFO PUSH (WRITE ATTEMPT): Value = %0d", sco_item.wr_dat), UVM_LOW);
          `uvm_info(get_type_name(), $sformatf("Time is = %0t and Values in the Queue : GOLDEN FIFO = %0p, RD_CHK_QUEUE = %0p, RD_PIPE_PUSHBACK = %0p", $time, golden_fifo, rd_chk_queue, rd_data_pipe), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT END]-----------------------------------------------------------------------------------------");
      	end
      	//READ TRANSACTION--------------------------------------------------------------------------------------------------
      	if(sco_item.rd_en && sco_item.empty)begin
          `uvm_error(get_type_name(),$sformatf("READ ATTEMPTED BUT Golden FIFO is EMPTY ! : empty = %0d", sco_item.empty));
          `uvm_info(get_type_name(), $sformatf("Time is = %0t and Values in the Queue : GOLDEN FIFO = %0p", $time, golden_fifo), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT END]-----------------------------------------------------------------------------------------");
      	end
      	else if(sco_item.rd_en && !sco_item.empty) begin
          latent_rd_chk l_rd_chk;
          l_rd_chk.expected_data = golden_fifo.pop_front();
          l_rd_chk.time_stamp = $time;
          rd_chk_queue.push_back(l_rd_chk);
          `uvm_info(get_type_name(), $sformatf("Golden FIFO READ DEFERRED : Will Check Value = %0d at the next cycle !", l_rd_chk.expected_data), UVM_LOW);
          rd_data_pipe.push_back(rd_chk_queue[0].expected_data);
          `uvm_info(get_type_name(), $sformatf("Time is = %0t and Values in the Queue : GOLDEN FIFO = %0p, RD_CHK_QUEUE = %0p, RD_PIPE_PUSHBACK = %0p", $time, golden_fifo, rd_chk_queue, rd_data_pipe), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT END]-----------------------------------------------------------------------------------------");
      	end
      end
      if(rd_data_pipe.size() >=1 && rd_chk_queue.size() > 0)begin
        bit [7:0] delayed_rd_dat = rd_data_pipe.pop_front();
        latent_rd_chk rd_chk_item = rd_chk_queue.pop_front();
        if(delayed_rd_dat != rd_chk_item.expected_data)begin
          `uvm_error(get_type_name(), $sformatf("Golden FIFO POP (READ ATTEMPT): DATA MISMATCH: time = %0t, Expected = %0d, Got = %0d", $time, rd_chk_item.expected_data, delayed_rd_dat));
          `uvm_info(get_type_name(), $sformatf("Time is = %0t and Values in the Queue : GOLDEN FIFO = %0p", $time, golden_fifo), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT END]-----------------------------------------------------------------------------------------");
       	end
       	else begin
          `uvm_info(get_type_name(), $sformatf("Golden FIFO POP (READ ATTEMPT): DATA MATCH: time = %0t, Expected = %0d, Got = %0d", $time, rd_chk_item.expected_data, delayed_rd_dat), UVM_LOW);
          `uvm_info(get_type_name(), $sformatf("Time is = %0t and Values in the Queue : GOLDEN FIFO = %0p", $time, golden_fifo), UVM_LOW);
          $display("-----------------------------------------------------------------------------------------[SCOREBOARD REPORT END]-----------------------------------------------------------------------------------------");
        end
      end
    end
  endtask
endclass
    
//ENVIRONMENT--------------------------------------------------------------------------------
class env extends uvm_env;
  `uvm_component_utils(env)
  agent agt;
  scoreboard sb;

  function new(string name = "env", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    agt = agent::type_id::create("agt", this);
    sb = scoreboard::type_id::create("sb", this);
  endfunction
  
  function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    agt.mon.item_collect_port.connect(sb.item_collection_export);
  endfunction
endclass

//TEST---------------------------------------------------------------------------------------
class base_test extends uvm_test;
  `uvm_component_utils(base_test)
  env env_o;
  base_stress_seq bseq;
  
  function new(string name = "base_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    env_o = env::type_id::create("env_o", this);
  endfunction
  
  virtual task run_phase(uvm_phase phase);
    `uvm_info(get_type_name(), "Test Task To Be Overridden", UVM_LOW);
  endtask
endclass
    
class unified_stress_test extends base_test;
  
  `uvm_component_utils(unified_stress_test)
  
  function new(string name = "unified_stress_test", uvm_component parent = null);
    super.new(name, parent);
  endfunction
  
  task run_write_stress_test(int count);
    `uvm_info(get_type_name(), "Running WRITE STRESS TEST SEQ", UVM_MEDIUM);
    repeat(count)begin
      #1;
      bseq = base_stress_seq::type_id::create("bseq");
      bseq.swtch = base_stress_seq::WRITE;
      bseq.start(env_o.agt.sqr);
    end
  endtask
  
  task run_read_stress_test(int count);
    `uvm_info(get_type_name(), "Running READ STRESS TEST SEQ", UVM_MEDIUM);
    repeat(count)begin
      #1;
      bseq = base_stress_seq::type_id::create("bseq");
      bseq.swtch = base_stress_seq::READ;
      bseq.start(env_o.agt.sqr);
    end
  endtask
  
  task run_readwrite_stress_test(int count);
    `uvm_info(get_type_name(), "Running READWRITE STRESS TEST SEQ", UVM_MEDIUM);
    repeat(count)begin
      #1;
      bseq = base_stress_seq::type_id::create("bseq");
      bseq.swtch = base_stress_seq::READWRITE;
      bseq.start(env_o.agt.sqr);
    end
  endtask

  task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    run_write_stress_test(20);
    run_read_stress_test(20);
    run_write_stress_test(5);
    run_readwrite_stress_test(20);
    phase.drop_objection(this);
    `uvm_info(get_type_name(), "End of Testcase", UVM_LOW);
  endtask
endclass

//TB_TOP-------------------------------------------------------------------------------------
module tB_top;
  bit clk_rd;
  bit clk_wr;
  bit rst;
  bit underflow;
  bit overflow;
  
  always #2 clk_wr = ~clk_wr;
  initial begin
    clk_rd = 0;
  	forever begin 
    	#2 clk_rd = 1;
    	#4 clk_rd = 0;
    	#2;
  	end
  end
  initial begin
    clk_wr = 0;
    rst = 1;
    repeat(8)@(posedge fif.clk_wr);
    rst = 0;
  end
  
  fifo_if fif(clk_wr, clk_rd, rst);
  asynch_fifo DUT(.clk_wr(fif.clk_wr), .clk_rd(fif.clk_rd), .rst(fif.rst), .rd_en(fif.rd_en), .rd_dat(fif.rd_dat), .wr_en(fif.wr_en), .wr_dat(fif.wr_dat), .empty(fif.empty), .full(fif.full), .underflow(fif.underflow), .overflow(fif.overflow));
  
  initial begin
    uvm_config_db#(virtual fifo_if) :: set(uvm_root :: get(), "*", "fif", fif);
    $dumpfile("dump.vcd");
    $dumpvars(0);
  end
  
  initial begin
    run_test("unified_stress_test");
  end
endmodule

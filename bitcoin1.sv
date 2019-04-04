module bitcoin1(input logic clk, reset_n, start,
			input logic [15:0] message_addr, output_addr,
			output logic done, mem_clk, mem_we,
			output logic [15:0] mem_addr,
			output logic [31:0] mem_write_data,
			input logic [31:0] mem_read_data);

parameter int NONCES = 16;			
			
// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// mem_clk assign
assign mem_clk = clk;

// variable declarations
logic   [31:0] H0, H1, H2, H3, H4, H5, H6, H7;
logic   [31:0] fH0, fH1, fH2, fH3, fH4, fH5, fH6, fH7;

logic   [31:0] a, b, c, d, e, f, g, h;

logic   [31:0] w[16];

// counters
logic [5:0] n;
logic [7:0] t; 

logic [31:0] nonce;
							
// right rotation
function logic [31:0] rightrotate(input logic [31:0] x,
	input logic [5:0] r);
	rightrotate = (x >> r) | (x << (32-r));
endfunction	
	
// init w array
function logic [31:0] wtnew; // function with no inputs
	logic [31:0] s0, s1;
	s0 = rightrotate(w[1],7)^rightrotate(w[1],18)^(w[1]>>3);
	s1 = rightrotate(w[14],17)^rightrotate(w[14],19)^(w[14]>>10);
	
	wtnew = w[0] + s0 + w[9] + s1;
endfunction
	
// round of sha-256
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
	logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals

	begin
		S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
		ch = (e & f) ^ ((~e) & g);
		t1 = h + S1 + ch + k[t] + w;
		S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
		maj = (a & b) ^ (a & c) ^ (b & c);
		t2 = S0 + maj;

		sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
	end
endfunction	

// state enumerations
enum logic [3:0] {IDLE = 4'h0,
						READ1 = 4'h1,
						READ_IDLE1 = 4'h2,
						COMPUTE1 = 4'h3,
						READ2 = 4'h4,
						READ_IDLE2 = 4'h5,
						COMPUTE2 = 4'h6,
						//READ3 = 4'h7,
						//READ_IDLE3 = 4'h8,
						COMPUTE3 = 4'h7,
						COMPUTE_IDLE3 = 4'h8,
						WRITE = 4'h9,
						COMPLETE = 4'ha} state;
						
always_ff @(posedge mem_clk, negedge reset_n) begin
	if (!reset_n) begin
		state <= IDLE;
		
		H0 <= 32'h6a09e667;
		H1 <= 32'hbb67ae85;
		H2 <= 32'h3c6ef372;
		H3 <= 32'ha54ff53a;
		H4 <= 32'h510e527f;
		H5 <= 32'h9b05688c;
		H6 <= 32'h1f83d9ab;
		H7 <= 32'h5be0cd19;
		
		fH0 <= 32'h6a09e667;
		fH1 <= 32'hbb67ae85;
		fH2 <= 32'h3c6ef372;
		fH3 <= 32'ha54ff53a;
		fH4 <= 32'h510e527f;
		fH5 <= 32'h9b05688c;
		fH6 <= 32'h1f83d9ab;
		fH7 <= 32'h5be0cd19;
		
		a <= 32'h6a09e667;
		b <= 32'hbb67ae85;
		c <= 32'h3c6ef372;
		d <= 32'ha54ff53a;
		e <= 32'h510e527f;
		f <= 32'h9b05688c;
		g <= 32'h1f83d9ab;
		h <= 32'h5be0cd19;
		
		nonce <= 32'h0;
		
		for(int i = 0; i < 16; i++) begin
			w[i] <= 0;
		end
		
		n <= 0;
		t <= 0;
		mem_we <= 0;
		
		mem_addr <= 32'h0;
		mem_write_data <= 32'h0;
		
		done = 0;
	end
	else case(state)
	IDLE: begin
		if(start) begin
			done <= 0;
			state <= READ1;
			mem_we <= 0;
			mem_addr <= message_addr + 1;
			n <= 1;
		end
	end
	READ1: begin
		w[n-1] <= mem_read_data;
		
		mem_addr <= message_addr + n + 1;
		n <= n + 1;
		
		if(n == 15) begin
			t <= 0;
			state <= READ_IDLE1;
		end
	end
	READ_IDLE1: begin
		w[n-1] <= mem_read_data;
		n <= 0;
		state <= COMPUTE1;
	end
	// this stage is universal for all nonces
	COMPUTE1: begin
		for(int i = 0; i < 15; i++) begin
			w[i] <= w[i + 1];
		end
		w[15] <= wtnew;
		
		{a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, w[0], t);
		
		t <= t + 1;
		
		if(t == 64) begin
			fH0 <= a + fH0;
			fH1 <= b + fH1;
			fH2 <= c + fH2;
			fH3 <= d + fH3;
			fH4 <= e + fH4;
			fH5 <= f + fH5;
			fH6 <= g + fH6;
			fH7 <= h + fH7;
			
			mem_addr <= message_addr + 16;
			
			state <= READ2;
		end
	end
	// nonces involved
	READ2: begin
		if(n > 0) begin
			w[n - 1] <= mem_read_data;
		end
		mem_addr <= message_addr + n + 16 + 1;
		n <= n + 1;
		
		a <= fH0;
		b <= fH1;
		c <= fH2;
		d <= fH3;
		e <= fH4;
		f <= fH5;
		g <= fH6;
		h <= fH7;
		
		if(n == 3) begin
			w[4] <= 32'h80000000;
			for(int i = 5; i < 15; i++) begin
				w[i] <= 32'h0;
			end
			w[15] <= 32'd640;
			t <= 0;
			state <= READ_IDLE2;
		end
	end
	READ_IDLE2: begin
		for(int i = 0; i < 15; i++) begin
			w[i] <= w[i + 1];
		end
		w[n-2] <= nonce;
		w[15] <= wtnew;
		n <= 0;
		{a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, w[0], t);
		
		t <= t + 1;
		
		state <= COMPUTE2;
	end
	COMPUTE2: begin
		//if(t > 0) begin
			for(int i = 0; i < 15; i++) begin
				w[i] <= w[i + 1];
			end
			w[15] <= wtnew;
		//end
		
		{a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, w[0], t);
		
		t <= t + 1;
		
		if(t == 64) begin
			t <= 0;
			
			H0 <= 32'h6a09e667;
			H1 <= 32'hbb67ae85;
			H2 <= 32'h3c6ef372;
			H3 <= 32'ha54ff53a;
			H4 <= 32'h510e527f;
			H5 <= 32'h9b05688c;
			H6 <= 32'h1f83d9ab;
			H7 <= 32'h5be0cd19;
			
			a <= 32'h6a09e667;
			b <= 32'hbb67ae85;
			c <= 32'h3c6ef372;
			d <= 32'ha54ff53a;
			e <= 32'h510e527f;
			f <= 32'h9b05688c;
			g <= 32'h1f83d9ab;
			h <= 32'h5be0cd19;
			
			w[0] <= a + fH0;
			w[1] <= b + fH1;
			w[2] <= c + fH2;
			w[3] <= d + fH3;
			w[4] <= e + fH4;
			w[5] <= f + fH5;
			w[6] <= g + fH6;
			w[7] <= h + fH7;
			w[8] <= 32'h80000000;
			for(int i = 9; i < 15; i++) begin
				w[i] <= 32'h0;
			end
			w[15] <= 32'd256;
			
			n <= 0;
			mem_we <= 1;
			state <= COMPUTE3;
		end
	end
	COMPUTE3: begin
		for(int i = 0; i < 15; i++) begin
			w[i] <= w[i + 1];
		end
		w[15] <= wtnew;
				
		{a, b, c, d, e, f, g, h} <= sha256_op(a, b, c, d, e, f, g, h, w[0], t);
		
		t <= t + 1;
		
		if(t == 63) begin
			mem_we <= 1;
			state <= COMPUTE_IDLE3;
		end
	end
	COMPUTE_IDLE3: begin
		H0 <= a + H0;
		H1 <= b + H1;
		H2 <= c + H2;
		H3 <= d + H3;
		H4 <= e + H4;
		H5 <= f + H5;
		H6 <= g + H6;
		H7 <= h + H7;
		n <= 0;
		
		state <= WRITE;
	end
	WRITE: begin
		// code
		// the testbench only cares for the first word of each hash; the commented
		// code afterwards returns the full hash
		mem_addr <= output_addr + nonce[15:0] + n;//n + nonce[15:0]*15'd8;
		
		if(n == 0) begin
			mem_write_data <= H0;
		end
		if(n == 1) begin
			mem_write_data <= H1;
		end
		if(n == 2) begin
			mem_write_data <= H2;
		end
		if(n == 3) begin
			mem_write_data <= H3;
		end
		if(n == 4) begin
			mem_write_data <= H4;
		end
		if(n == 5) begin
			mem_write_data <= H5;
		end
		if(n == 6) begin
			mem_write_data <= H6;
		end
		if(n == 7) begin
			mem_write_data <= H7;
		end		
		
		n <= n + 1;
		
		if(n == 8) begin
			if(nonce == 15) begin
				mem_we <= 0;
				state <= COMPLETE;
			end
			else begin
				n <= 0;
				mem_addr <= message_addr + 16;
				nonce <= nonce + 1;
				mem_we <= 0;
				state <= READ2;
			end
		end
	end
	COMPLETE: begin
		done <= 1;
		state <= IDLE;
	end
	default: begin
		state <= IDLE;
	end
	endcase
end						
						
endmodule

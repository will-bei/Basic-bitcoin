module bitcoin2(input logic clk, reset_n, start,
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

// stores output result
logic   [31:0] H0[NONCES], H1[NONCES], H2[NONCES], H3[NONCES], H4[NONCES], H5[NONCES], H6[NONCES], H7[NONCES];
// stores first hash results
logic   [31:0] fH0, fH1, fH2, fH3, fH4, fH5, fH6, fH7;

// stores output results
logic   [31:0] a[NONCES], b[NONCES], c[NONCES], d[NONCES], e[NONCES], f[NONCES], g[NONCES], h[NONCES];

//logic   [31:0] w[16];

logic	  [31:0] w[NONCES][16];

// counters
//logic [2:0] m;
logic [5:0] n;
logic [7:0] t; 

logic [31:0] nonce;
							
// right rotation
function logic [31:0] rightrotate(input logic [31:0] x,
	input logic [5:0] r);
	rightrotate = (x >> r) | (x << (32-r));
endfunction	
	
// init w array
function logic [31:0] wtnew(int i); // function with no inputs
	logic [31:0] s0, s1;
	s0 = rightrotate(w[i][1],7)^rightrotate(w[i][1],18)^(w[i][1]>>3);
	s1 = rightrotate(w[i][14],17)^rightrotate(w[i][14],19)^(w[i][14]>>10);
	
	wtnew = w[i][0] + s0 + w[i][9] + s1;
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
		
		for(int i = 0; i < NONCES; i++) begin
			H0[i] <= 32'h6a09e667;
			H1[i] <= 32'hbb67ae85;
			H2[i] <= 32'h3c6ef372;
			H3[i] <= 32'ha54ff53a;
			H4[i] <= 32'h510e527f;
			H5[i] <= 32'h9b05688c;
			H6[i] <= 32'h1f83d9ab;
			H7[i] <= 32'h5be0cd19;
		
			a[i] <= 32'h6a09e667;
			b[i] <= 32'hbb67ae85;
			c[i] <= 32'h3c6ef372;
			d[i] <= 32'ha54ff53a;
			e[i] <= 32'h510e527f;
			f[i] <= 32'h9b05688c;
			g[i] <= 32'h1f83d9ab;
			h[i] <= 32'h5be0cd19;
			
			for(int j = 0; j < 16; j++) begin
				w[i][j] <= 0;
			end
		end
		
		fH0 <= 32'h6a09e667;
		fH1 <= 32'hbb67ae85;
		fH2 <= 32'h3c6ef372;
		fH3 <= 32'ha54ff53a;
		fH4 <= 32'h510e527f;
		fH5 <= 32'h9b05688c;
		fH6 <= 32'h1f83d9ab;
		fH7 <= 32'h5be0cd19;
	
		nonce <= 32'h0;
		
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
		for(int i = 0; i < NONCES; i++) begin
			w[i][n-1] <= mem_read_data;
		end
		
		mem_addr <= message_addr + n + 1;
		n <= n + 1;
		
		if(n == 15) begin
			t <= 0;
			state <= READ_IDLE1;
		end
	end
	READ_IDLE1: begin
		for(int i = 0; i < NONCES; i++) begin
			w[i][n-1] <= mem_read_data;
		end
		n <= 0;
		state <= COMPUTE1;
	end
	// this stage is universal for all nonces
	// hence, to reduce logic, only [0] will be operated on, and other
	// nonces will share the shae fH0 - fH7
	COMPUTE1: begin
		for(int i = 0; i < 15; i++) begin
			w[0][i] <= w[0][i + 1];
		end
		w[0][15] <= wtnew(0);
		
		{a[0], b[0], c[0], d[0], e[0], f[0], g[0], h[0]} 
			<= sha256_op(a[0], b[0], c[0], d[0], e[0], f[0], g[0], h[0], w[0][0], t);
		
		t <= t + 1;
		
		if(t == 64) begin
			fH0 <= a[0] + fH0;
			fH1 <= b[0] + fH1;
			fH2 <= c[0] + fH2;
			fH3 <= d[0] + fH3;
			fH4 <= e[0] + fH4;
			fH5 <= f[0] + fH5;
			fH6 <= g[0] + fH6;
			fH7 <= h[0] + fH7;
			
			mem_addr <= message_addr + 16;
			
			state <= READ2;
		end
	end
	
	// nonces involved
	READ2: begin
		for(int i = 0; i < NONCES; i++) begin
			if(n > 0) begin
				w[i][n - 1] <= mem_read_data;
			end
			mem_addr <= message_addr + n + 16 + 1;
			n <= n + 1;
		
			a[i] <= fH0;
			b[i] <= fH1;
			c[i] <= fH2;
			d[i] <= fH3;
			e[i] <= fH4;
			f[i] <= fH5;
			g[i] <= fH6;
			h[i] <= fH7;
		
			if(n == 3) begin
				w[i][4] <= 32'h80000000;
				for(int j = 5; j < 15; j++) begin
					w[i][j] <= 32'h0;
				end
				w[i][15] <= 32'd640;
				t <= 0;
				state <= READ_IDLE2;
			end
		end
	end
	READ_IDLE2: begin
		for(int i = 0; i < NONCES; i++) begin
			for(int j = 0; j < 15; j++) begin
				w[i][j] <= w[i][j + 1];
			end
			w[i][n-2] <= 32'(i);
			w[i][15] <= wtnew(i);
			n <= 0;
			{a[i], b[i], c[i], d[i], e[i], f[i], g[i], h[i]} 
				<= sha256_op(a[i], b[i], c[i], d[i], e[i], f[i], g[i], h[i], w[i][0], t);
		
			t <= t + 1;
		end
		state <= COMPUTE2;
	end
	COMPUTE2: begin
		//if(t > 0) begin
		for(int i = 0; i < NONCES; i++) begin
			for(int j = 0; j < 15; j++) begin
				w[i][j] <= w[i][j + 1];
			end
			w[i][15] <= wtnew(i);
		//end
		
			{a[i], b[i], c[i], d[i], e[i], f[i], g[i], h[i]} 
				<= sha256_op(a[i], b[i], c[i], d[i], e[i], f[i], g[i], h[i], w[i][0], t);
				
			t <= t + 1;
		
			if(t == 64) begin
				t <= 0;
			
				H0[i] <= 32'h6a09e667;
				H1[i] <= 32'hbb67ae85;
				H2[i] <= 32'h3c6ef372;
				H3[i] <= 32'ha54ff53a;
				H4[i] <= 32'h510e527f;
				H5[i] <= 32'h9b05688c;
				H6[i] <= 32'h1f83d9ab;
				H7[i] <= 32'h5be0cd19;
			
				a[i] <= 32'h6a09e667;
				b[i] <= 32'hbb67ae85;
				c[i] <= 32'h3c6ef372;
				d[i] <= 32'ha54ff53a;
				e[i] <= 32'h510e527f;
				f[i] <= 32'h9b05688c;
				g[i] <= 32'h1f83d9ab;
				h[i] <= 32'h5be0cd19;
			
				w[i][0] <= a[i] + fH0;
				w[i][1] <= b[i] + fH1;
				w[i][2] <= c[i] + fH2;
				w[i][3] <= d[i] + fH3;
				w[i][4] <= e[i] + fH4;
				w[i][5] <= f[i] + fH5;
				w[i][6] <= g[i] + fH6;
				w[i][7] <= h[i] + fH7;
				w[i][8] <= 32'h80000000;
				for(int j = 9; j < 15; j++) begin
					w[i][j] <= 32'h0;
				end
				w[i][15] <= 32'd256;
			
				n <= 0;
				mem_we <= 1;
				
				state <= COMPUTE3;
			end
		end
	end
	COMPUTE3: begin
		for(int i = 0; i < NONCES; i++) begin
			for(int j = 0; j < 15; j++) begin
				w[i][j] <= w[i][j + 1];
			end
			w[i][15] <= wtnew(i);
				
			{a[i], b[i], c[i], d[i], e[i], f[i], g[i], h[i]} 
				<= sha256_op(a[i], b[i], c[i], d[i], e[i], f[i], g[i], h[i], w[i][0], t);
		
			t <= t + 1;
		
			if(t == 63) begin
				mem_we <= 1;
				state <= COMPUTE_IDLE3;
			end
		end
	end
	COMPUTE_IDLE3: begin
		for(int i = 0; i < NONCES; i++) begin
			H0[i] <= a[i] + H0[i];
			H1[i] <= b[i] + H1[i];
			H2[i] <= c[i] + H2[i];
			H3[i] <= d[i] + H3[i];
			H4[i] <= e[i] + H4[i];
			H5[i] <= f[i] + H5[i];
			H6[i] <= g[i] + H6[i];
			H7[i] <= h[i] + H7[i];
		end
		
		n <= 0;
		state <= WRITE;
	end
	WRITE: begin
		// this output is for testbench only; only first words of the hashes
		// are covered
		if(n < NONCES) begin
			mem_addr <= output_addr + n; // + nonce[15:0] + n;
		
			mem_write_data <= H0[n];
		
			n <= n + 1;
		end
		if(n == NONCES) begin
			state <= COMPLETE;
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

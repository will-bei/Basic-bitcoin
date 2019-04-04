This repo contains four files for bitcoin hashing. The code is heavily based off of
files submitted for Prof. Bill Lin's 2019 Winter Quarter ECE 111 final project. 
bitcoin1.sv is the code for a minimum area*delay submission, while bitcoin2.sv and 
the code for a min delay only design.

The respective testbenches, tb_bitcoin1_hash.sv and tb_bitcoin2_hash.sv, are slightly-
modified from a testbench originally written by Prof. Bill Lin.

Specifically, bitcoin1.sv contains the code for a sequential bitcoin hash. Nonces are
processed one-by-one.

bitcoin2.sv contains the code for parallel hash computation. 16 sha-256 processes are
simultaneously run to calculate 16 nonces at once.

Folders contain fitter and static timing analysis reports for the two designs.

bitcoin1.sv completes the testbench in 2373 cycles.

bitcoin2.sv completes the testbench in 236 cycles.

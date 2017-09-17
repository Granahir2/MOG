# MOG INSTRUCTION SET


The mog instruction set is split between 16 groups of 4 opcodes, each opcode having 4 modes depending on the last 2 bits :


	Double Constant (flag 00) : The operands are passed as-is to the opcode
	Right  Constant (flag 10) : The left operand is replaced by the register corresponding to it's ID, the right operand is passed as-is
	Left   Constant (flag 01) : The right operand is replaced by the register corresponding to it's ID, the left operand is passed as-is
	No     Constant (flag 11) : All the operands are replaced by the register corresponding to their IDs


The rightmost operand is the destination and thus, is not affected by the mode.
The NOPs of group 0x0 are implementation defined and can change between hardwares. It's not recommended to use them, except when portability is not an issue.


##	MEMORY (group 0x1)
		LD     $p $b %r		0b00010000 0b0000RRRR 0bPPPPPPPP 0bBBBBBBBB	(r) = ( p  << 8 |  b )
		LD     %p $b %r		0b00010010 0b0000RRRR 0b0000PPPP 0bBBBBBBBB	(r) = ((p) << 8 |  b )
		LD     $p %b %r		0b00010001 0b0000RRRR 0bPPPPPPPP 0b0000BBBB	(r) = ( p  << 8 | (b))
		LD     %p %b %r		0b00010011 0b0000RRRR 0b0000PPPP 0b0000BBBB	(r) = ((p) << 8 | (b))

		ST0     $p $b		0b00010100 0b0000RRRR 0bPPPPPPPP 0bBBBBBBBB	( p  << 8 |  b ) = (0)
		ST0     %p $b		0b00010110 0b0000RRRR 0b0000PPPP 0bBBBBBBBB	((p) << 8 |  b ) = (0)
		ST0     $p %b		0b00010101 0b0000RRRR 0bPPPPPPPP 0b0000BBBB	( p  << 8 | (b)) = (0)
		ST0     %p %b		0b00010111 0b0000RRRR 0b0000PPPP 0b0000BBBB	((p) << 8 | (b)) = (0)

		SB     $b		0b00011000 0b00000000 0bBBBBBBBB 0b00000000	sb =  b
		SB     %b		0b00011010 0b00000000 0b0000RRRR 0b00000000	sb = (b)

		MOV    $c %r		0b00011100 0b0000RRRR 0bCCCCCCCC 0b00000000	(r) =  c 
		MOV    %c %r		0b00011110 0b0000RRRR 0b0000CCCC 0b00000000	(r) = (c)


##	CONTROL FLOW (group 0x2)
		JMP    label		0b00100000 0b00000000 0bPPPPPPPP 0bBBBBBBBB	pc = [ p  << 8 |  b ] 
		JMP    %p $b		0b00100010 0b00000000 0b0000PPPP 0bBBBBBBBB	pc = [(p) << 8 |  b ]
		JMP    $p %b		0b00100001 0b00000000 0bPPPPPPPP 0b0000BBBB	pc = [ p  << 8 | (b)]
		JMP    %p %b		0b00100011 0b00000000 0b0000PPPP 0b0000BBBB	pc = [(p) << 8 | (b)]

		JCC    label		0b00100100 0b0000CCCC 0bPPPPPPPP 0bBBBBBBBB	pc = Fc != 0 ? [ p  << 8 |  b ] : pc
		JCC    %p $b		0b00100110 0b0000CCCC 0b0000PPPP 0bBBBBBBBB	pc = Fc != 0 ? [(p) << 8 |  b ] : pc
		JCC    $p %b		0b00100101 0b0000CCCC 0bPPPPPPPP 0b0000BBBB	pc = Fc != 0 ? [ p  << 8 | (b)] : pc
		JCC    %p %b		0b00100111 0b0000CCCC 0b0000PPPP 0b0000BBBB	pc = Fc != 0 ? [(p) << 8 | (b)] : pc

		JSR    label		0b00101000 0b0000CCCC 0bPPPPPPPP 0bBBBBBBBB	pc = Fc != 0 ? {push pc; jmp label;} : pc
		JSR    %p $b		0b00101010 0b0000CCCC 0b0000PPPP 0bBBBBBBBB	pc = Fc != 0 ? {push pc; jmp [(p) << 8 |  b ];} : pc
		JSR    $p %p		0b00101001 0b0000CCCC 0bPPPPPPPP 0b0000BBBB	pc = Fc != 0 ? {push pc; jmp [ p  << 8 | (b)];} : pc
		JSR    %p %b		0b00101011 0b0000CCCC 0b0000PPPP 0b0000BBBB	pc = Fc != 0 ? {push pc; jmp [(p) << 8 | (b)];} : pc

		RET			0b00101100 0b00000000 0b00000000 0b00000000	pc = pop;


##	UNSIGNED COMPARISON (group 0x3)
		TLT    $a $b %r		0b00110000 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) =  a  <  b  ? 1 : 0
		TLT    %a $b %r		0b00110010 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = (a) <  b  ? 1 : 0
		TLT    $a %b %r		0b00110001 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) =  a  < (b) ? 1 : 0
		TLT    %a %b %r		0b00110011 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = (a) < (b) ? 1 : 0

		TGT    $a $b %r		0b00110100 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) =  a  >  b  ? 1 : 0
		TGT    %a $b %r		0b00110110 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = (a) >  b  ? 1 : 0
		TGT    $a %b %r		0b00110101 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) =  a  > (b) ? 1 : 0
		TGT    %a %b %r		0b00110111 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = (a) > (b) ? 1 : 0

		TEQ    $a $b %r		0b00111000 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) =  a  ==  b  ? 1 : 0
		TEQ    %a $b %r		0b00111010 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = (a) ==  b  ? 1 : 0
		TEQ    $a %b %r		0b00111001 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) =  a  == (b) ? 1 : 0
		TEQ    %a %b %r		0b00111011 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = (a) == (b) ? 1 : 0

		TDT    $a $b %r		0b00111100 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) =  a  !=  b  ? 1 : 0
		TDT    %a $b %r		0b00111110 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = (a) !=  b  ? 1 : 0
		TDT    $a %b %r		0b00111101 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) =  a  != (b) ? 1 : 0
		TDT    %a %b %r		0b00111111 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = (a) != (b) ? 1 : 0

	
##	FLAG/BIT MANIPULATION (group 0x4)
		SETF   $v $p		0b01000000 0b00000000 0b0000000V 0b00000PPP	Fp   =  v
		SETF   %v $p		0b01000010 0b00000000 0b0000VVVV 0b00000PPP	Fp   = (v)
		SETF   $v %p		0b01000001 0b00000000 0b0000000V 0b0000PPPP	F(p) =  v
		SETF   %v %p		0b01000011 0b00000000 0b0000VVVV 0b0000PPPP	F(p) = (v)

		MOVF   $p $z %r		0b01000100 0b0000RRRR 0b00000PPP 0b0000000Z	(r) = Fp   ^  z
		MOVF   %p $z %r		0b01000110 0b0000RRRR 0b0000PPPP 0b0000000Z	(r) = F(p) ^  z
		MOVF   $p %z %r		0b01000101 0b0000RRRR 0b00000PPP 0b0000ZZZZ	(r) = Fp   ^ (z)
		MOVF   %p %z %r		0b01000111 0b0000RRRR 0b0000PPPP 0b0000ZZZZ	(r) = F(p) ^ (z)


##	LOGIC GATES (group 0x5)
		AND    $m $i %r		0b01010000 0b0000RRRR 0bMMMMMMMM 0bIIIIIIII	(r) =  i  &  m
		AND    %m $i %r		0b01010010 0b0000RRRR 0b0000MMMM 0bIIIIIIII	(r) =  i  & (m)
		AND    $m %i %r		0b01010001 0b0000RRRR 0bMMMMMMMM 0b0000IIII	(r) = (i) &  m
		AND    %m %i %r		0b01010011 0b0000RRRR 0b0000MMMM 0b0000IIII	(r) = (i) & (m)

		OR     $v $i %r		0b01010100 0b0000RRRR 0bVVVVVVVV 0bIIIIIIII	(r) =  i  |  v
		OR     %v $i %r		0b01010110 0b0000RRRR 0b0000VVVV 0bIIIIIIII	(r) =  i  | (v)
		OR     $v %i %r		0b01010101 0b0000RRRR 0bVVVVVVVV 0b0000IIII	(r) = (i) |  v
		OR     %v %i %r		0b01010111 0b0000RRRR 0b0000VVVV 0b0000IIII	(r) = (i) | (v)

		XOR    $k $i %r		0b01011000 0b0000RRRR 0bKKKKKKKK 0bIIIIIIII	(r) =  i  ^  k
		XOR    %k $i %r		0b01011010 0b0000RRRR 0b0000KKKK 0bIIIIIIII	(r) =  i  ^ (k)
		XOR    $k %i %r		0b01011001 0b0000RRRR 0bKKKKKKKK 0b0000IIII	(r) = (i) ^  k
		XOR    %k %i %r		0b01011011 0b0000RRRR 0b0000KKKK 0b0000IIII	(r) = (i) ^ (k)

		XNOT   $i $k %r		0b01011100 0b0000RRRR 0bIIIIIIII 0bKKKKKKKK	(r) = ![ i  ^  k ]
		XNOT   %i $k %r		0b01011110 0b0000RRRR 0b0000IIII 0bKKKKKKKK	(r) = ![(i) ^  k ]
		XNOT   $i %k %r		0b01011101 0b0000RRRR 0bIIIIIIII 0b0000KKKK	(r) = ![ i  ^ (k)]
		XNOT   %i %k %r		0b01011111 0b0000RRRR 0b0000IIII 0b0000KKKK	(r) = ![(i) ^ (k)]

	
##	INTEGER ALU (group 0x6)
		ADD    $a $b %r		0b01100000 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) = [ a  +  b ]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit
		ADD    %a $b %r		0b01100010 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = [(a) +  b ]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit
		ADD    $a %b %r		0b01100001 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) = [ a  + (b)]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit
		ADD    %a %b %r		0b01100011 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = [(a) + (b)]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit

		SUB    $a $b %r		0b01100100 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) = [ a  -  b ]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit
		SUB    %a $b %r		0b01100110 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = [(a) -  b ]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit
		SUB    $a %b %r		0b01100101 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) = [ a  - (b)]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit
		SUB    %a %b %r		0b01100111 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = [(a) - (b)]; Fo = 9th bit set; Fs = 8th bit ^ 9th bit

		MUL   $a $b %r		0b01101100 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) =  a  *  b ; Fo = is any of the bits on 2nd byte set; Fs = sign bit ^ all the second byte and the 8th bit of the first byte
		MUL   %a $b %r		0b01101110 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = (a) *  b ; Fo = is any of the bits on 2nd byte set; Fs = sign bit ^ all the second byte and the 8th bit of the first byte
		MUL   $a %b %r		0b01101101 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) =  a  * (b); Fo = is any of the bits on 2nd byte set; Fs = sign bit ^ all the second byte and the 8th bit of the first byte
		MUL   %a %b %r		0b01101111 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = (a) * (b); Fo = is any of the bits on 2nd byte set; Fs = sign bit ^ all the second byte and the 8th bit of the first byte

		
##	HIGH BITS ALU (group 0x7)
		ADDHI  $a $b %r		0b01110000 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) = [ a  +  b ] << 1;
		ADDHI  %a $b %r		0b01110010 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = [(a) +  b ] << 1;
		ADDHI  $a %b %r		0b01110001 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) = [ a  + (b)] << 1;
		ADDHI  %a %b %r		0b01110011 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = [(a) + (b)] << 1;

		SUBHI  $a $b %r		0b01110100 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) = [ a  -  b ] << 1;
		SUBHI  %a $b %r		0b01110110 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = [(a) -  b ] << 1;
		SUBHI  $a %b %r		0b01110101 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) = [ a  - (b)] << 1;
		SUBHI  %a %b %r		0b01110111 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = [(a) - (b)] << 1;

		MULHI  $a $b %r		0b01111000 0b0000RRRR 0bAAAAAAAA 0bBBBBBBBB	(r) = [ a  *  b ] << 8;
		MULHI  %a $b %r		0b01111010 0b0000RRRR 0b0000AAAA 0bBBBBBBBB	(r) = [(a) *  b ] << 8;
		MULHI  $a %b %r		0b01111001 0b0000RRRR 0bAAAAAAAA 0b0000BBBB	(r) = [ a  * (b)] << 8;
		MULHI  %a %b %r		0b01111011 0b0000RRRR 0b0000AAAA 0b0000BBBB	(r) = [(a) * (b)] << 8;

##	SHIFTS (group 0x8)
		// SHL and SHR use 0-extending.

		SHL    $i $n %r		0b10000000 0b0000RRRR 0bIIIIIIII 0bNNNNNNNN	(r) =  i  <<  n ;
		SHL    %i $n %r		0b10000010 0b0000RRRR 0b0000IIII 0bNNNNNNNN	(r) = (i) <<  n ;
		SHL    $i %n %r		0b10000001 0b0000RRRR 0bIIIIIIII 0b0000NNNN	(r) =  i  << (n);
		SHL    %i %n %r		0b10000011 0b0000RRRR 0b0000IIII 0b0000NNNN	(r) = (i) << (n);

		SHR    $i $n %r		0b10000100 0b0000RRRR 0bIIIIIIII 0bNNNNNNNN	(r) =  i  >>  n ;
		SHR    %i $n %r		0b10000110 0b0000RRRR 0b0000IIII 0bNNNNNNNN	(r) = (i) >>  n ;
		SHR    $i %n %r		0b10000101 0b0000RRRR 0bIIIIIIII 0b0000NNNN	(r) =  i  >> (n);
		SHR    %i %n %r		0b10000111 0b0000RRRR 0b0000IIII 0b0000NNNN	(r) = (i) >> (n);

		ROL    $i $n %r		0b10001000 0b0000RRRR 0bIIIIIIII 0bNNNNNNNN	(r) = [[ i  <<  n ] | [ i  >> [8 -  n ]]
		ROL    %i $n %r		0b10001010 0b0000RRRR 0b0000IIII 0bNNNNNNNN	(r) = [[(i) <<  n ] | [(i) >> [8 -  n ]]
		ROL    $i %n %r		0b10001001 0b0000RRRR 0bIIIIIIII 0b0000NNNN	(r) = [[ i  << (n)] | [ i  >> [8 - (n)]]
		ROL    %i %n %r		0b10001011 0b0000RRRR 0b0000IIII 0b0000NNNN	(r) = [[(i) << (n)] | [(i) >> [8 - (n)]]

		ROR    $i $n %r		0b10001100 0b0000RRRR 0bIIIIIIII 0bNNNNNNNN	(r) = [[ i  >>  n ] | [ i  << [8 -  n ]]
		ROR    %i $n %r		0b10001110 0b0000RRRR 0b0000IIII 0bNNNNNNNN	(r) = [[(i) >>  n ] | [(i) << [8 -  n ]]
		ROR    $i %n %r		0b10001101 0b0000RRRR 0bIIIIIIII 0b0000NNNN	(r) = [[ i  >> (n)] | [ i  << [8 - (n)]]
		ROR    %i %n %r		0b10001111 0b0000RRRR 0b0000IIII 0b0000NNNN	(r) = [[(i) >> (n)] | [(i) << [8 - (n)]]

##	USER INPUT (group 0x9)
		GETC   %r		0b10010000 0b0000RRRR 0b00000000 0b00000000	(r) = key
		WAITC  %r		0b10010100 0b0000RRRR 0b00000000 0b00000000	emptyFIFO ? (r) = 0 : (r) = GETC r

##	Recapitulative table
C is for a constant, while R is a register

 LOW NIBBLE / HIGH NIBBLE|0x0|0x1|0x2|0x3|0x4|0x5|0x6|0x7|0x8|0x9|0xA|0xB|0xC|0xD|0xE|0xF
 ------------------------|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---
 0x0                     |nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0x1|ld cc|ld cr|ld rc|ld rr|st0 cc|st0 cr|st0 rc|st0 rr|sb c|nop|sb r|nop|mov c|nop|mov r|nop
 0x2|jmp cc|jmp cr|jmp rc|jmp rr|jcc cc|jcc cr|jcc rc|jcc rr|jsr cc|jsr cr|jsr rc|jsr rr|ret|nop|nop|nop
 0x3|tlt cc|tlt cr|tlt rc|tlt rr|tgt cc|tgt cr|tgt rc|tgt rr|teq cc|teq cr|teq rc|teq rr|tdt cc|tdt cr|tdt rc|tdt rr
 0x4|setf cc|setf cr|setf rc|setf rr|movf cc|movf cr|movf rc|movf rr|nop|nop|nop|nop|nop|nop|nop|nop
 0x5|and cc|and cr|and rc|and rr|or cc|or cr|or rc|or rr|xor cc|xor cr|xor rc|xor rr|xnot cc|xnot cr|xnot rc|xnot rr
 0x6|add cc|add cr|add rc|add rr|sub cc|sub cr|sub rc|sub rr|mul cc|mul cr|mul rc|mul rr|nop|nop|nop|nop
 0x7|addhi cc|addhi cr|addhi rc|addhi rr|subhi cc|subhi cr|subhi rc|subhi rr|mulhi cc|mulhi cr|mulhi rc|mulhi rr|nop|nop|nop|nop
 0x8|shl cc|shl cr|shl rc|shl rr|shr cc|shr cr|shr rc|shr rr|rol cc|rol cr|rol rc|rol rr|ror cc|ror cr|ror rc|ror rr
 0x9|getc r|nop|nop|nop|waitc r|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0xA|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0xB|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0xC|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0xD|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0xE|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop
 0xF|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop|nop





















.386P
.model flat   
 
data segment use16
    buf db 10 
    s   db ?
        db 10 dup(0)  ;CHARACTERS ENTERED BY USER.

    err_unexpected_chr        db "Invalid symbol in number"
    err_float_to_int32_overflow db "Overflow detected in float_to_int32$"
data ends
 
code segment 'code' use16
    assume cs:code, ds:data

exit_with_message macro message
    xor    eax, eax
    
    mov dx, offset message 
	mov ah, 09h
	int 21h

    mov    ah, 4Ch
    int    21h
endm
        
input proc ; (char *) -> void
    push ebp
    mov ebp, esp
    
    push si

    mov dx, word ptr [ebp+6]
    xor ax, ax
    mov ah, 0Ah
    int 21h

    mov si, word ptr [ebp+6]
    inc si
    mov cl, byte ptr [si]
    xor ch, ch
    add si, cx
    mov byte ptr [si+1], '$'
    
    pop si

    mov esp, ebp
    pop ebp
    ret
input endp

; eax, ecx, edx are caller-safe!
float_parse proc ; (uint16 len, char *str)
    push ebp
    mov ebp, esp
    
    sub ESP, 12;
    sign__float_parse equ dword ptr [EBP - 4]
    mov sign__float_parse, 0 ; sign__float_parse data
    
    buffer__float_parse EQU dword ptr [EBP - 8]
    mov buffer__float_parse, 0 ; mantissa buffer__float_parse
    exp_from_mantissa__float_parse equ dword ptr [EBP - 12]
    mov exp_from_mantissa__float_parse, 0 
    
    ; callee-safe registers
    push bx
    push si
    push di
    
    ; subroutine body 
    len__float_parse EQU di
    mov len__float_parse, word ptr [ebp + 6]
    
    str_ptr__float_parse EQU bx
    mov str_ptr__float_parse, word ptr [ebp + 6 + 2]  ; string ptr
    
    xor eax, eax
    whole_part__float_parse EQU EAX
    whole_part_l__float_parse EQU AL 

    xor si, si
    
    cmp byte ptr [str_ptr__float_parse], '-'
        jne _after_sign_check__float_parse 
        mov sign__float_parse, 080000000h ; 1 << 31
        inc si
    
_after_sign_check__float_parse:
    exponent__float_parse EQU EDX
    xor exponent__float_parse, exponent__float_parse

    mantissa__float_parse EQU ECX
    xor mantissa__float_parse, mantissa__float_parse

_loop__float_parse:
    cmp si, len__float_parse
    je _check_for_value_triviality__float_parse 
    
    cmp byte ptr [str_ptr__float_parse + si], '.'
    je _found_dot__float_parse
    
    cmp byte ptr [str_ptr__float_parse + si], '0'
    jl _error__float_parse
    cmp byte ptr [str_ptr__float_parse + si], '9' 
    jg _error__float_parse
    
    imul whole_part__float_parse, whole_part__float_parse, 10
    add whole_part_l__float_parse, byte ptr [str_ptr__float_parse + si]
    sub whole_part_l__float_parse, '0'
    
    inc si
    jmp _loop__float_parse
    
_found_dot__float_parse:
    inc si ; skipping the dot
    
    push whole_part__float_parse 
    
    sub ebp, 12  ; exp_from_mantissa's address
    push ebp
    add ebp, 12
    
    cmp whole_part__float_parse, 0
        jle _whole_part_is_empty__float_parse 
        push 0
        jmp _done_whole_part_cmp__float_parse
    _whole_part_is_empty__float_parse:
        push 1
        jmp _done_whole_part_cmp__float_parse
    _done_whole_part_cmp__float_parse:
    
    add str_ptr__float_parse, si
    push str_ptr__float_parse
    sub len__float_parse, si
    push len__float_parse
    call parse_mantissa
    add esp, 10

    mov exponent__float_parse, exp_from_mantissa__float_parse 

    mov mantissa__float_parse, eax
    pop whole_part__float_parse
    
_check_for_value_triviality__float_parse:
    cmp whole_part__float_parse, 0
        jne _build_float__float_parse

    cmp exponent__float_parse, 0
        je _check_mantissa_triviality__float_parse
    mov whole_part__float_parse, 1
    jmp _build_float__float_parse
    
_check_mantissa_triviality__float_parse:
    cmp mantissa__float_parse, 0
        je _value_is_zero__float_parse
    mov whole_part__float_parse, 1
    jmp _build_float__float_parse
    
_value_is_zero__float_parse:
    mov eax, 0
    jmp _epilogue__float_parse

_build_float__float_parse:
        
_loop2__float_parse:
    cmp whole_part__float_parse, 1
        je _loop2_end__float_parse 
    
    inc exponent__float_parse
    
    mov buffer__float_parse, whole_part__float_parse
    and buffer__float_parse, 1 
    shl buffer__float_parse, 31
    
    shr mantissa__float_parse, 1
    or mantissa__float_parse, buffer__float_parse
    
    shr whole_part__float_parse, 1
    jmp _loop2__float_parse
    
_loop2_end__float_parse:
    add exponent__float_parse, 127
    shl exponent__float_parse, 24
    shr exponent__float_parse, 1
    
    or exponent__float_parse, sign__float_parse
    shr mantissa__float_parse, 9
    or mantissa__float_parse, exponent__float_parse

_epilogue__float_parse:
    mov EAX, mantissa__float_parse

    ; restoring registers
    pop di
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error__float_parse:
    exit_with_message err_unexpected_chr
float_parse endp

parse_mantissa proc  ; (uint16 len, char [data *] str, uint16 skip, uint32 [stack *] exponent)
    push ebp
    mov ebp, esp
    
    sub ESP, 2
     
    push bx
    push si
    push edi
    ; subroutine body

    xor eax, eax ; mantissa
    
    len__parse_mantissa EQU word PTR [EBP + 6]     
    
    str_ptr__parse_mantissa EQU bx
    mov str_ptr__parse_mantissa, WORD PTR [EBP + 6 + 2]  ; str_ptr__parse_mantissa. points after a dot symbol
    
    still_skipping_flag__parse_mantissa equ word ptr [ebp + 6 + 4]
    
    exponent_ptr__parse_mantissa equ EDI
    mov exponent_ptr__parse_mantissa, DWORD PTR [EBP + 6 + 6]
    mov dword ptr [exponent_ptr__parse_mantissa], 0
    
    MAX_MANTISSA_SIZE__parse_mantissa = 23
    cmp len__parse_mantissa, MAX_MANTISSA_SIZE__parse_mantissa 
    jle _mantissa_is_at_most_23__parse_mantissa
        mov len__parse_mantissa, MAX_MANTISSA_SIZE__parse_mantissa

_mantissa_is_at_most_23__parse_mantissa:
    xor si, si 
_normaize_loop__parse_mantissa:
    cmp si, len__parse_mantissa
    jge _normaize_loop_end__parse_mantissa
    
    cmp byte ptr [str_ptr__parse_mantissa + si], '0'
    jl _error__parse_mantissa

    cmp byte ptr [str_ptr__parse_mantissa + si], '9'
    jg _error__parse_mantissa
    
    sub byte ptr [str_ptr__parse_mantissa + si], '0'
    inc si
    jmp _normaize_loop__parse_mantissa
_normaize_loop_end__parse_mantissa:
    
    has_decimal_part__parse_mantissa EQU byte ptr [EBP - 1]
    iteration_count__parse_mantissa EQU byte ptr [EBP - 2]
    mov iteration_count__parse_mantissa, 0

    mov cl, 31
_decimal_part_outer_start__parse_mantissa:
    cmp iteration_count__parse_mantissa, MAX_MANTISSA_SIZE__parse_mantissa
        je _decimal_part_outer_end__parse_mantissa

    inc iteration_count__parse_mantissa

    mov si, len__parse_mantissa
    dec si
    
    carry__parse_mantissa equ edx
    xor carry__parse_mantissa, carry__parse_mantissa 
    carry_l__parse_mantissa equ dl

    mov has_decimal_part__parse_mantissa, 0 
    _decimal_part_inner__parse_mantissa:
        digit__parse_mantissa EQU byte ptr [str_ptr__parse_mantissa + si]
    
        add carry_l__parse_mantissa, digit__parse_mantissa 
        add digit__parse_mantissa, carry_l__parse_mantissa ; multiply digit__parse_mantissa by 2 with a carry__parse_mantissa
        
        cmp digit__parse_mantissa, 10
        jl _decimal_part_inner_digit_lt_10__parse_mantissa
            sub digit__parse_mantissa, 10
            mov carry_l__parse_mantissa, 1
            jmp _decimal_part_inner_digit_lt_10_done__parse_mantissa
        _decimal_part_inner_digit_lt_10__parse_mantissa:
            xor carry_l__parse_mantissa, carry_l__parse_mantissa 
            jmp _decimal_part_inner_digit_lt_10_done__parse_mantissa 
 
       _decimal_part_inner_digit_lt_10_done__parse_mantissa: 
        cmp digit__parse_mantissa, 0
            je cmp_done__parse_mantissa 
            mov has_decimal_part__parse_mantissa, 1
        cmp_done__parse_mantissa:
        
        cmp si, 0
        je _decimal_part_inner_end__parse_mantissa

        dec si
        jmp _decimal_part_inner__parse_mantissa
_decimal_part_inner_end__parse_mantissa:    
    cmp has_decimal_part__parse_mantissa, 1
        jne _no_decimal_part_left__parse_mantissa

        cmp still_skipping_flag__parse_mantissa, 1
            jne _after_still_skipping_flag__parse_mantissa
            cmp carry__parse_mantissa, 0
                je _carry_cmp_done__parse_mantissa
                mov still_skipping_flag__parse_mantissa, 0
        
            _carry_cmp_done__parse_mantissa:
            dec iteration_count__parse_mantissa
            dec dword ptr ss:[exponent_ptr__parse_mantissa]
            jmp _decimal_part_outer_start__parse_mantissa
            
        _after_still_skipping_flag__parse_mantissa:
        shl carry__parse_mantissa, CL
        dec CL
        
        or eax, carry__parse_mantissa
        jmp _decimal_part_outer_start__parse_mantissa

    _no_decimal_part_left__parse_mantissa:
        cmp carry__parse_mantissa, 0
        je _decimal_part_outer_end__parse_mantissa

        cmp still_skipping_flag__parse_mantissa, 1
            jne _after_still_skipping_flag_no_decimal__parse_mantissa
            dec dword ptr ss:[exponent_ptr__parse_mantissa]
            jmp _decimal_part_outer_end__parse_mantissa
            
        _after_still_skipping_flag_no_decimal__parse_mantissa:
        shl carry__parse_mantissa, CL
        dec CL
        
        or eax, carry__parse_mantissa

        jmp _decimal_part_outer_end__parse_mantissa 
_decimal_part_outer_end__parse_mantissa:
    ; epilogue
    pop edi
    pop si
    pop bx
    
    mov esp, ebp
    pop ebp
    ret

_error__parse_mantissa: 
    exit_with_message err_unexpected_chr
parse_mantissa endp
    
exchange_memory macro left, right, tmp_reg
    mov tmp_reg, left
    xchg tmp_reg, right
    mov left, tmp_reg
endm
    
mov_memory macro left, right, tmp_reg
    mov tmp_reg, right
    mov left, tmp_reg
endm

cmp_memory macro left, right, tmp_reg
    mov tmp_reg, left
    cmp tmp_reg, right
endm
    
float_decompose macro num_reg, sign32, exponent32, mantissa32
    mov sign32, num_reg
    shr sign32, 31
    
    mov exponent32, num_reg
    shl exponent32, 1
    shr exponent32, 24
    
    mov mantissa32, num_reg
    shl mantissa32, 9
    shr mantissa32, 9
endm
    
float_check_zero macro float, temp_reg
    mov temp_reg, float
    shl temp_reg, 1
    shr temp_reg, 1
    cmp temp_reg, 0
endm

float_add proc ; (uint32 [float] left, uint32 [float] right) -> uint32 [float]
    push ebp
    mov ebp, esp

    sub esp, 28

    push EBX
    push EDI
    push ESI 

    left__float_add equ EBX
    right__float_add equ EDX
    buffer__float_add    equ ECX
    buffer_l__float_add  equ CX
    buffer_ll__float_add equ CL

    mov left__float_add, dword ptr [EBP + 6]
    mov right__float_add, dword ptr [EBP + 10]

    float_check_zero left__float_add, buffer__float_add
    jne _left_not_trivial__float_add
    mov eax, right__float_add
    jmp _epilogue__float_add

_left_not_trivial__float_add:          
    float_check_zero right__float_add, buffer__float_add
    jne _not_trivial__float_add 
    mov eax, left__float_add
    jmp _epilogue__float_add

_not_trivial__float_add:         
    left_sign__float_add     equ dword ptr [EBP - 4]
    left_exponent__float_add equ dword ptr [EBP - 8]
    left_mantissa__float_add equ dword ptr [EBP - 12]
    
    right_sign__float_add     equ dword ptr [EBP - 16]
    right_exponent__float_add equ dword ptr [EBP - 20]
    right_mantissa__float_add equ dword ptr [EBP - 24]

    float_decompose left__float_add, left_sign__float_add, left_exponent__float_add, left_mantissa__float_add
    float_decompose right__float_add, right_sign__float_add, right_exponent__float_add, right_mantissa__float_add
_init_done__float_add:   
    mov EDI, left__float_add
    shl edi, 1
    shr edi, 1

    mov ESI, right__float_add
    shl esi, 1
    shr esi, 1

    cmp edi, esi
    jge _left_exp_ge_right__float_add
    xchg left__float_add, right__float_add

    exchange_memory left_sign__float_add,     right_sign__float_add,     buffer__float_add
    exchange_memory left_exponent__float_add, right_exponent__float_add, buffer__float_add
    exchange_memory left_mantissa__float_add, right_mantissa__float_add, buffer__float_add

_left_exp_ge_right__float_add:   
    mov buffer__float_add, left_exponent__float_add
    sub buffer__float_add, right_exponent__float_add

    cmp buffer__float_add, 24
    jle _exp_diff_is_at_most_24__float_add
    mov eax, left__float_add
    jmp _epilogue__float_add
    
_exp_diff_is_at_most_24__float_add:    
    or left_mantissa__float_add,  0800000h ; 1 << 23
    or right_mantissa__float_add, 0800000h ; 1 << 23
                
    shr right_mantissa__float_add, cl
    cmp_memory left_sign__float_add, right_sign__float_add, buffer__float_add
    jne _diff_signs__float_add 

_same_signs__float_add:
    mov eax, left_mantissa__float_add
    add eax, right_mantissa__float_add
    cmp eax, 01000000h ; 1 << 24 = 10 << 23
        jl _not_greater_2__float_add 
        inc left_exponent__float_add
        shr eax, 1

    _not_greater_2__float_add: 
    xor eax, 0800000h

    shl left_sign__float_add, 31
    shl left_exponent__float_add, 23
    or eax, left_sign__float_add
    or eax, left_exponent__float_add
    jmp _epilogue__float_add

_diff_signs__float_add:                
    mov eax, left_mantissa__float_add
    sub eax, right_mantissa__float_add
    cmp eax, 0
    je _epilogue__float_add

    _while__float_add: 
        mov buffer__float_add, eax
        and buffer__float_add, 0800000h ; 1 << 23. 
        cmp buffer__float_add, 0
        jg _while_end__float_add
        shl eax, 1
        dec left_exponent__float_add
        jmp _while__float_add

_while_end__float_add: 
    xor eax, 0800000h
    shl left_sign__float_add, 31
    shl left_exponent__float_add, 23
    or eax, left_sign__float_add
    or eax, left_exponent__float_add
    jmp _epilogue__float_add

_epilogue__float_add:
    pop ESI
    pop EDI
    pop EBX

    mov esp, ebp
    pop ebp
    ret
float_add endp
    
; fast power-of-two multiplication (-127 <= power <= 126)
float_power_2_mult proc ; (uint32 [float] target; uint16 power_of_two)
    push ebp
    mov ebp, esp
    
    sub esp, 8
    sign__float_power_2_mult           equ dword ptr [EBP - 4]
    exponent__float_power_2_mult       equ ecx
    exponent_lower__float_power_2_mult equ cl
    mantissa__float_power_2_mult       equ dword ptr [EBP - 8]
    buffer__float_power_2_mult         equ  dl
    
    target__float_power_2_mult equ eax
    power_of_two__float_power_2_mult equ byte ptr [ebp + 10] ; NOT WORD PTR!! 
    
    mov target__float_power_2_mult, dword ptr [EBP + 6]
    cmp target__float_power_2_mult, 0
    je _epilogue__float_power_2_mult
    
    cmp power_of_two__float_power_2_mult, 0
    je _epilogue__float_power_2_mult
    
    float_decompose target__float_power_2_mult, sign__float_power_2_mult, exponent__float_power_2_mult, mantissa__float_power_2_mult
    sub exponent_lower__float_power_2_mult, 127

_init_done__float_power_2_mult:
    cmp power_of_two__float_power_2_mult, 0
    jl _check_underflow__float_power_2_mult

_check_overflow__float_power_2_mult: 
    ; power_of_two__float_power_2_mult >= 0
    ; exponent__float_power_2_mult + power <= 126 <->  exponent__float_power_2_mult <= 126 - power
    mov buffer__float_power_2_mult, 126
    sub buffer__float_power_2_mult, power_of_two__float_power_2_mult
    cmp exponent_lower__float_power_2_mult, buffer__float_power_2_mult
        jle _result_is_bounded__float_power_2_mult
        mov target__float_power_2_mult, 07f800000h  ; 255 << 23
        shl sign__float_power_2_mult, 31
        or target__float_power_2_mult, sign__float_power_2_mult
        jmp _epilogue__float_power_2_mult
    
_check_underflow__float_power_2_mult: 
    ; power_of_two__float_power_2_mult < 0
    ; exponent__float_power_2_mult + power >= -127 <-> exponent__float_power_2_mult >= -127 - power
    mov buffer__float_power_2_mult, -127
    sub buffer__float_power_2_mult, power_of_two__float_power_2_mult
    cmp exponent_lower__float_power_2_mult, buffer__float_power_2_mult
        jge _result_is_bounded__float_power_2_mult
        mov target__float_power_2_mult, 0
        jmp _epilogue__float_power_2_mult
        
_result_is_bounded__float_power_2_mult:
    add exponent_lower__float_power_2_mult, power_of_two__float_power_2_mult
    add exponent_lower__float_power_2_mult, 127
    
    mov target__float_power_2_mult, exponent__float_power_2_mult
    shl target__float_power_2_mult, 23
    shl sign__float_power_2_mult, 31
    or target__float_power_2_mult, sign__float_power_2_mult
    or target__float_power_2_mult, mantissa__float_power_2_mult

_epilogue__float_power_2_mult: 
    mov esp, ebp
    pop ebp
    ret
float_power_2_mult endp

float_mul proc ; (uint32 [float] left, uint32 [float] right) -> uint32 [float]
    push ebp
    mov ebp, esp
    
    sub esp, 28

    push EBX
    push EDI
    push esi
    push edi

    left__float_mul equ EBX
    right__float_mul equ EDX
    buffer__float_mul equ ECX
    buffer_l__float_mul  equ CX
    buffer_ll__float_mul equ CL

    mov left__float_mul, dword ptr [EBP + 6]
    mov right__float_mul, dword ptr [EBP + 10]

    float_check_zero left__float_mul, buffer__float_mul
    jne _left_not_trivial__float_mul
    mov eax, 0
    jmp _epilogue__float_mul

_left_not_trivial__float_mul:          
    float_check_zero right__float_mul, buffer__float_mul
    jne _not_trivial__float_mul 
    mov eax, 0
    jmp _epilogue__float_mul

_not_trivial__float_mul:         
    left_sign__float_mul     equ dword ptr [EBP - 4]
    left_exponent__float_mul equ dword ptr [EBP - 8]
    left_mantissa__float_mul equ dword ptr [EBP - 12]
    
    right_sign__float_mul     equ dword ptr [EBP - 16]
    right_exponent__float_mul equ dword ptr [EBP - 20]
    right_mantissa__float_mul equ dword ptr [EBP - 24]
    
    old_left_mantissa__float_mul equ dword ptr [EBP - 28]

    float_decompose left__float_mul, left_sign__float_mul, left_exponent__float_mul, left_mantissa__float_mul
    float_decompose right__float_mul, right_sign__float_mul, right_exponent__float_mul, right_mantissa__float_mul
    
    or right_mantissa__float_mul, 0800000h ; 1 << 23
    
    mov buffer__float_mul, 03f800000h
    or buffer__float_mul, left_mantissa__float_mul
    mov old_left_mantissa__float_mul, buffer__float_mul ; 2 ^ 0 * left_mantissa__float_mul

_init_done__float_mul:   
    result_mantissa__float_mul equ esi
    mov result_mantissa__float_mul, 0
    
    counter__float_mul equ di
    xor counter__float_mul, counter__float_mul
_while__float_mul:
    cmp right_mantissa__float_mul, 0
    je _while_done__float_mul
    
    mov buffer__float_mul, right_mantissa__float_mul
    and buffer__float_mul, 1
    cmp buffer__float_mul, 0
    je _after_addition__float_mul
    
    mov buffer__float_mul, old_left_mantissa__float_mul 

    push ecx 
    push edx
    push counter__float_mul
    push buffer__float_mul
    call float_power_2_mult
    add esp, 6
    pop edx
    pop ecx 
    
    push ecx 
    push edx
    push eax
    push result_mantissa__float_mul
    call float_add
    add esp, 8
    pop edx
    pop ecx 
    
    mov result_mantissa__float_mul, eax
_after_addition__float_mul:
    shr right_mantissa__float_mul, 1
    inc counter__float_mul
    jmp _while__float_mul
_while_done__float_mul:
    xor buffer_l__float_mul, buffer_l__float_mul
    mov buffer_ll__float_mul, -23
    push buffer_l__float_mul
    push result_mantissa__float_mul
    call float_power_2_mult
    add esp, 6
    
    mov buffer__float_mul, left_exponent__float_mul
    sub buffer_ll__float_mul, 127
    push buffer_l__float_mul
    push eax
    call float_power_2_mult
    add esp, 6
    
    mov buffer__float_mul, right_exponent__float_mul
    sub buffer_ll__float_mul, 127
    push buffer_l__float_mul
    push eax
    call float_power_2_mult
    add esp, 6
    
    mov buffer__float_mul, left_sign__float_mul
    cmp buffer__float_mul, right_sign__float_mul
        je _epilogue__float_mul
        or eax, 080000000h ; 1 << 31
        jmp _epilogue__float_mul

_epilogue__float_mul:
    pop edi
    pop esi
    pop EDI
    pop EBX

    mov esp, ebp
    pop ebp
    ret
float_mul endp
    
scan_float macro loc, dst
    push ecx
    push edx

    push offset loc + 1 
    xor dx, dx
    mov dl, byte ptr [loc] 
    push dx
    call float_parse
    sub esp, 4
    mov dst, eax

    pop edx
    pop ecx
endm
    
float_negate macro target
    xor target, 080000000h ; 1 << 31
endm

float_check_positivity macro target
    push target
    shr target, 31
    cmp target, 0
    pop target
endm
    
float_to_int32 proc ; (uint32 [float]) -> int32
    push ebp
    mov ebp, esp

    sub esp, 8
    sign__float_to_int32        equ dword ptr [EBP - 4]
    exponent__float_to_int32    equ dword ptr [EBP - 8]
    exponent_ll__float_to_int32 equ byte ptr [EBP - 8]  ; little endian
    mantissa__float_to_int32    equ EAX
    buffer__float_to_int32      equ ECX
    buffer_ll__float_to_int32   equ CL
    
    mov edx, dword ptr [EBP + 6]
    float_decompose edx, sign__float_to_int32, exponent__float_to_int32, mantissa__float_to_int32

_init_done__float_to_int32:
    sub exponent_ll__float_to_int32, 127
    cmp exponent_ll__float_to_int32, 31  ; keep one bit for the sign__float_to_int32
    jg _overflow__float_to_int32
    
    cmp exponent_ll__float_to_int32, 0
        jge _not_trivial__float_to_int32
    mov eax, 0
    jmp _epilogue__float_to_int32

_not_trivial__float_to_int32:
    or mantissa__float_to_int32, 0800000h; 1 << 23
    cmp exponent_ll__float_to_int32, 23
    jg _exponent_gt_23__float_to_int32
    jmp _exponent_le_23__float_to_int32

_exponent_le_23__float_to_int32:
    mov buffer_ll__float_to_int32, 23
    sub buffer_ll__float_to_int32, exponent_ll__float_to_int32
    shr mantissa__float_to_int32, buffer_ll__float_to_int32
    cmp sign__float_to_int32, 1
        jne _epilogue__float_to_int32
    neg mantissa__float_to_int32
    jmp _epilogue__float_to_int32

_exponent_gt_23__float_to_int32:
    mov buffer_ll__float_to_int32, exponent_ll__float_to_int32
    sub buffer_ll__float_to_int32, 23
    shl mantissa__float_to_int32, buffer_ll__float_to_int32
    
    cmp sign__float_to_int32, 1
        jne _epilogue__float_to_int32
    neg mantissa__float_to_int32
    jmp _epilogue__float_to_int32

_epilogue__float_to_int32:
    mov esp, ebp
    pop ebp
    ret
_overflow__float_to_int32:
    exit_with_message err_float_to_int32_overflow
float_to_int32 endp
    
int32_to_float proc ; (int32) -> float
    push ebp
    mov ebp, esp
    
    sub esp, 4
    
    sign__int32_to_float equ dword ptr [ebp - 4]
    mov sign__int32_to_float, 0
    
    num__int32_to_float equ edx
    mov num__int32_to_float, dword ptr [EBP + 6]

    cmp num__int32_to_float, 0
        jg _sign_determined__int32_to_float 
        jl _is_negative__int32_to_float
        mov eax, 0 
        jmp _epilogue__int32_to_float

_is_negative__int32_to_float:
    neg num__int32_to_float
    mov sign__int32_to_float, 080000000h ; 1 << 31
    jmp _sign_determined__int32_to_float

_sign_determined__int32_to_float:
    exponent__int32_to_float    equ ecx
    exponent_ll__int32_to_float equ cl
    xor exponent__int32_to_float, exponent__int32_to_float 
    
    bsr exponent__int32_to_float, num__int32_to_float
    cmp exponent_ll__int32_to_float, 23
    jl _first_bit_index_lt_23__int32_to_float

_first_bit_index_ge_23__int32_to_float:
    sub exponent_ll__int32_to_float, 23
    shr num__int32_to_float, exponent_ll__int32_to_float
    jmp _mantissa_done__int32_to_float
    
_first_bit_index_lt_23__int32_to_float:
    sub exponent_ll__int32_to_float, 23
    neg exponent_ll__int32_to_float
    shl num__int32_to_float, exponent_ll__int32_to_float
    neg exponent_ll__int32_to_float
    jmp _mantissa_done__int32_to_float

_mantissa_done__int32_to_float:
    add exponent_ll__int32_to_float, 23 + 127
    shl exponent__int32_to_float, 23
    or exponent__int32_to_float, sign__int32_to_float
    xor num__int32_to_float, 0800000h ; 1 << 23
    or exponent__int32_to_float, num__int32_to_float
    mov eax, exponent__int32_to_float
    jmp _epilogue__int32_to_float

_epilogue__int32_to_float: 
    mov esp, ebp
    pop ebp
    ret
int32_to_float endp

; https://stackoverflow.com/a/5812104
print_i32 proc ; (int32) -> void
    push ebp 
    mov ebp, esp
    
    sub esp, 1 
    is_negative__print_i32 equ byte ptr [EBP - 1]
    mov is_negative__print_i32, 0

    push ebx
    
    num__print_i32 equ eax
    mov num__print_i32, dword ptr [ebp + 6]
    
    cmp num__print_i32, 0
        jge _determined_sign__print_i32
    neg num__print_i32
    mov is_negative__print_i32, 1

_determined_sign__print_i32:
    mov cx, 0
    mov ebx, 10

_loophere__print_i32:
    xor edx, edx
    div ebx    
    add dl, '0'
    push dx    
    inc cx     
    cmp num__print_i32, 0  
jne _loophere__print_i32

    mov ah, 2                       ;2 is the function number of output char in the DOS Services.
    cmp is_negative__print_i32, 1
        jne _loophere2__print_i32 

    mov dx, '-'
    int 21h                         
    
_loophere2__print_i32:
    pop dx                          ;restore digits from last to first
    int 21h                         ;calls DOS Services
    loop _loophere2__print_i32 
    
_epilogue__print_i32:
    pop ebx
    
    mov esp, ebp
    pop ebp
    ret
print_i32 endp

float_display proc ; (uint32 float) -> void
    push ebp
    mov ebp, esp
    
    sub esp, 9

    float_10__float_display         equ dword ptr [ebp - 4]
    new_decimal_part__float_display equ dword ptr [ebp - 8]
    iter_count__float_display       equ  byte ptr [ebp - 9]

    mov eax, 10
    push eax
    call int32_to_float
    mov float_10__float_display, eax
    
    num__float_display equ dword ptr [ebp + 6]
    float_check_positivity num__float_display
    je _float_is_positive__float_display

    mov ah, 2                       
    mov dx, '-'
    int 21h    

    float_negate num__float_display

_float_is_positive__float_display:
    push num__float_display
    call float_to_int32
    add esp, 4
    
    push eax ; save int(num)
    push eax
    call print_i32
    add esp, 4
    pop eax  ; restore int(num)
    
    push eax
    call int32_to_float
    add esp, 4
    
    float_negate eax
    push eax
    push num__float_display
    call float_add
    add esp, 8
    
    decimal_part__float_display equ edx
    mov decimal_part__float_display, eax

    push dx
    mov ah, 2                       
    mov dx, '.'
    int 21h    
    pop dx

    mov iter_count__float_display, 0
_while__float_display:
    cmp iter_count__float_display, 10 ; how many digits to print after the point
    je _while_done__float_display
    
    push float_10__float_display
    push decimal_part__float_display
    call float_mul
    mov new_decimal_part__float_display, eax
    add esp, 8
    
    push eax
    call float_to_int32
    add esp, 4
    
    push eax
    mov dx, ax
    add dx, '0'
    mov ah, 2                       
    int 21h  
    pop eax

    push eax
    call int32_to_float
    add esp, 4
    
    mov decimal_part__float_display, new_decimal_part__float_display
    float_negate eax
    push eax
    push decimal_part__float_display 
    call float_add
    add esp, 8

    mov decimal_part__float_display, eax
    
    inc iter_count__float_display 
    jmp _while__float_display
_while_done__float_display:

_epilogue__float_display:
    mov esp, ebp
    pop ebp
    ret
float_display endp

main proc
    mov    eax, data
    mov    ds, eax
    
    mov ebp, esp 
    mov eax, 0
    push eax
    push eax
    
    left__main equ dword ptr [ebp - 4]
    right__main equ dword ptr [ebp - 8]
    
    mov dx, offset buf
    push dx
    call input
    add esp, 2
    scan_float s left__main
    
    mov ah, 2  ; print CR symbol
    mov dx, 10 
    int 21h                         

    mov dx, offset buf
    push dx
    call input
    add esp, 2
    scan_float s right__main
    
    mov ah, 2  ; print CR symbol
    mov dx, 10 
    int 21h                         
    
    push left__main
    push right__main
    call float_add
    add esp, 8
    
    push eax
    call float_display
    add esp, 4
    
    xor    eax, eax
    mov    ah, 4Ch
    int    21h
main endp

code ends
end main

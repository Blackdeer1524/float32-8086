.386P
.model flat   
 
data segment use16
    ;input_buf db 26         ;MAX NUMBER OF CHARACTERS ALLOWED (25).
    ;          db ?          ;NUMBER OF CHARACTERS ENTERED BY USER.
    ;          db 26 dup(0)  ;CHARACTERS ENTERED BY USER.
    first_str db (first_EOF - $ - 1)
              db "-414.4314"
    first_EOF db "$"
    
    second_str db (second_EOF - $ - 1)
              db "0.5"
    second_EOF db "$"

    err_unexpected_chr db "Invalid symbol in number"
data ends
 
code segment 'code' use16
    assume cs:code, ds:data

exit_invalid_char proc 
    xor    eax, eax
    
    mov dx, offset err_unexpected_chr
	mov ah, 09h
	int 21h

    mov    ah, 4Ch
    int    21h
exit_invalid_char endp
    
input proc
    push   ebp
    mov    ebp, esp

    mov    edx, [ebp + 2 + 4]
    xor    eax, eax
    mov    ah, 0Ah
    int    21h

    mov    edx, [ebp + 2 + 6]
    inc    edx
    mov    esi, edx
    mov    ecx, [esi]
    xor    ch, ch
    add    si, cx
    mov    byte ptr [si+1], '$'
    inc    dx

    pop    ebp
    ret
input endp

; eax, ecx, edx are caller-safe!
parse_float proc ; (uint16 len, char *str)
    push ebp
    mov ebp, esp
    
    sub ESP, 12;
    mov dword ptr [EBP - 4], 0 ; sign data
    mov dword ptr [EBP - 8], 0 ; mantissa buffer
    mov dword ptr [EBP - 12], 0 ; additional exponent
    
    ; callee-safe registers
    push bx
    push si
    push di
    
    ; subroutine body 
    len__parse_float EQU di
    mov len__parse_float, word ptr [ebp + 6]
    
    str_ptr__parse_float EQU bx
    mov str_ptr__parse_float, word ptr [ebp + 6 + 2]  ; string ptr
    
    xor eax, eax
    whole_part__parse_float EQU EAX
    whole_part_l__parse_float EQU AL 

    xor si, si
    
    cmp byte ptr [str_ptr__parse_float], '-'
        jne _loop__parse_float
        mov byte ptr [EBP - 1], 080h  ; 2^7
        inc si
    
    exponent__parse_float EQU EDX
    xor exponent__parse_float, exponent__parse_float
_loop__parse_float:
    cmp si, len__parse_float
    je _no_dot__parse_float
    
    cmp byte ptr [str_ptr__parse_float + si], '.'
    je _found_dot__parse_float
    
    cmp byte ptr [str_ptr__parse_float + si], '0'
    jl _error__parse_float
    cmp byte ptr [str_ptr__parse_float + si], '9' 
    jg _error__parse_float
    
    imul whole_part__parse_float, whole_part__parse_float, 10
    add whole_part_l__parse_float, byte ptr [str_ptr__parse_float + si]
    sub whole_part_l__parse_float, '0'
    
    inc si
    jmp _loop__parse_float

_no_dot__parse_float:
    mantissa__parse_float EQU ECX

    xor mantissa__parse_float,mantissa__parse_float
    jmp _build_float__parse_float
    
_found_dot__parse_float:
    inc si ; skipping the dot
    
    push whole_part__parse_float
    push mantissa__parse_float
    
    sub ebp, 12
    push ebp
    add ebp, 12
    
    cmp whole_part__parse_float, 0
        jle _whole_part_is_empty__parse_float 
        push 0
        jmp _done_whole_part_cmp__parse_float
    _whole_part_is_empty__parse_float:
        push 1
        jmp _done_whole_part_cmp__parse_float
    _done_whole_part_cmp__parse_float:
    
    add str_ptr__parse_float, si
    push str_ptr__parse_float

    sub len__parse_float, si
    push len__parse_float
    
    call parse_mantissa

    pop len__parse_float
    pop str_ptr__parse_float
    
    add esp, 2
    add esp, 4

    mov exponent__parse_float, dword ptr [ebp - 12]

    pop mantissa__parse_float
    mov mantissa__parse_float, eax
    pop whole_part__parse_float

_build_float__parse_float:
    
_loop2__parse_float:
    cmp whole_part__parse_float, 1
        jle _loop2_end__parse_float 
    
    inc exponent__parse_float
    
    mov byte ptr [ebp - 5], whole_part_l__parse_float
    and byte ptr [ebp - 5], 1 
    shl byte ptr [ebp - 5], 7
    
    shr mantissa__parse_float, 1
    or mantissa__parse_float, dword ptr [ebp - 8]
    
    shr whole_part__parse_float, 1
    jmp _loop2__parse_float
    
_loop2_end__parse_float:
    add exponent__parse_float, 127
    shl exponent__parse_float, 24
    shr exponent__parse_float, 1
    
    or exponent__parse_float, dword ptr [ebp - 4]  ; adding a sign bit
    shr mantissa__parse_float, 9
    or mantissa__parse_float, exponent__parse_float

_epilogue__parse_float:
    mov EAX, mantissa__parse_float

    ; restoring registers
    pop di
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error__parse_float:
    call exit_invalid_char 
parse_float endp

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
    call exit_invalid_char    
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

float_add proc ; (uint32 [float] left, uint32 [float] right) -> uint32 [float]
    push ebp
    mov ebp, esp

    sub esp, 28

    push EBX
    push EDI
    push ESI 

    left__float_add equ EBX
    right__float_add equ EDX
    buffer__float_add equ ECX

    mov left__float_add, dword ptr [EBP + 6]
    mov right__float_add, dword ptr [EBP + 10]

    cmp left__float_add, 0
    jne _left_not_trivial__float_add
    mov eax, right__float_add
    jmp _epilogue__float_add

_left_not_trivial__float_add:          
    cmp right__float_add, 0
    jne _right_not_trivial__float_add
    mov eax, left__float_add
    jmp _epilogue__float_add

_right_not_trivial__float_add:         
    left_sign__float_add     equ dword ptr [EBP - 4]
    left_exponent__float_add equ dword ptr [EBP - 8]
    left_mantissa__float_add equ dword ptr [EBP - 12]
    
    right_sign__float_add     equ dword ptr [EBP - 16]
    right_exponent__float_add equ dword ptr [EBP - 20]
    right_mantissa__float_add equ dword ptr [EBP - 24]

    mov left_sign__float_add, left__float_add
    shr left_sign__float_add, 31
    
    mov left_exponent__float_add, left__float_add
    shl left_exponent__float_add, 1
    shr left_exponent__float_add, 24
    
    mov left_mantissa__float_add, left__float_add
    shl left_mantissa__float_add, 9
    shr left_mantissa__float_add, 9
    
    mov right_sign__float_add, right__float_add
    shr right_sign__float_add, 31
    
    mov right_exponent__float_add, right__float_add
    shl right_exponent__float_add, 1
    shr right_exponent__float_add, 24
    
    mov right_mantissa__float_add, right__float_add
    shl right_mantissa__float_add, 9
    shr right_mantissa__float_add, 9

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
    
    mov sign__float_power_2_mult, target__float_power_2_mult
    shr sign__float_power_2_mult, 31
    
    mov exponent__float_power_2_mult, target__float_power_2_mult
    shl exponent__float_power_2_mult, 1
    shr exponent__float_power_2_mult, 24
    sub exponent_lower__float_power_2_mult, 127
    
    mov mantissa__float_power_2_mult, target__float_power_2_mult
    shl mantissa__float_power_2_mult, 9
    shr mantissa__float_power_2_mult, 9

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

; float_mul proc ; (uint32 [float] left, uint32 [float] right) -> uint32 [float]
;     push ebp
;     mov ebp, esp
;     
;     sub esp, 28
; 
;     push EBX
;     push EDI
;     push esi
;     push edi
; 
;     left equ EBX
;     right equ EDX
;     buffer equ ECX
; 
;     mov left, dword ptr [EBP + 6]
;     mov right, dword ptr [EBP + 10]
; 
;     cmp left, 0
;     jne _left_not_trivial
;     mov eax, 0
;     jmp _epilogue
; 
; _left_not_trivial:          
;     cmp right, 0
;     jne _right_not_trivial
;     mov eax, 0
;     jmp _epilogue
; 
; _right_not_trivial:         
;     left_sign     equ dword ptr [EBP - 4]
;     left_exponent equ dword ptr [EBP - 8]
;     left_mantissa equ dword ptr [EBP - 12]
;     
;     right_sign     equ dword ptr [EBP - 16]
;     right_exponent equ dword ptr [EBP - 20]
;     right_mantissa equ dword ptr [EBP - 24]
;     
;     old_left_mantissa equ dword ptr [EBP - 28]
; 
;     mov left_sign, left
;     shr left_sign, 31
;     
;     mov left_exponent, left
;     shl left_exponent, 1
;     shr left_exponent, 24
;     
;     mov left_mantissa, left
;     shl left_mantissa, 9
;     shr left_mantissa, 9
;     
;     mov right_sign, right
;     shr right_sign, 31
;     
;     mov right_exponent, right
;     shl right_exponent, 1
;     shr right_exponent, 24
;     
;     mov right_mantissa, right
;     shl right_mantissa, 9
;     shr right_mantissa, 9
;     or right_mantissa, 0800000h ; 1 << 23
;     
;     mov old_left_mantissa, 03f800000h
;     or  old_left_mantissa, left_mantissa  ; 2 ^ 0 * left mantissa
; _init_done:   
;     result_exponent equ edi
;     mov result_exponent, left_exponent
;     add result_exponent, right_exponent ; TODO: CHECK EXPONENT OVERFLOW 
;     
;     result_mantissa equ esi
;     mov result_mantissa 0
;     
;     counter equ di
;     xor counter, counter
; _while:
;     cmp right_mantissa, 0
;     je _while_done
;     
;     mov buffer, right_mantissa
;     and buffer, 1
;     cmp buffer, 0
;     je _after_addition
;     
;     mov buffer, old_left_mantissa 
; 
;     push ecx 
;     push edx
;     push counter
;     push buffer
;     call float_power_2_mult
;     add esp, 6
;     pop edx
;     pop ecx 
;     
;     push ecx 
;     push edx
;     push eax
;     push result_mantissa
;     call float_add
;     add esp, 8
;     pop edx
;     pop ecx 
;     
;     mov result_mantissa, eax
; _after_addition:
;     shr right_mantissa, 1
;     inc counter
;     jmp _while
; _while_done:
;     push ecx 
;     push edx
;     
;     push 23
;     push result_mantissa
;     call float_power_2_div
;     add esp, 6
;     
;     pop edx
;     pop ecx 
; 
; _epilogue:
;     pop edi
;     pop esi
;     pop EDI
;     pop EBX
; 
;     mov esp, ebp
;     pop ebp
;     ret
; float_mul endp

main proc
    mov    eax, data
    mov    ds, eax
    
    mov ebp, esp 
    mov eax, 0
    push eax
    push eax
    
    left__main equ dword ptr [ebp - 4]
    right__main equ dword ptr [ebp - 8]
    
    
    ; ========= parsing a float =========
    push offset first_str + 1 
    xor dx, dx
    mov dl, byte ptr [first_str] 
    push dx
    call parse_float
    sub esp, 4
    ; ===================================
    
    push ecx
    push edx
    
    xor dx, dx
    mov dl, -30
    push dx
    push eax
    call float_power_2_mult
    add esp, 6
    
    pop edx
    pop ecx 
    
    ;; ============================
    ;push offset first_str + 1 
    ;
    ;xor dx, dx
    ;mov dl, byte ptr [first_str] 
    ;push dx
    ;call parse_float
    ;sub esp, 4
    ;
    ;mov left__main, eax

    ;; ============================
    ;push offset second_str + 1 
    ;
    ;xor dx, dx
    ;mov dl, byte ptr [second_str] 
    ;push dx
    ;call parse_float
    ;add esp, 4
    ;
    ;mov right__main, eax
    ;; ============================
    ;
    ;mov eax, right__main
    ;push right__main
    ;mov eax, left__main
    ;push left__main
    ;call float_add
    ;add esp, 8
    ;
    ;xor    eax, eax
    ;mov    ah, 4Ch
    ;int    21h
main endp

code ends
end main

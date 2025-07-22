.386P
.model flat   
 
data segment use16
    ;input_buf db 26         ;MAX NUMBER OF CHARACTERS ALLOWED (25).
    ;          db ?          ;NUMBER OF CHARACTERS ENTERED BY USER.
    ;          db 26 dup(0)  ;CHARACTERS ENTERED BY USER.
    input_str db (input_EOF - $ - 1)
              db "0.1"
    input_EOF db "$"

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
    cmp mantissa__parse_float, 0
    je _epilogue__parse_float
    
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
                jne _carry_is_not_zero__parse_mantissa
                dec iteration_count__parse_mantissa
                dec dword ptr ss:[exponent_ptr__parse_mantissa]
                jmp _decimal_part_outer_start__parse_mantissa
        
            _carry_is_not_zero__parse_mantissa:
            mov still_skipping_flag__parse_mantissa, 0
            dec iteration_count__parse_mantissa
            dec dword ptr ss:[exponent_ptr__parse_mantissa]
            jmp _decimal_part_outer_start__parse_mantissa
            
        _after_still_skipping_flag__parse_mantissa:
        shl carry__parse_mantissa, CL
        dec CL
        
        or eax, carry__parse_mantissa
        jmp _decimal_part_outer_start__parse_mantissa

    _no_decimal_part_left__parse_mantissa:
        cmp carry__parse_mantissa, 1
        jne _decimal_part_outer_end__parse_mantissa

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

    
float_add proc ; (uint32 [float] left, uint32 [float] right) -> uint32 [float]
    push ebp
    mov ebp, esp
    
    sub esp, 24

    left_sign__float_add     equ dword ptr [EBP - 4]
    left_exponent__float_add equ dword ptr [EBP - 8]
    left_mantissa__float_add equ dword ptr [EBP - 12]
    
    right_sign__float_add     equ dword ptr [EBP - 16]
    right_exponent__float_add equ dword ptr [EBP - 20]
    right_mantissa__float_add equ dword ptr [EBP - 24]
    
    mov left_sign__float_add, dword ptr [EBP + 6]
    shr left_sign__float_add, 31
    
    mov left_exponent__float_add, dword ptr [EBP + 6]
    shl left_exponent__float_add, 1
    shr left_exponent__float_add, 24
    
    mov left_mantissa__float_add, dword ptr [EBP + 6]
    shl left_mantissa__float_add, 9
    shr left_mantissa__float_add, 9
    
    mov right_sign__float_add, dword ptr [EBP + 10]
    shr right_sign__float_add, 31
    
    mov right_exponent__float_add, dword ptr [EBP + 10]
    shl right_exponent__float_add, 1
    shr right_exponent__float_add, 24
    
    mov right_mantissa__float_add, dword ptr [EBP + 10]
    shl right_mantissa__float_add, 9
    shr right_mantissa__float_add, 9
    
    cmp left_exponent__float_add, right_exponent__float_add
        jge _left_exp_ge_right__float_add
        xchg left_sign__float_add, right_sign__float_add
        xchg left_exponent__float_add, right_exponent__float_add
        xchg left_mantissa__float_add, right_mantissa__float_add

    _left_exp_ge_right__float_add:
    
    sub esp, 4
    
    exp_diff__float_add equ dword ptr [EBP - 28]
    mov exp_diff__float_add, left_exponent__float_add
    sub exp_diff__float_add, right_exponent__float_add
    
    cmp exp_diff__float_add, 31
        jle _exp_diff_is_at_most_31__float_add
        ; return left
        shl left_sign__float_add, 31
        shl left_exponent__float_add, 23
        mov eax, left_sign__float_add
        or eax, left_exponent__float_add
        or eax, left_mantissa__float_add
        jmp _epilogue__float_add
    
    _exp_diff_is_at_most_31__float_add:
    cmp left_sign__float_add, right_sign__float_add
        jne _different_signs__float_add

        or left_mantissa__float_add,  01000000h ; 1 << 24
        or right_mantissa__float_add, 01000000h ; 1 << 24
    

        

        

        
    
    
    
    
        

        


    
    

    _different_signs__float_add:
    
    

    
    
    



_epilogue__float_add:
    mov esp, ebp
    pop ebp
    ret
float_add endp


main:     
    mov    eax, data
    mov    ds, eax


    mov dx, offset input_str + 1
    push dx

    xor dx, dx
    mov dl, [input_str]
    push dx
    call parse_float

    xor    eax, eax
    mov    ah, 4Ch
    int    21h
code ends
end main

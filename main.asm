.386P
.model flat   
 
data segment use16
    ;input_buf db 26         ;MAX NUMBER OF CHARACTERS ALLOWED (25).
    ;          db ?          ;NUMBER OF CHARACTERS ENTERED BY USER.
    ;          db 26 dup(0)  ;CHARACTERS ENTERED BY USER.
    first_str db (first_EOF - $ - 1)
              db "-0.1"
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
    len EQU di
    mov len, word ptr [ebp + 6]
    
    str_ptr EQU bx
    mov str_ptr, word ptr [ebp + 6 + 2]  ; string ptr
    
    xor eax, eax
    whole_part EQU EAX
    whole_part_l EQU AL 

    xor si, si
    
    cmp byte ptr [str_ptr], '-'
        jne _loop
        mov byte ptr [EBP - 1], 080h  ; 2^7
        inc si
    
    exponent EQU EDX
    xor exponent, exponent
_loop:
    cmp si, len
    je _no_dot
    
    cmp byte ptr [str_ptr + si], '.'
    je _found_dot
    
    cmp byte ptr [str_ptr + si], '0'
    jl _error
    cmp byte ptr [str_ptr + si], '9' 
    jg _error
    
    imul whole_part, whole_part, 10
    add whole_part_l, byte ptr [str_ptr + si]
    sub whole_part_l, '0'
    
    inc si
    jmp _loop

_no_dot:
    mantissa EQU ECX

    xor mantissa,mantissa
    jmp _build_float
    
_found_dot:
    inc si ; skipping the dot
    
    push whole_part
    push mantissa
    
    sub ebp, 12
    push ebp
    add ebp, 12
    
    cmp whole_part, 0
        jle _whole_part_is_empty 
        push 0
        jmp _done_whole_part_cmp
    _whole_part_is_empty:
        push 1
        jmp _done_whole_part_cmp
    _done_whole_part_cmp:
    
    add str_ptr, si
    push str_ptr

    sub len, si
    push len
    
    call parse_mantissa

    pop len
    pop str_ptr
    
    add esp, 2
    add esp, 4

    mov exponent, dword ptr [ebp - 12]

    pop mantissa
    mov mantissa, eax
    pop whole_part

_build_float:
    
_loop2:
    cmp whole_part, 1
        jle _loop2_end 
    
    inc exponent
    
    mov byte ptr [ebp - 5], whole_part_l
    and byte ptr [ebp - 5], 1 
    shl byte ptr [ebp - 5], 7
    
    shr mantissa, 1
    or mantissa, dword ptr [ebp - 8]
    
    shr whole_part, 1
    jmp _loop2
    
_loop2_end:
    add exponent, 127
    shl exponent, 24
    shr exponent, 1
    
    or exponent, dword ptr [ebp - 4]  ; adding a sign bit
    shr mantissa, 9
    or mantissa, exponent

_epilogue:
    mov EAX, mantissa

    ; restoring registers
    pop di
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error:
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
    
    len EQU word PTR [EBP + 6]     
    
    str_ptr EQU bx
    mov str_ptr, WORD PTR [EBP + 6 + 2]  ; str_ptr. points after a dot symbol
    
    still_skipping_flag equ word ptr [ebp + 6 + 4]
    
    exponent_ptr equ EDI
    mov exponent_ptr, DWORD PTR [EBP + 6 + 6]
    mov dword ptr [exponent_ptr], 0
    
    MAX_MANTISSA_SIZE = 23
    cmp len, MAX_MANTISSA_SIZE 
    jle _mantissa_is_at_most_23
        mov len, MAX_MANTISSA_SIZE

_mantissa_is_at_most_23:
    xor si, si 
_normaize_loop:
    cmp si, len
    jge _normaize_loop_end
    
    cmp byte ptr [str_ptr + si], '0'
    jl _error

    cmp byte ptr [str_ptr + si], '9'
    jg _error
    
    sub byte ptr [str_ptr + si], '0'
    inc si
    jmp _normaize_loop
_normaize_loop_end:
    
    has_decimal_part EQU byte ptr [EBP - 1]
    iteration_count EQU byte ptr [EBP - 2]
    mov iteration_count, 0

    mov cl, 31
_decimal_part_outer_start:
    cmp iteration_count, MAX_MANTISSA_SIZE
        je _decimal_part_outer_end

    inc iteration_count

    mov si, len
    dec si
    
    carry equ edx
    xor carry, carry 
    carry_l equ dl

    mov has_decimal_part, 0 
    _decimal_part_inner:
        digit EQU byte ptr [str_ptr + si]
    
        add carry_l, digit 
        add digit, carry_l ; multiply digit by 2 with a carry
        
        cmp digit, 10
        jl _decimal_part_inner_digit_lt_10
            sub digit, 10
            mov carry_l, 1
            jmp _decimal_part_inner_digit_lt_10_done
        _decimal_part_inner_digit_lt_10:
            xor carry_l, carry_l 
            jmp _decimal_part_inner_digit_lt_10_done 
 
       _decimal_part_inner_digit_lt_10_done: 
        cmp digit, 0
            je cmp_done 
            mov has_decimal_part, 1
        cmp_done:
        
        cmp si, 0
        je _decimal_part_inner_end

        dec si
        jmp _decimal_part_inner
_decimal_part_inner_end:    
    cmp has_decimal_part, 1
        jne _no_decimal_part_left

        cmp still_skipping_flag, 1
            jne _after_still_skipping_flag
            cmp carry, 0
                je _carry_cmp_done
                mov still_skipping_flag, 0
        
            _carry_cmp_done:
            dec iteration_count
            dec dword ptr ss:[exponent_ptr]
            jmp _decimal_part_outer_start
            
        _after_still_skipping_flag:
        shl carry, CL
        dec CL
        
        or eax, carry
        jmp _decimal_part_outer_start

    _no_decimal_part_left:
        cmp carry, 0
        je _decimal_part_outer_end

        cmp still_skipping_flag, 1
            jne _after_still_skipping_flag_no_decimal
            dec dword ptr ss:[exponent_ptr]
            jmp _decimal_part_outer_end
            
        _after_still_skipping_flag_no_decimal:
        shl carry, CL
        dec CL
        
        or eax, carry

        jmp _decimal_part_outer_end 
_decimal_part_outer_end:
    ; epilogue
    pop edi
    pop si
    pop bx
    
    mov esp, ebp
    pop ebp
    ret

_error: 
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

    left equ EBX
    right equ EDX
    buffer equ ECX

    mov left, dword ptr [EBP + 6]
    mov right, dword ptr [EBP + 10]

    cmp left, 0
    jne _left_not_trivial
    mov eax, right
    jmp _epilogue

_left_not_trivial:          
    cmp right, 0
    jne _right_not_trivial
    mov eax, left
    jmp _epilogue

_right_not_trivial:         
    left_sign     equ dword ptr [EBP - 4]
    left_exponent equ dword ptr [EBP - 8]
    left_mantissa equ dword ptr [EBP - 12]
    
    right_sign     equ dword ptr [EBP - 16]
    right_exponent equ dword ptr [EBP - 20]
    right_mantissa equ dword ptr [EBP - 24]

    mov left_sign, left
    shr left_sign, 31
    
    mov left_exponent, left
    shl left_exponent, 1
    shr left_exponent, 24
    
    mov left_mantissa, left
    shl left_mantissa, 9
    shr left_mantissa, 9
    
    mov right_sign, right
    shr right_sign, 31
    
    mov right_exponent, right
    shl right_exponent, 1
    shr right_exponent, 24
    
    mov right_mantissa, right
    shl right_mantissa, 9
    shr right_mantissa, 9

_init_done:   
    mov EDI, left
    shr edi, 1
    shl edi, 1

    mov ESI, right
    shr esi, 1
    shl esi, 1

    cmp edi, esi
    jge _left_exp_ge_right
    xchg left, right

    exchange_memory left_sign,     right_sign,     buffer
    exchange_memory left_exponent, right_exponent, buffer
    exchange_memory left_mantissa, right_mantissa, buffer

_left_exp_ge_right:   
    mov buffer, left_exponent
    sub buffer, right_exponent

    cmp buffer, 24
    jle _exp_diff_is_at_most_24
    mov eax, left
    jmp _epilogue
    
_exp_diff_is_at_most_24:    
    or left_mantissa,  0800000h ; 1 << 23
    or right_mantissa, 0800000h ; 1 << 23
                
    shr right_mantissa, cl
    cmp_memory left_sign, right_sign, buffer
    jne _diff_signs 

_same_signs:
    mov eax, left_mantissa
    add eax, right_mantissa
    cmp eax, 01000000h ; 1 << 24 = 10 << 23
        jl _not_greater_2 
        inc left_exponent
        shr eax, 1

    _not_greater_2: 
    xor eax, 0800000h

    shl left_sign, 31
    shl left_exponent, 23
    or eax, left_sign
    or eax, left_exponent
    jmp _epilogue

_diff_signs:                
    mov eax, left_mantissa
    sub eax, right_mantissa
    cmp eax, 0
    je _epilogue

    _while: 
        mov buffer, eax
        and buffer, 0800000h ; 1 << 23. 
        cmp buffer, 0
        jg _while_end
        shl eax, 1
        dec left_exponent
        jmp _while

_while_end: 
    xor eax, 0800000h
    shl left_sign, 31
    shl left_exponent, 23
    or eax, left_sign
    or eax, left_exponent
    jmp _epilogue

_epilogue:
    pop ESI
    pop EDI
    pop EBX

    mov esp, ebp
    pop ebp
    ret
float_add endp


main proc
    mov    eax, data
    mov    ds, eax
    
    mov ebp, esp 
    mov eax, 0
    push eax
    push eax
    
    left equ dword ptr [ebp - 4]
    right equ dword ptr [ebp - 8]
    
    ; ============================
    push offset first_str + 1 
    
    xor dx, dx
    mov dl, byte ptr [first_str] 
    push dx
    call parse_float
    sub esp, 4
    
    mov left, eax

    ; ============================
    push offset second_str + 1 
    
    xor dx, dx
    mov dl, byte ptr [second_str] 
    push dx
    call parse_float
    add esp, 4
    
    mov right, eax
    ; ============================
    
    mov eax, right
    push right
    mov eax, left
    push left
    call float_add
    add esp, 8
    
    xor    eax, eax
    mov    ah, 4Ch
    int    21h
main endp

code ends
end main

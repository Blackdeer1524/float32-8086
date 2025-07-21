.386P
.model flat   
 
data segment use16
    ;input_buf db 26         ;MAX NUMBER OF CHARACTERS ALLOWED (25).
    ;          db ?          ;NUMBER OF CHARACTERS ENTERED BY USER.
    ;          db 26 dup(0)  ;CHARACTERS ENTERED BY USER.
    input_str db 5 
              db "-3.25", "$"

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
    
    sub ESP, 8;
    mov dword ptr [EBP - 4], 0 ; sign data
    mov dword ptr [EBP - 8], 0 ; mantissa buffer
    
    ; callee-safe registers
    push bx
    push si
    
    ; subroutine body 
    len__parse_float EQU dx
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

    xor ECX, ECX
    jmp _build_float__parse_float
    
_found_dot__parse_float:
    push whole_part__parse_float

    inc si
    add str_ptr__parse_float, si
    push str_ptr__parse_float

    sub len__parse_float, si
    push len__parse_float
    
    call parse_mantissa
    pop len__parse_float
    pop str_ptr__parse_float
    
    pop mantissa__parse_float 
    
    xchg mantissa__parse_float, whole_part__parse_float

_build_float__parse_float:
    power__parse_float EQU EDX
    xor power__parse_float, power__parse_float
    
_loop2__parse_float:
    cmp whole_part__parse_float, 1
        jle _loop2_end__parse_float 
    
    inc power__parse_float
    
    mov byte ptr [ebp - 5], whole_part_l__parse_float
    and byte ptr [ebp - 5], 1 ; whole (mod 2)
    shl byte ptr [ebp - 5], 7
    
    shr mantissa__parse_float, 1
    or mantissa__parse_float, dword ptr [ebp - 8]
    
    shr whole_part__parse_float, 1
    jmp _loop2__parse_float
    
_loop2_end__parse_float:
    cmp mantissa__parse_float, 0
    je _epilogue__parse_float
    
    add power__parse_float, 127
    shl power__parse_float, 24
    shr power__parse_float, 1
    
    or power__parse_float, dword ptr [ebp - 4]  ; adding a sign bit
    shr mantissa__parse_float, 9
    or mantissa__parse_float, power__parse_float

_epilogue__parse_float:
    mov EAX, mantissa__parse_float

    ; restoring registers
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error__parse_float:
    call exit_invalid_char 
parse_float endp


parse_mantissa proc  ; (uint16 len, char [data *] str)
    push ebp
    mov ebp, esp
    
    sub ESP, 2
    
    push bx
    push si
    push di
    ; subroutine body

    xor eax, eax ; mantissa
    xor ecx, ecx
    mov cx, word PTR [EBP + 6]      ; str_length
    mov bx, WORD PTR [EBP + 6 + 2]  ; str_ptr. points after a dot symbol
    
    MAX_MANTISSA_SIZE__parse_mantissa = 23
    cmp cl, MAX_MANTISSA_SIZE__parse_mantissa 
    jle _mantissa_is_at_most_23__parse_mantissa
        mov cl, MAX_MANTISSA_SIZE__parse_mantissa

    _mantissa_is_at_most_23__parse_mantissa:
    
    xor si, si 
_normaize_loop__parse_mantissa:
    cmp si, cx
    jge _normaize_loop_end__parse_mantissa
    
    cmp byte ptr [bx + si], '0'
    jl _error__parse_mantissa

    cmp byte ptr [bx + si], '9'
    jg _error__parse_mantissa
    
    sub byte ptr [bx + si], '0'
    inc si
    jmp _normaize_loop__parse_mantissa
_normaize_loop_end__parse_mantissa:

    has_decimal_part__parse_mantissa EQU byte ptr [EBP - 1]
    iteration_count__parse_mantissa EQU byte ptr [EBP - 2]
    mov iteration_count__parse_mantissa, 0

    mov di, 31
_decimal_part_outer_start__parse_mantissa:
    cmp iteration_count__parse_mantissa, MAX_MANTISSA_SIZE__parse_mantissa
        je _decimal_part_outer_end__parse_mantissa

    inc iteration_count__parse_mantissa

    mov si, cx
    dec si
    
    xor edx, edx 
    mov has_decimal_part__parse_mantissa, 0 
    _decimal_part_inner__parse_mantissa:
        digit__parse_mantissa EQU byte ptr [bx + si]
        
        add dl, digit__parse_mantissa 
        add digit__parse_mantissa, dl ; multiply digit__parse_mantissa by 2 with a carry
        
        cmp digit__parse_mantissa, 10
        jl _decimal_part_inner_digit_lt_10__parse_mantissa
            sub digit__parse_mantissa, 10
            mov dl, 1
            jmp _decimal_part_inner_digit_lt_10_done__parse_mantissa
        _decimal_part_inner_digit_lt_10__parse_mantissa:
            xor dl, dl
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

        push CX
        mov CX, DI

        shl EDX, CL
        dec DI

        pop CX
        
        or eax, edx
        jmp _decimal_part_outer_start__parse_mantissa

    _no_decimal_part_left__parse_mantissa:
        cmp EDX, 1
        jne _decimal_part_outer_end__parse_mantissa

        push CX
        mov CX, DI

        shl EDX, CL
        dec DI

        pop CX
        
        or eax, edx
        jmp _decimal_part_outer_end__parse_mantissa 
_decimal_part_outer_end__parse_mantissa:
    ; epilogue
    pop di
    pop si
    pop bx
    
    ADD ESP, 2

    mov esp, ebp
    pop ebp
    ret

_error__parse_mantissa: 
    call exit_invalid_char    
parse_mantissa endp

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

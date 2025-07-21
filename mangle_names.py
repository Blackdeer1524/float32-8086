from enum import Enum, auto
from dataclasses import dataclass, field
import os
import sys
from stat import S_IREAD, S_IRGRP, S_IROTH


class BlockType(Enum):
    PROGRAMM = auto()
    CODE = auto()
    PROC = auto()
    MACRO = auto()


@dataclass
class Block:
    b_type: BlockType
    name: str
    start: int
    end: int = 0  # including termination

    inner: list["Block"] = field(default_factory=list)

    def append(self, other: "Block"): 
        self.inner.append(other)


def get_next_word(line: str, word_start: int) -> tuple[str, int]:
    word_end = word_start
    while word_end < len(line) and line[word_end].isspace():
        word_start += 1
        word_end += 1
    
    # http://teaching.idallen.com/dat2343/01f/notes/reserved_words.htm
    RESERVED_1_LETTER_CHARS = set(("$", "*", "+", ",", ".", "/", "=", "?", "[", "]", ":"))
    if line[word_start:word_end + 1] in RESERVED_1_LETTER_CHARS:
        return line[word_start:word_end + 1], word_end + 1 

    while word_end < len(line) and not (char := line[word_end]).isspace():
        if char in RESERVED_1_LETTER_CHARS:
            break
        word_end += 1

    return line[word_start:word_end], word_end


def parse_programm(programm_text: list[str],
                   block: Block,
                   termination: str | None = None) -> int:
    row = block.start
    while row < len(programm_text):
        code_block = Block(b_type=BlockType.CODE,
                            name=block.name,
                            start=row)
        block.append(code_block)
        row = parse_code(programm_text=programm_text,
                            block=code_block,
                            parent_terminator=termination)

        if row >= len(programm_text):
            break

        line = programm_text[row]
        if line.strip() == termination:
            break

        col = 0
        proc_decl, next_token_start = get_next_word(line=line, word_start=col)
        if proc_decl.startswith(";"): 
            row += 1
            continue

        proc_name, next_token_start = get_next_word(line=line, word_start=next_token_start)
        if proc_name.lower() in ("macro", "proc"):
            proc_name, proc_decl = proc_decl, proc_name

        if (is_proc := (proc_decl.lower() == "proc")) or (proc_decl.lower() == "macro"):
            proc_block = Block(b_type=BlockType.PROC if is_proc else BlockType.MACRO,
                               name=f"{block.name}_{proc_name}",
                               start=row + 1)
            block.append(proc_block)
            row = parse_programm(programm_text=programm_text,
                                 block=proc_block,
                                 termination=f"{proc_name} endp" if is_proc else "endm")
    
    block.end = row
    return row


def parse_code(programm_text: list[str], 
               block: Block,
               parent_terminator: str | None = None) -> int:
    row = block.start
    while row < len(programm_text):
        line = programm_text[row]
        if line.strip() == parent_terminator:
            block.end = row
            return row

        proc_decl, next_token_index = get_next_word(line, 0)
        if proc_decl.startswith(";"):
            row += 1
            continue

        proc_name, next_token_index = get_next_word(line, next_token_index)
        if proc_decl.lower() in ("macro", "proc") or proc_name.lower() in ("macro", "proc"):
            block.end = row + 1  # for enclosed macro mangling
            return row

        row += 1
    block.end = row
    return row


def parse_file(lines: list[str]) -> Block:
    program_block = Block(b_type=BlockType.PROGRAMM,
                          name="",
                          start=0)
    res = parse_programm(programm_text=lines,
                         block=program_block)
    assert res == len(lines), res - len(lines)
    return program_block


def mangle(programm_text: list[str], 
           block: Block,
           enclosing_label_table: dict[str, str],
           enclosing_alias_table: dict[str, str],
           mangle_labels: bool,
           parent_block_type: BlockType | None) -> tuple[dict[str, str], dict[str, str]]:
    if block.b_type == BlockType.CODE:
        label_table: dict[str, str] = {} | enclosing_label_table
        JUMP_INSTRUCTIONS = set((
            "ja", "jae", "jb",
            "jbe", "jc", "jcxz",
            "je", "jg", "jge",
            "jl", "jle", "jna",
            "jnae", "jnb", "jnbe",
            "jnc", "jne", "jng",
            "jnge", "jnl", "jnle",
            "jno", "jnp", "jns",
            "jnz", "jo", "jp",
            "jpe", "jpo", "js",
            "jz", "jcc", "jmp"
            ))

        for i in range(block.start, block.end):
            line = programm_text[i]
            word_start = 0
            word, word_end = get_next_word(line, word_start)

            if word.lower() in JUMP_INSTRUCTIONS:
                label_start = word_end
                label, label_end = get_next_word(line, label_start)
                if mangle_labels and label_table.get(label) is None:
                    label_table[label] = label + "_" + block.name if block.name else label

                if label_table.get(label) is not None:
                    while label_start < len(line) and line[label_start].isspace():
                        label_start += 1  # saves spaces
                    programm_text[i] = line = line[:label_start] + label_table[label] + line[label_end:]
            else:
                semicolon, _ = get_next_word(line, word_end)

                if semicolon != ":":
                    continue

                if mangle_labels and label_table.get(word) is None:
                    label_table[word] = word + "_" + block.name if block.name else word

                if label_table.get(word) is not None:
                    while word_start < len(line) and line[word_start].isspace():
                        word_start += 1  # saves spaces
                    programm_text[i] = line = line[:word_start] + label_table[word] + line[word_end:]  # : is considered

        alias_table: dict[str, str] = {} | enclosing_alias_table
        for i in range(block.start, block.end):
            line = programm_text[i]
            split_line = line.strip().split()

            new_alias = ""
            if len(split_line) >= 2 and (split_line[0].lower() == "macro"):
                new_alias = split_line[1]
            elif parent_block_type != BlockType.MACRO and len(split_line) >= 2 and (split_line[0].lower() == "proc"):
                new_alias = split_line[1]
            elif len(split_line) >= 2 and (split_line[1].lower() == "macro"):
                new_alias = split_line[0]
            elif parent_block_type != BlockType.MACRO and len(split_line) >= 2 and (split_line[1].lower() == "proc"):
                new_alias = split_line[0]
            elif len(split_line) >= 3 and split_line[1].lower() == "equ":
                new_alias = split_line[0]
            elif len(split_line) >= 3 and split_line[1].lower() == "=":
                new_alias = split_line[0]

            if new_alias:
                if alias_table.get(new_alias) is None:
                    alias_table[new_alias] = new_alias + "_" + block.name if block.name else new_alias
                # if redefinition already exists 
                else:  
                    old_alias = alias_table.pop(new_alias)
                    alias_table[new_alias] = f"_{old_alias}"
            
            word_start = 0
            word, word_end = get_next_word(line, word_start)
            while (word):
                if (res := alias_table.get(word)) is not None:
                    while word_start < len(line) and line[word_start].isspace():
                        word_start += 1  # saves spaces
                    programm_text[i] = line = line[:word_start] + res + line[word_end:] 
                    word_start += len(res)
                else:
                    word_start = word_end
                word, word_end = get_next_word(line, word_start)
        
        return label_table, alias_table
    elif block.b_type in (BlockType.PROGRAMM, BlockType.PROC, BlockType.MACRO):
        non_code_blocks = []
        for sub_block in block.inner:
            if sub_block.b_type == BlockType.CODE:
                enclosing_label_table, enclosing_alias_table = \
                    mangle(programm_text=programm_text,
                           block=sub_block, 
                           enclosing_label_table=enclosing_label_table, 
                           enclosing_alias_table=enclosing_alias_table,
                           mangle_labels=block.b_type != BlockType.MACRO,
                           parent_block_type=block.b_type)
            else:
                non_code_blocks.append(sub_block)

        for sub_block in non_code_blocks:
            mangle(programm_text=programm_text,
                   block=sub_block, 
                   enclosing_label_table=enclosing_label_table, 
                   enclosing_alias_table=enclosing_alias_table,
                   mangle_labels=True,
                   parent_block_type=block.b_type)   
                           
        return enclosing_label_table, enclosing_alias_table

    raise NotImplementedError(f"Unknown block type: {block.b_type}")

if __name__ == "__main__":
    file_basename = sys.argv[1]

    DIRECTORY = "./"
    with open(os.path.join(DIRECTORY, f"{file_basename}"), "r") as f:
        lines = f.readlines()

    programm = parse_file(lines)
    mangle(programm_text=lines, block=programm, 
           enclosing_alias_table={}, enclosing_label_table={},
           mangle_labels=True,
           parent_block_type=None)

    with open(os.path.join(DIRECTORY, f"mangled_{file_basename}"), "w") as f:
        f.writelines(lines)
    
    

type uint8

type byte = uint8

type uint160

type address = uint160

type uint256

type machword = uint256

type memory_content =
  | Item8 of uint256
  | Item256 of uint256

type memory = uint256 -> (memory_content option)

type stack = uint256 list

type storage = uint256 -> uint256

type error =
  | OutOfBounds
  | OutOfGas
  | OutOfStack
  | OutOfCode
  | OutOfData
  | InvalidOpcode

type return_type =
  | Normal of uint256
  | Create of uint256
  | Revert

type vmstatus =
  | Running
  | Error of error
  | Finish of return_type

type st_num = (Z.t) * (Z.t)

type bits_inst =
  | AND
  | OR
  | XOR
  | NOT
  | BYTE

let bit_inst_opcode (inst: bits_inst) : Z.t =
  begin match inst with
  | AND -> Z.of_string "22"
  | OR -> Z.of_string "23"
  | XOR -> Z.of_string "24"
  | NOT -> Z.of_string "25"
  | BYTE -> Z.of_string "26"
  end

let bit_inst_num (inst: bits_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | AND -> (Z.of_string "2", Z.one)
  | OR -> (Z.of_string "2", Z.one)
  | XOR -> (Z.of_string "2", Z.one)
  | NOT -> (Z.one, Z.one)
  | BYTE -> (Z.of_string "2", Z.one)
  end

type sign_arith_inst =
  | SDIV
  | SMOD
  | SLT
  | SGT
  | SIGNEXTEND

let sign_arith_inst_opcode (inst: sign_arith_inst) : Z.t =
  begin match inst with
  | SDIV -> Z.of_string "5"
  | SMOD -> Z.of_string "7"
  | SLT -> Z.of_string "18"
  | SGT -> Z.of_string "19"
  | SIGNEXTEND -> Z.of_string "11"
  end

let sign_arith_inst_num (inst: sign_arith_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | SDIV -> (Z.of_string "2", Z.one)
  | SMOD -> (Z.of_string "2", Z.one)
  | SLT -> (Z.of_string "2", Z.one)
  | SGT -> (Z.of_string "2", Z.one)
  | SIGNEXTEND -> (Z.of_string "2", Z.one)
  end

type arith_inst =
  | ADD
  | MUL
  | SUB
  | DIV
  | MOD
  | ADDMOD
  | MULMOD
  | EXP
  | LT
  | GT
  | EQ
  | ISZERO
  | SHA3

let arith_inst_opcode (inst: arith_inst) : Z.t =
  begin match inst with
  | ADD -> Z.one
  | MUL -> Z.of_string "2"
  | SUB -> Z.of_string "3"
  | DIV -> Z.of_string "4"
  | MOD -> Z.of_string "6"
  | ADDMOD -> Z.of_string "8"
  | MULMOD -> Z.of_string "9"
  | EXP -> Z.of_string "10"
  | LT -> Z.of_string "16"
  | GT -> Z.of_string "17"
  | EQ -> Z.of_string "20"
  | ISZERO -> Z.of_string "21"
  | SHA3 -> Z.of_string "32"
  end

let arith_inst_num (inst: arith_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | ADD -> (Z.of_string "2", Z.one)
  | MUL -> (Z.of_string "2", Z.one)
  | SUB -> (Z.of_string "2", Z.one)
  | DIV -> (Z.of_string "2", Z.one)
  | MOD -> (Z.of_string "2", Z.one)
  | ADDMOD -> (Z.of_string "3", Z.one)
  | MULMOD -> (Z.of_string "3", Z.one)
  | EXP -> (Z.of_string "2", Z.one)
  | LT -> (Z.of_string "2", Z.one)
  | GT -> (Z.of_string "2", Z.one)
  | EQ -> (Z.of_string "2", Z.one)
  | ISZERO -> (Z.one, Z.one)
  | SHA3 -> (Z.of_string "2", Z.one)
  end

type info_inst =
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATASIZE
  | CODESIZE
  | GASPRICE
  | EXTCODESIZE
  | RETURNDATASIZE
  | RETURNDATACOPY
  | BLOCKHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT
  | GAS

let info_inst_opcode (inst: info_inst) : Z.t =
  begin match inst with
  | ADDRESS -> Z.of_string "48"
  | BALANCE -> Z.of_string "49"
  | ORIGIN -> Z.of_string "50"
  | CALLER -> Z.of_string "51"
  | CALLVALUE -> Z.of_string "52"
  | CALLDATASIZE -> Z.of_string "54"
  | CODESIZE -> Z.of_string "56"
  | GASPRICE -> Z.of_string "58"
  | EXTCODESIZE -> Z.of_string "59"
  | RETURNDATASIZE -> Z.of_string "61"
  | RETURNDATACOPY -> Z.of_string "62"
  | BLOCKHASH -> Z.of_string "64"
  | COINBASE -> Z.of_string "65"
  | TIMESTAMP -> Z.of_string "66"
  | NUMBER -> Z.of_string "67"
  | DIFFICULTY -> Z.of_string "68"
  | GASLIMIT -> Z.of_string "69"
  | GAS -> Z.of_string "90"
  end

let info_inst_num (inst: info_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | ADDRESS -> (Z.zero, Z.one)
  | BALANCE -> (Z.one, Z.one)
  | ORIGIN -> (Z.zero, Z.one)
  | CALLER -> (Z.zero, Z.one)
  | CALLVALUE -> (Z.zero, Z.one)
  | CALLDATASIZE -> (Z.zero, Z.one)
  | CODESIZE -> (Z.zero, Z.one)
  | GASPRICE -> (Z.zero, Z.one)
  | EXTCODESIZE -> (Z.one, Z.one)
  | RETURNDATASIZE -> (Z.zero, Z.one)
  | RETURNDATACOPY -> (Z.of_string "3", Z.zero)
  | BLOCKHASH -> (Z.one, Z.one)
  | COINBASE -> (Z.zero, Z.one)
  | TIMESTAMP -> (Z.zero, Z.one)
  | NUMBER -> (Z.zero, Z.one)
  | DIFFICULTY -> (Z.zero, Z.one)
  | GASLIMIT -> (Z.zero, Z.one)
  | GAS -> (Z.zero, Z.one)
  end

type memory_inst =
  | MLOAD
  | MSTORE
  | MSTORE8
  | MSIZE
  | CALLDATACOPY
  | CODECOPY
  | EXTCODECOPY

let memory_inst_opcode (inst: memory_inst) : Z.t =
  begin match inst with
  | MLOAD -> Z.of_string "81"
  | MSTORE -> Z.of_string "82"
  | MSTORE8 -> Z.of_string "83"
  | MSIZE -> Z.of_string "89"
  | CALLDATACOPY -> Z.of_string "55"
  | CODECOPY -> Z.of_string "57"
  | EXTCODECOPY -> Z.of_string "60"
  end

let memory_inst_num (inst: memory_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | MLOAD -> (Z.one, Z.one)
  | MSTORE -> (Z.of_string "2", Z.zero)
  | MSTORE8 -> (Z.of_string "2", Z.zero)
  | MSIZE -> (Z.zero, Z.one)
  | CALLDATACOPY -> (Z.of_string "3", Z.zero)
  | CODECOPY -> (Z.of_string "3", Z.zero)
  | EXTCODECOPY -> (Z.of_string "4", Z.zero)
  end

type storage_inst =
  | SLOAD
  | SSTORE

let storage_inst_opcode (inst: storage_inst) : Z.t =
  begin match inst with
  | SLOAD -> Z.of_string "84"
  | SSTORE -> Z.of_string "85"
  end

let storage_inst_num (inst: storage_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | SLOAD -> (Z.one, Z.one)
  | SSTORE -> (Z.of_string "2", Z.zero)
  end

type pc_inst =
  | JUMP
  | JUMPI
  | PC
  | JUMPDEST

let pc_inst_opcode (inst: pc_inst) : Z.t =
  begin match inst with
  | JUMP -> Z.of_string "86"
  | JUMPI -> Z.of_string "87"
  | PC -> Z.of_string "88"
  | JUMPDEST -> Z.of_string "91"
  end

let pc_inst_num (inst: pc_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | JUMP -> (Z.one, Z.zero)
  | JUMPI -> (Z.of_string "2", Z.zero)
  | PC -> (Z.zero, Z.one)
  | JUMPDEST -> (Z.zero, Z.zero)
  end

type stack_inst =
  | POP
  | PUSH of uint256
  | CALLDATALOAD

exception PushError

let stack_inst_opcode (inst: stack_inst) : (Z.t) list =
  begin match inst with
  | POP -> (Z.of_string "80") :: []
  | CALLDATALOAD -> (Z.of_string "53") :: []
  | _ -> assert false (* absurd *)
  end

let stack_inst_num (inst: stack_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | POP -> (Z.one, Z.zero)
  | PUSH _ -> (Z.zero, Z.one)
  | CALLDATALOAD -> (Z.one, Z.one)
  end

type uint4

type dup_inst = uint4


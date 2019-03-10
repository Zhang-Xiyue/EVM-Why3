type memory_content =
  | Item8 of Z.t
  | Item256 of Z.t

type memory = (Z.t) -> (memory_content option)

type stack = (Z.t) list

type storage = (Z.t) -> (Z.t)

type error =
  | OutOfBounds
  | OutOfGas
  | OutOfStack
  | OutOfCode
  | OutOfData
  | InvalidOpcode

type return_type =
  | Normal of Z.t
  | Create of Z.t
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
  | PUSH of Z.t
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

type dup_inst = Z.t

let dup_inst_opcode (inst: Z.t) : Z.t = Z.add inst (Z.of_string "128")

let dup_inst_num (inst: Z.t) : (Z.t) * (Z.t) = (inst, Z.add inst Z.one)

type swap_inst = Z.t

let swap_inst_opcode (inst: Z.t) : Z.t = Z.add inst (Z.of_string "144")

let swap_inst_num (inst: Z.t) : (Z.t) * (Z.t) =
  (Z.add inst Z.one, Z.add inst Z.one)

type log_inst =
  | LOG0
  | LOG1
  | LOG2
  | LOG3
  | LOG4

let log_inst_opcode (inst: log_inst) : Z.t =
  begin match inst with
  | LOG0 -> Z.of_string "160"
  | LOG1 -> Z.of_string "161"
  | LOG2 -> Z.of_string "162"
  | LOG3 -> Z.of_string "163"
  | LOG4 -> Z.of_string "164"
  end

let log_inst_num (inst: log_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | LOG0 -> (Z.of_string "2", Z.zero)
  | LOG1 -> (Z.of_string "3", Z.zero)
  | LOG2 -> (Z.of_string "4", Z.zero)
  | LOG3 -> (Z.of_string "5", Z.zero)
  | LOG4 -> (Z.of_string "6", Z.zero)
  end

type system_inst =
  | STOP
  | CREATE
  | CALL
  | CALLCODE
  | RETURN
  | DELEGATECALL
  | STATICCALL
  | REVERT
  | SELFDESTRUCT

let system_inst_opcode (inst: system_inst) : Z.t =
  begin match inst with
  | STOP -> Z.zero
  | CREATE -> Z.of_string "240"
  | CALL -> Z.of_string "241"
  | CALLCODE -> Z.of_string "242"
  | RETURN -> Z.of_string "243"
  | DELEGATECALL -> Z.of_string "244"
  | STATICCALL -> Z.of_string "250"
  | REVERT -> Z.of_string "253"
  | SELFDESTRUCT -> Z.of_string "255"
  end

let system_inst_num (inst: system_inst) : (Z.t) * (Z.t) =
  begin match inst with
  | STOP -> (Z.zero, Z.zero)
  | CREATE -> (Z.of_string "3", Z.one)
  | CALL -> (Z.of_string "7", Z.one)
  | CALLCODE -> (Z.of_string "7", Z.one)
  | RETURN -> (Z.of_string "2", Z.zero)
  | DELEGATECALL -> (Z.of_string "6", Z.one)
  | STATICCALL -> (Z.of_string "6", Z.one)
  | REVERT -> (Z.of_string "2", Z.zero)
  | SELFDESTRUCT -> (Z.one, Z.zero)
  end

type instruction =
  | Bits of bits_inst
  | Sarith of sign_arith_inst
  | Arith of arith_inst
  | Info of info_inst
  | Dup of Z.t
  | Memory of memory_inst
  | Storage of storage_inst
  | Pc of pc_inst
  | Stack of stack_inst
  | Swap of Z.t
  | Log of log_inst
  | System of system_inst

let inst_opcode (inst: instruction) : (Z.t) list =
  begin match inst with
  | Bits b -> (bit_inst_opcode b) :: []
  | Sarith s -> (sign_arith_inst_opcode s) :: []
  | Arith a -> (arith_inst_opcode a) :: []
  | Info i -> (info_inst_opcode i) :: []
  | Dup d -> (dup_inst_opcode d) :: []
  | Memory m -> (memory_inst_opcode m) :: []
  | Storage s -> (storage_inst_opcode s) :: []
  | Pc p -> (pc_inst_opcode p) :: []
  | Stack s -> stack_inst_opcode s
  | Swap s -> (swap_inst_opcode s) :: []
  | Log l -> (log_inst_opcode l) :: []
  | System m -> (system_inst_opcode m) :: []
  end

let inst_stack_num (inst: instruction) : (Z.t) * (Z.t) =
  begin match inst with
  | Bits b -> bit_inst_num b
  | Sarith s -> sign_arith_inst_num s
  | Arith a -> arith_inst_num a
  | Info i -> info_inst_num i
  | Dup d -> dup_inst_num d
  | Memory m -> memory_inst_num m
  | Storage s -> storage_inst_num s
  | Pc p -> pc_inst_num p
  | Stack s -> stack_inst_num s
  | Swap s -> swap_inst_num s
  | Log l -> log_inst_num l
  | System m -> system_inst_num m
  end

let gas_of_Wzero (inst: instruction) : Z.t =
  begin match inst with
  | System STOP | System RETURN | System REVERT -> Z.zero
  | _ -> assert false (* absurd *)
  end

let gas_of_Wlow (inst: instruction) : Z.t =
  begin match inst with
  | Arith MUL | Arith DIV | Sarith SDIV | Arith MOD | Sarith SMOD | Sarith SIGNEXTEND ->
    Z.of_string "5"
  | _ -> assert false (* absurd *)
  end

type machine_state = {
  mac_stack: (Z.t) list;
  mac_memory: (Z.t) -> (memory_content option);
  mac_pc: Z.t;
  mac_status: vmstatus;
  mac_memory_usage: Z.t;
  mac_gas: Z.t;
  mac_insts: instruction list;
  mac_jumpmap: (Z.t) -> (Z.t);
  }

let update_memory_content (m: (Z.t) -> (memory_content option)) (idx: Z.t)
                          (cont: memory_content option) : (Z.t) -> (memory_content option)
  =
  fun (addr: Z.t) ->
    if Z.equal addr idx then begin cont end else begin m addr end

let push_stack (s: (Z.t) list) (ele: Z.t) : (Z.t) list = ele :: s

let pop_stack (s: (Z.t) list) : ((Z.t) list) * ((Z.t) option) =
  begin match s with
  | x :: t -> (t, Some x)
  | _ -> (s, None)
  end

let rec fetch (lst: (Z.t) list) (n: Z.t) : (Z.t) list =
  if (Z.gt n (Z.of_int (List.length lst))) || (Z.equal n Z.zero) then begin
    [] end
  else
  begin
    begin match lst with
    | [] -> []
    | x :: r -> x :: (fetch r (Z.sub n Z.one))
    end end

let rec drop (lst: (Z.t) list) (n: Z.t) : (Z.t) list =
  if Z.equal n Z.zero then begin lst end
  else
  begin
    List.rev (fetch (List.rev lst) (Z.sub (Z.of_int (List.length lst)) n)) end

let swap_stack (s: (Z.t) list) (i: Z.t) : ((Z.t) list) option =
  begin match (List.nth Z.zero s, List.nth i s) with
  | (Some ele_0, Some ele_i) ->
    Some (List.append (ele_i :: (fetch (drop s Z.one) (Z.sub i Z.one))) (ele_0 :: (
    drop s i)))
  | (_, _) -> None
  end

let update_stack (st: (Z.t) list) (m: machine_state) : machine_state =
  { mac_stack = st; mac_memory = (m.mac_memory); mac_pc = (m.mac_pc);
    mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
    mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
    (m.mac_jumpmap) }

let update_memory (memo: (Z.t) -> (memory_content option)) (m: machine_state) : machine_state
  =
  { mac_stack = (m.mac_stack); mac_memory = memo; mac_pc = (m.mac_pc);
    mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
    mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
    (m.mac_jumpmap) }

let update_pc (pc: Z.t) (m: machine_state) : machine_state =
  { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc = pc;
    mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
    mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
    (m.mac_jumpmap) }

let update_status (vst: vmstatus) (m: machine_state) : machine_state =
  { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
    (m.mac_pc); mac_status = vst; mac_memory_usage = (m.mac_memory_usage);
    mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
    (m.mac_jumpmap) }

let inc_pc (m: machine_state) : machine_state =
  update_pc (Z.add (m.mac_pc) Z.one) m

let get_inst (mac_st: machine_state) : instruction option =
  let (pc, insts) = (mac_st.mac_pc, mac_st.mac_insts) in List.nth pc insts

let interpreter (m: machine_state) : machine_state =
  let inst = get_inst m in
  begin match inst with
  | Some System STOP ->
    { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
      (m.mac_pc); mac_status = (Finish (Normal Z.zero)); mac_memory_usage =
      (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
      mac_jumpmap = (m.mac_jumpmap) }
  | Some Arith ADD ->
    let (stqt, a) = pop_stack (m.mac_stack) in let (stqtqt, b) =
    pop_stack stqt in
    begin match (a, b) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt (Z.add aqt bqt)); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith MUL ->
    let (stqt1, a1) = pop_stack (m.mac_stack) in let (stqtqt1, b1) =
    pop_stack stqt1 in
    begin match (a1, b1) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt1 (Z.mul aqt bqt)); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith SUB ->
    let (stqt2, a2) = pop_stack (m.mac_stack) in let (stqtqt2, b2) =
    pop_stack stqt2 in
    begin match (a2, b2) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt2 (Z.sub aqt bqt)); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith DIV ->
    let (stqt3, a3) = pop_stack (m.mac_stack) in let (stqtqt3, b3) =
    pop_stack stqt3 in
    begin match (a3, b3) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt3 (Z.div aqt bqt)); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith MOD ->
    let (stqt4, a4) = pop_stack (m.mac_stack) in let (stqtqt4, b4) =
    pop_stack stqt4 in
    begin match (a4, b4) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt4 (Z.rem aqt bqt)); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith EXP ->
    let (stqt5, a5) = pop_stack (m.mac_stack) in let (stqtqt5, b5) =
    pop_stack stqt5 in
    begin match (a5, b5) with
    | (Some aqt, Some bqt) ->
      { mac_stack = (push_stack stqtqt5 (power aqt bqt)); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Arith LT ->
    let (stqt6, a6) = pop_stack (m.mac_stack) in let (stqtqt6, b6) =
    pop_stack stqt6 in
    begin match (a6, b6) with
    | (Some aqt, Some bqt) ->
      { mac_stack =
        (push_stack stqtqt6
           (if Z.lt aqt bqt then begin Z.one end else begin Z.zero end));
        mac_memory = (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one);
        mac_status = (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
        mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Stack POP ->
    let (stqt7, a7) = pop_stack (m.mac_stack) in
    begin match a7 with
    | Some _ ->
      { mac_stack = stqt7; mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Stack PUSH a8 ->
    inc_pc ({ mac_stack = (push_stack (m.mac_stack) a8); mac_memory =
              (m.mac_memory); mac_pc = (m.mac_pc); mac_status =
              (m.mac_status); mac_memory_usage = (m.mac_memory_usage);
              mac_gas = (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
              (m.mac_jumpmap) })
  | Some Dup i ->
    let ele = List.nth (Z.sub i Z.one) (m.mac_stack) in
    begin match ele with
    | Some a8 ->
      { mac_stack = (push_stack (m.mac_stack) a8); mac_memory =
        (m.mac_memory); mac_pc = (Z.add (m.mac_pc) Z.one); mac_status =
        (m.mac_status); mac_memory_usage = (m.mac_memory_usage); mac_gas =
        (m.mac_gas); mac_insts = (m.mac_insts); mac_jumpmap =
        (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Swap i ->
    let s = swap_stack (m.mac_stack) i in
    begin match s with
    | Some uss ->
      { mac_stack = uss; mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Pc JUMP ->
    let (sqt, dest) = pop_stack (m.mac_stack) in
    begin match dest with
    | Some usdest ->
      { mac_stack = sqt; mac_memory = (m.mac_memory); mac_pc =
        ((m.mac_jumpmap) usdest); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (m.mac_pc); mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Pc JUMPI ->
    let (sqt1, dest1) = pop_stack (m.mac_stack) in let (sqtqt, con) =
    pop_stack sqt1 in
    begin match (dest1, con) with
    | (Some usdest, Some uscon) ->
      if Z.equal uscon Z.zero then begin
        inc_pc ({ mac_stack = sqtqt; mac_memory = (m.mac_memory); mac_pc =
                  (m.mac_pc); mac_status = (m.mac_status); mac_memory_usage =
                  (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
                  (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }) end
      else
      begin
        { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
          ((m.mac_jumpmap) usdest); mac_status = (m.mac_status);
          mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
          mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) } end
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (m.mac_pc); mac_status = (Error OutOfStack); mac_memory_usage =
        (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts =
        (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Pc JUMPDEST -> inc_pc m
  | Some Memory MSTORE ->
    let (sqt2, offset) = pop_stack (m.mac_stack) in let (sqtqt1, cont) =
    pop_stack sqt2 in
    begin match (offset, cont) with
    | (Some o, Some c) ->
      { mac_stack = sqtqt1; mac_memory =
        (update_memory_content (m.mac_memory) o (Some (Item256 c))); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Memory MSTORE8 ->
    let (sqt3, offset1) = pop_stack (m.mac_stack) in let (sqtqt2, cont1) =
    pop_stack sqt3 in
    begin match (offset1, cont1) with
    | (Some o, Some c) ->
      { mac_stack = sqtqt2; mac_memory =
        (update_memory_content (m.mac_memory) o (Some (Item8 c))); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | (_, _) ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    end
  | Some Stack CALLDATALOAD ->
    let (sqt4, offset2) = pop_stack (m.mac_stack) in
    begin match offset2 with
    | Some _ ->
      { mac_stack = (push_stack sqt4 Z.one); mac_memory = (m.mac_memory);
        mac_pc = (Z.add (m.mac_pc) Z.one); mac_status = (m.mac_status);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | None ->
      { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
        (Z.add (m.mac_pc) Z.one); mac_status = (Error OutOfStack);
        mac_memory_usage = (m.mac_memory_usage); mac_gas = (m.mac_gas);
        mac_insts = (m.mac_insts); mac_jumpmap = (m.mac_jumpmap) }
    | _ -> assert false (* absurd *)
    end
  | _ ->
    { mac_stack = (m.mac_stack); mac_memory = (m.mac_memory); mac_pc =
      (m.mac_pc); mac_status = (Error InvalidOpcode); mac_memory_usage =
      (m.mac_memory_usage); mac_gas = (m.mac_gas); mac_insts = (m.mac_insts);
      mac_jumpmap = (m.mac_jumpmap) }
  end


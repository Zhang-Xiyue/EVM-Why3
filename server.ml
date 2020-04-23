open Evm;;
open Common;;


exception UnrecognizedOpcode of string


let parse_contract (contract: string) : (instruction list) * (Z.t -> Z.t) =
    let pos : int ref = ref 0 in
    let insts : instruction list ref = ref [] in

    let get_opcode () : int =
        let substr = String.sub contract !pos 2 in
        let opcode = int_of_string ("0x" ^ substr) in
        pos := !pos + 2;
        opcode
    in
    
    let get_integer (size: int) : Z.t =
        let ret = ref (Big_int.zero_big_int) in
        for i = 1 to size do
            let open Big_int in
            ret := add_int_big_int (get_opcode ()) (mult_int_big_int 256 !ret);
        done;
        !ret
    in

    let append_inst (inst: instruction) =
        insts := !insts @ [inst]
    in

    while (!pos < String.length contract) do
        let opcode = get_opcode () in
        append_inst begin
            match opcode with
            | 0x00 -> (* STOP *)
                System STOP
            | 0x01 -> (* ADD *)
                Arith ADD
            | 0x55 -> (* SSTORE *)
                Storage SSTORE
            (* start of PUSHXXX *)
            | pushop when (0x60 <= pushop && pushop < 0x80) -> begin
                let nwords = (pushop - 0x60 + 1) in
                let operand = get_integer nwords in
                Stack (PUSH operand)
            end
            | _ ->
                raise (UnrecognizedOpcode Printf.(sprintf "0x%x" opcode))
        end
    done;

    !insts, (fun x -> x)

let _ = 
    try
        while true do
            let contract = read_line () in
            let insts, jumpmap = parse_contract contract in
            let vm = init_vm insts jumpmap in
            let result = run_vm vm in
            match result with
            | Error err -> begin
                let message =
                    match err with
                    | OutOfBounds -> "OutOfBounds"
                    | OutOfGas -> "OutOfGas"
                    | OutOfCode -> "OutOfCode"
                    | OutOfData -> "OutOfData"
                    | OutOfStack -> "OutOfStack"
                    | InvalidOpcode -> "InvalidOpcode"
                in
                Printf.printf "{ \"error\": true, \"message\": \"%s\" }\n" message
            end
            | Finish _ -> Printf.printf "{ \"error\": false }\n"
            | _ -> assert false
        done
    with End_of_file ->
        ()

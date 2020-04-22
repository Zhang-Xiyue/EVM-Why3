open Evm;;
open Common;;

let parse_contract (contract: string) : (instruction list) * (Z.t -> Z.t) =
    [], (fun x -> x)

let _ = 
    Printf.printf "EVM Running As Cli Server ...\n";

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

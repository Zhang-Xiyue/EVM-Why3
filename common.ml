open Evm;;


let empty_vm () : machine_state = {
    mac_stack = [];
    mac_memory = (fun _ -> None);
    mac_pc = Z.zero;
    mac_status = Running;
    mac_memory_usage = Z.zero;
    mac_gas = Z.zero;
    mac_insts = [];
    mac_jumpmap = (fun _ -> Z.zero)
}

let init_vm insts jumpmap : machine_state = {
    mac_stack = [];
    mac_memory = (fun _ -> None);
    mac_pc = Z.zero;
    mac_status = Running;
    mac_memory_usage = Z.zero;
    mac_gas = Z.zero;
    mac_insts = insts;
    mac_jumpmap = jumpmap
}

let run_vm (vm: machine_state) : vmstatus =
    let rec run vm =
        let next = (interpreter vm) in
        match (next.mac_status) with
        | Running -> (run next)
        | _ -> next
    in
    let final = run vm in
    final.mac_status

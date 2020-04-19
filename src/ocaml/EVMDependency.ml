open Big_int;;

module Z = struct
    type t = big_int

    let of_int = big_int_of_int
    let to_int = int_of_big_int

    let add = add_big_int
    let sub = sub_big_int
    let mul = mult_big_int
    let div = div_big_int
    let ediv = div

    let zmod = mod_big_int
    let erem = zmod

    let eq = eq_big_int
    let gt = gt_big_int
    let ge = ge_big_int
    let le = le_big_int
    let lt = lt_big_int

    let negate a = sub (of_int 0) a

end

let power = power_big_int_positive_big_int

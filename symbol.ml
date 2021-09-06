type sym = int

let a = 0

let nb_syms = 26

let of_char c =
	let n = Char.code c in
	if n < 123 && n > 96
		then n - 97
		else failwith "ce n'est pas une lettre majuscule!"

let of_int n =
	if n < 123 && n > 96
		then Char.chr (n + 97)
		else failwith "ce n'est pas un nombre entre 0 et 25!"

let next x =
	if x = 25
	then 0
	else x + 1
	
let (++) x y = x + y mod 26

let (--) x y = x - y mod 26

let rec mk_syms n =
	match n with
	| 0 -> []
	| n' -> (25 - n') :: (mk_syms (n' - 1))

let syms = mk_syms 25

let iter f = List.iter f syms

let fold f x = List.fold_left f syms

module Set =
	struct
	
	type t = int
	
	let empty = 0
	
	let rec member x s =
		match s with
		| 0 -> x mod 2 = 1
		| s' -> member (x / 2) (s - 1)
	
	let rec pow n p =
		match p with
		| 0 -> 1
		| p' -> pow n (p - 1) * n
	
	let add x s = if not (member x s) then pow 2 x + s else s
	
	end	

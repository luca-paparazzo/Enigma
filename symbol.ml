type sym = int

let a = 0

let nb_syms = 26

let of_char c : sym =
	let n = Char.code c in
	if n < 91 && n > 64
		then n - 65
		else failwith "ce n'est pas une lettre majuscule!"

let to_char (n : sym) = Char.chr(n + 65)

let of_int n : sym =
	if n < 26 && n > -1
		then n
		else failwith "ce n'est pas un nombre entre 0 et 25!"

let to_int (s : sym) = s

let next x =
	if x = 25
	then 0
	else x + 1
	
let (++) x y = (x + y) mod 26

let (--) x y = (x - y + 26) mod 26

let rec mk_syms n : sym list =
	match n with
	| 0 -> []
	| n' -> (25 - n') :: (mk_syms (n' - 1))

let syms = mk_syms 25

let iter f = f 0; f 1; f 2; f 3; f 4; f 5; f 6; f 7; f 8; f 9; f 10; f 11; f 12; f 13; f 14; f 15; f 16; f 17; f 18; f 19; f 20; f 21; f 22; f 23; f 24; f 25; () 

let fold f x = List.fold_left f x syms

module Set =
	struct
	
	type t = int
	
	let empty = 0
	
	let rec member x s =
		match s with
		| 0 -> x mod 2 = 1
		| s' -> member (x / 2) (s' - 1)
	
	let rec pow n p =
		match p with
		| 0 -> 1
		| p' -> pow n (p' - 1) * n
	
	let add x s = if not (member x s) then pow 2 x + s else s
	
	let singleton x = pow 2 x
	
	end	

module Map =
	struct
	
	type 'a t = 'a array
	
	let get (tab : 'a t)  (s : sym) = tab.(s)
	
	let set tab s x = tab.(s) <- x
	
	let make x = Array.make 26 x
	
	let init f = Array.init 26 f
	
	let copy tab =
		let new_tab = Array.make 26 tab.(0) in
		for i = 1 to 25 do
			new_tab.(i) <- tab.(i)
		done; new_tab
	
	let map (f : 'a -> 'b) (tab : 'a t) : 'b t =
		let new_tab = make (f tab.(0)) in
		for i = 1 to 25 do
			new_tab.(i) <- (f tab.(i))
		done; new_tab
	
	let inverse map =
		let new_map = copy map in
		for i = 0 to 25 do
			new_map.(map.(i)) <- i
		done; new_map
	
	let print_tmap out (map : sym t) =
		for i = 0 to 25 do
			output_char out (to_char map.(i))
		done		
		
	end

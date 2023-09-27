open Imp.Control;;

type 'a lTree = Leaf of 'a | Node of ((('a lTree) Lazy.t) * (('a lTree) Lazy.t));;

let rec subst (x : 'a lTree) (f : 'a -> 'b lTree) : 'b lTree = match x with
  | Leaf a -> f a
  | Node (l, r) -> Node (lazy (subst (Lazy.force l) f), lazy (subst (Lazy.force r) f));;


implicit module MonlTree : sig 
  include Functor with type 'a t = 'a lTree
  include Applicative with type 'a t := 'a t 
  include Monad with type 'a t := 'a t
end = struct 
  type 'a t = 'a lTree
  let fmap f x = subst x (fun a -> Leaf (f a))
  let return a = Leaf a
  let bind x f = subst x f
  let apply fs xs = bind fs (fun f -> bind xs (fun x -> return (f x)))
end;;

let rec fullTree (x : int) : int lTree = if x = 1 then Leaf 1 else 
                  (fullTree (x - 1)) >>= (fun i -> Node (lazy (Leaf (x - 1 - i)), lazy (Leaf (i + 1))));;

let zigzag (x : int lTree) : int = 
  let rec zig = function 
    | Leaf a -> a
    | Node (l, _) -> zag (Lazy.force l)
and 
  zag = function 
    | Leaf a -> a
    | Node (_, r) -> zig (Lazy.force r)
in 
  zig x;;  

let z = zigzag (fullTree 100);;

type 'a cTree = {p : 'b . ('a -> 'b lTree) -> 'b lTree};;

let rep (t : 'a lTree) : 'a cTree = {p = (fun f -> subst t f)};;

let abs (c : 'a cTree) : 'a lTree = c.p (fun x -> Leaf x)


implicit module MonCTree : sig 
  include Functor with type 'a t = 'a cTree
  include Applicative with type 'a t := 'a t
  include Monad with type 'a t := 'a t
end = struct 
  type 'a t = 'a cTree
  let bind x f = {p = (fun h -> x.p (fun a -> (f a).p h))};;
  let return (a : 'a) : 'a t = {p = (fun h -> h a)}
  let apply fs xs = bind fs (fun f -> bind xs (fun x -> return (f x)));;
  let fmap f x = bind x (fun i -> return (f i))
end;;


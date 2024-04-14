(* Projet INF 201*)
(* Étudiant 1 : JULY Héloïse     Étudiant 2 : PEAKE James*)       

(*PARTIE III*)
(*Q1*)

(*implémentation des types *)

type nat = int                             (*restreint aux entiers positifs*);;
type 'a multielement = 'a * nat            (*'a = réservoir des valeurs*);;
type 'a multiensemble =                    (*'a = réservoir des valeurs*)
|V                                         (*V mis pour "vide"*)
|A of 'a multielement * 'a multiensemble   (*A mis pour "ajout"*) ;;


(*définition de plusieurs constantes pour les jeux d'essais*)

let cst_test: nat multiensemble = A((1,2),A((3,2),A((4,1),A((6,3),V))));;
let cst_test2: nat multiensemble = A((3,2),A((4,1),V));;
let cst_test3: nat multiensemble = A((4,1),A((3,1),A((6,2),V)));;
let cst_nul: nat multiensemble = V ;;


(*fonction cardinalité*)
let rec cardinal (ens:'a multiensemble) : nat =
match ens with
|A((_,m),l)-> let suite = cardinal l in m+suite
|V-> 0
;;
assert (cardinal cst_test = 8) ;;
assert (cardinal cst_nul = 0) ;;


(*fonction nombre d'occurences*)
let rec nbocc (elem:'a )(ens:'a multiensemble) : nat =
match ens with
|A((e,m),l)-> if e=elem then m else nbocc elem l
|V-> 0
;;
assert (nbocc 4 cst_test = 1);;
assert (nbocc 1 cst_test3 = 0);;


(*fonction appartenance*)
let rec appartient (elem:'a)(ens:'a multiensemble) : bool =
match ens with
|A((e,m),l)-> if e=elem then true else appartient elem l
|V-> false
;;
assert ( appartient 1 cst_test = true) ;;
assert (appartient 1 cst_nul = false) ;;


(*fonction inclusion*)
let rec inclus (ens1:'a multiensemble)(ens2: 'a multiensemble) : bool =
match ens1 with
|A((e,m),l)-> (appartient e ens2) && (inclus l ens2)
|V->true
;;
assert (inclus cst_nul cst_test2 = true) ;;
assert (inclus cst_test3 cst_test = true) ;;
assert (inclus cst_test3 cst_test2 = false) ;;


(*fonction ajout*)
let rec ajout (elt:'a multielement) (ens:'a multiensemble): 'a multiensemble = 
let (x,m) = elt in
match ens with
|A((elem,multipl),l)-> if x = elem then A((elem,multipl+m),l) else A((elem,multipl),ajout elt l) 
|V-> A((x,m),V)
;;
assert (ajout (3,1) cst_test2 = A((3,3),A((4,1),V)) );;
assert (ajout (1,1) cst_nul = A((1,1),V) );;


(*fonction suppression*)
let rec supprime (elt:'a multielement) (ens:'a multiensemble): 'a multiensemble = 
let (x,m) = elt in
match ens with
|A((elem,multipl),l)-> if x = elem  then  if m>multipl then l else A((elem,multipl-m),l) else A((elem,multipl),supprime elt l) 
|V->failwith "liste vide "
;;
assert (supprime (6,2) cst_test = A((1,2),A((3,2),A((4,1),A((6,1),V)))) );;
assert (supprime (4,4) cst_test =A((1,2),A((3,2),A((6,3),V))) )  ;;


(*fonction égalité*)
let rec egaux (ens1:'a multiensemble) (ens2:'a multiensemble): bool =
match ens1 with
 |A((e,m),l)-> if appartient e ens2 then if nbocc e ens1 = nbocc e ens2 then egaux l ens2 else false else false
 |V-> ens2 = V 
 ;;
 assert (egaux cst_test2 cst_test3 = false);;
 assert (egaux cst_nul cst_nul = true);;


(*fonction intersection*)
let rec intersection (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble = 
match ens1 with
|A((x,m),l)-> if appartient x ens2 then A((x,m),intersection l ens2) else (intersection l ens2)
|V->V
;;
assert (intersection cst_nul cst_test = cst_nul ) ;;
assert (intersection cst_test cst_test2 = A((3, 2),A((4, 1),V)) ) ;;


(*fonction différence*)
let rec difference (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =  
match ens1 with
|A((x,m),l)-> let nbens1 = nbocc x ens1 in let nbens2 = nbocc x ens2 in if nbens1 > nbens2 then ajout (x, nbens1 - nbens2) (difference l ens2) else (difference l ens2)
|V->V 
;;
assert (difference cst_test cst_test2 = A((6,3), A((1,2),V)) ) ;;
assert (difference cst_test cst_test3 =  A ((6, 1), A ((3, 1), A ((1, 2), V))) );;


(*fonction obtention aléatoire*)
(*fonction intermédiaire*)
let rec ieme (n:nat) (ens:'a multiensemble):'a =
match ens with
|A((x,m),l)-> let nb = nbocc x ens in if n>nb then ieme (n-nb) l else x
|V -> failwith "liste vide"
;;
assert (ieme 3 cst_test = 3) ;;
assert (ieme 1 cst_test2 = 3 ) ;;

let un_dans (ens:'a multiensemble) : 'a =
let k = cardinal ens in let nb = Random.int(k) in ieme nb ens ;;
(*on ne peut pas faire de jeux d'essais pour cette fonction car elle est aléatoire*)




(*PARTIE IV*)
(*Q2*)

(*nouvelle implémentation du type multiensemble*)

type nat = int                             (*restreint aux entiers positifs*);;
type 'a multielement = 'a * nat            (*'a = réservoir des valeurs*);;
type 'a multiensemble = 'a multielement list;;


(*définition de plusieurs constantes pour les jeux d'essais*)

let cst_test: nat multiensemble = [(1,2);(3,2);(4,1);(6,3)];;
let cst_test2: nat multiensemble = [(3,2);(4,1)];;
let cst_test3: nat multiensemble = [(4,1);(3,1);(6,2)];;
let cst_test4: nat multiensemble = [(1,2);(3,2);(4,1);(6,2)];;
let cst_nul: nat multiensemble = [] ;;


(*fonction cardinalité*)
let rec cardinal (ens:'a multiensemble) : nat =
match ens with
|(e,m)::l-> let suite = cardinal l in m+suite
|[]-> 0
;;
assert (cardinal cst_test = 8);;
assert (cardinal cst_test2 = 3);;


(*fonction nombre d'occurences*)
let rec nbocc (elem:'a )(ens:'a multiensemble) : nat =
match ens with
|(e,m)::l-> if e = elem then m else nbocc elem l
|[]-> 0
;;
assert (nbocc 4 cst_test = 1);;
assert (nbocc 6 cst_test = 3);;


(*fonction appartenance*)
let rec appartient (elem:'a )(ens:'a multiensemble) : bool =
match ens with
|(e,m)::l-> if e = elem then true else appartient elem l
|[]-> false
;;
assert (appartient 4 cst_test = true);;
assert (appartient 9 cst_test = false);;


(*fonction inclusion*)
let rec inclus (ens1:'a multiensemble)(ens2: 'a multiensemble) : bool =
match ens1 with
|(e,m)::l-> (appartient e ens2) && (inclus l ens2)
|[]->true
;;
assert (inclus cst_test2 cst_test = true);;
assert (inclus cst_test3 cst_test = true);;


(*fonction ajout*)
let rec ajout (elt:'a multielement) (ens:'a multiensemble): 'a multiensemble = 
let (x,multipl) = elt in
match ens with
|(e,m)::l->  if e = x then (e,multipl+m)::l else (e,m)::ajout elt l
|[]->  (x,multipl)::[]
;;
assert (ajout (2,2) cst_test = [(1,2);(3,2);(4,1);(6,3);(2,2)]);;
assert (ajout (4,2) cst_test =[(1,2);(3,2);(4,3);(6,3)]);;


(*fonction supprime*)
let rec supprime (elt:'a multielement) (ens:'a multiensemble): 'a multiensemble =
  let (x,m) = elt in
  match ens with
  |(elem,multipl)::l-> if x = elem  then  if m>multipl then l else (elem,multipl-m)::l else (elem,multipl)::supprime elt l
  |[]->failwith "liste vide "
;;
assert (supprime (6,2) cst_test = [(1,2);(3,2);(4,1);(6,1)]);;
assert (supprime (4,4) cst_test =[(1,2);(3,2);(6,3)]);;


(*fonction égalité*)
let rec egaux (ens1:'a multiensemble) (ens2:'a  multiensemble): bool =
 match ens1 with
 |(e,m)::l-> if appartient e ens2 then if nbocc e ens1 = nbocc e ens2 then egaux l ens2 else false else false
 |[]-> ens2 = []
 ;;
assert (egaux cst_test cst_test4 = false);;
assert (egaux cst_nul cst_nul = true);;


(*fonction intersection*)
let rec intersection (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble = 
match ens1 with
(e,m)::l-> if appartient e ens2 then (e,m)::intersection l ens2 else (intersection l ens2)
|[]->[]
;;
assert (intersection cst_test cst_test2 = [(3, 2); (4, 1)]);;
assert (intersection cst_test cst_test3 =[(3, 2); (4, 1); (6, 3)]);;


let rec difference (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =  
match ens1 with
|(x,m)::l-> let nbens1 = nbocc x ens1 in let nbens2 = nbocc x ens2 in if nbens1 > nbens2 then (x, nbens1 - nbens2):: difference l ens2 else (difference l ens2)
|[]->[] 
;;
assert (difference cst_test cst_test2 = [(1, 2); (6, 3)] );;
assert (difference cst_test cst_test3 =[(1, 2); (3, 1); (6, 1)] );;


(*fonction obtention aléatoire*)
(*fonction intermédiaire*)
let rec ieme (n:nat) (ens:'a multiensemble):'a =
match ens with
|(x,m)::l-> let nb = nbocc x ens in if n>nb then ieme (n-nb) l else x
|[]-> failwith "liste vide"
;;
assert (ieme 3 cst_test = 3);;
assert (ieme 5 cst_test4 = 4);;


let un_dans (ens:'a multiensemble) : 'a =
let k = cardinal ens in let nb = Random.int(k) in ieme nb ens 
;;
(*on ne peut pas faire de jeux d'essais pour cette fonction car elle est aléatoire*)


(*Q3*)
(*reusinage avec fold_left*)

open List ;;


(* cardinal *)
let cardinal1 (ens:'a multiensemble) : nat =
 match ens with
 | [] -> 0
 | (e,m)::l -> (fold_left (fun x (c,d) -> x+d) m l )
;;
assert (cardinal1 cst_test = 8);;
assert (cardinal1 cst_test2 = 3);;


(* appartient *)
let appartient1 (elem:'a )(ens:'a multiensemble) : bool = 
  mem_assoc elem ens
;;
assert (appartient1 4 cst_test = true);;
assert (appartient1 9 cst_test = false);;


(* nbocc *)
let nbocc1 (elem:'a )(ens:'a multiensemble) : nat =
  if (appartient1 elem ens) then (assoc elem ens) else 0
;;
assert (nbocc1 4 cst_test = 1);;
assert (nbocc1 6 cst_test = 3);;


(* inclus *)
let inclus1 (ens1:'a multiensemble)(ens2: 'a multiensemble) : bool =
  for_all (fun (el,m) -> appartient1 el ens2) ens1
;;
assert (inclus1 cst_test2 cst_test = true);;
assert (inclus1 cst_test3 cst_test = true);;


(* ajoute *)
let ajout1 (elt:'a multielement) (ens:'a multiensemble): 'a multiensemble = 
let (x,m) = elt in
if (appartient1 x ens) then fold_left (fun acc (c,d) -> if x=c then (c,d+m)::acc else (c,d)::acc) [] ens 
else (x,m)::(rev ens)
|> rev
;;
assert (ajout1 (2,2) cst_test = [(1,2); (3,2); (4,1); (6,3); (2,2)]);;
assert (ajout1 (4,2) cst_test =[(6, 3); (4, 3); (3, 2); (1, 2)]);;


(* supprime *)
let supprime1 (elt:'a multielement) (ens:'a multiensemble): 'a multiensemble =
  let (x,m) = elt in
 fold_left (fun acc (k, v) -> if x=k then (if m>v then acc else (k,v-m)::acc) else (k,v)::acc) [] ens 
 |> rev
 ;;
assert (supprime1 (6,2) cst_test = [(1,2);(3,2);(4,1);(6,1)]);;
assert (supprime1 (4,4) cst_test =[(1,2);(3,2);(6,3)]);;


(* intersection *)
let intersection1 (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble = 
  filter (fun (c,d) -> appartient1 c ens2) ens1 
;;
assert (intersection1 cst_test cst_test2 = [(3, 2); (4, 1)]);;
assert (intersection1 cst_test cst_test3 =[(3, 2); (4, 1); (6, 3)]);;


(* difference *)
let difference1 (ens1:'a multiensemble) (ens2:'a multiensemble) : 'a multiensemble =  
  fold_left (fun acc (c,d) -> 
  if (appartient1 c ens2) then (let y = (assoc c ens2) in if y<d then (c, d-y)::acc else acc) else (c,d)::acc) [] ens1
  |> rev
;;
assert (difference1 cst_test cst_test2 = [(1, 2); (6, 3)] );;
assert (difference1 cst_test cst_test3 =[(1, 2); (3, 1); (6, 1)] );;


(*PARTIE VI *)
(*Q4*)
(* implémentation des types 𝑐𝑜𝑢𝑙𝑒𝑢𝑟, 𝑣𝑎𝑙𝑒𝑢𝑟 et 𝑡𝑢𝑖𝑙𝑒*)

type couleur = Bleu |Rouge |Jaune |Noir                                         ;;
type valeur = int                              (*restreint entre 1 et 13*) ;;
type tuile = Joker | T of valeur*couleur                                   ;;

(*Q5*)
(*Implémentation des types 𝑐𝑜𝑚𝑏𝑖𝑛𝑎𝑖𝑠𝑜𝑛, 𝑡𝑎𝑏𝑙𝑒 et 𝑝𝑜𝑠𝑒*)

type combinaison =  tuile list ;;                                       ;;

type table = combinaison list  ;;                                       ;;

type pose =  table                                                      ;;

(*Q6*)
(*Implémentation des types 𝑚𝑎𝑖𝑛 et 𝑝𝑖𝑜𝑐ℎ𝑒*)
 
type main = (tuile*int) list                                             ;;
type pioche = main                                                      ;;

let cst_PIOCHE_INIT : pioche = [ (Joker, 2) ;
T(1,Rouge), 2 ; T(2,Rouge), 2 ; T(3,Rouge), 2 ; T(4,Rouge), 2 ; T(5,Rouge), 2 ;
T(6,Rouge), 2 ; T(7,Rouge), 2 ; T(8,Rouge), 2 ; T(9,Rouge), 2 ; T(10,Rouge), 2 ;
T(11,Rouge), 2 ; T(12,Rouge), 2 ; T(13,Rouge), 2 ;
T(1,Bleu), 2 ; T(2,Bleu), 2 ; T(3,Bleu), 2 ; T(4,Bleu), 2 ; T(5,Bleu), 2 ;
T(6,Bleu), 2 ; T(7,Bleu), 2 ; T(8,Bleu), 2 ; T(9,Bleu), 2 ; T(10,Bleu), 2 ;
T(11,Bleu), 2 ; T(12,Bleu), 2 ; T(13,Bleu), 2 ;
T(1,Jaune), 2 ; T(2,Jaune), 2 ; T(3,Jaune), 2 ; T(4,Jaune), 2 ; T(5,Jaune), 2 ;
T(6,Jaune), 2 ; T(7,Jaune), 2 ; T(8,Jaune), 2 ; T(9,Jaune), 2 ; T(10,Jaune), 2 ;
T(11,Jaune), 2 ; T(12,Jaune), 2 ; T(13,Jaune), 2 ;
T(1,Noir), 2 ; T(2,Noir), 2 ; T(3,Noir), 2 ; T(4,Noir), 2 ; T(5,Noir), 2 ;
T(6,Noir), 2 ; T(7,Noir), 2 ; T(8,Noir), 2 ; T(9,Noir), 2 ; T(10,Noir), 2 ;
T(11,Noir), 2 ; T(12,Noir), 2 ; T(13,Noir), 2
]
;;

let main_test : main = [ (Joker, 2) ;
T(8,Rouge), 2 ; T(4,Rouge), 1 ; T(5,Rouge), 2 ; T(6,Jaune), 2 ; T(7,Jaune), 2 ; 
T(11,Noir), 2 ; T(9,Noir), 2 ; T(10,Noir), 1 ; (Joker,1) ;T(1,Bleu), 2 ;
T(2,Bleu), 2 ; T(3,Bleu), 2 ; T(7,Bleu), 2 ; T(5,Bleu), 2 ] ;;

(*Q7*)

let cmp_tuiles (a:tuile)(b:tuile) : int =
  match a with
  |Joker -> if b = Joker then 0 else 1
  |(T(n,Noir)) ->  let T(v,_) = b in if b=Joker || (b=T(v,Noir)&&(v>n)) 
                  then 0 else 1
  |(T(n,Jaune)) ->  let T(v,_) = b in if (b=Joker)||(b=T(v,Noir))||(b=T(v,Jaune)&&(v>n)) 
                   then 0 else 1
  |(T(n,Rouge)) ->  let T(v,_) = b in if (b=Joker)||(b=T(v,Noir))||(b=T(v,Jaune))||((b=T(v,Rouge))&&(v>n)) 
                   then 0 else 1
  |(T(n,Bleu)) ->  let T(v,_) = b in if (b = T(v,Bleu)) && (n>v) 
                   then 1 else 0
;;

let rec tuil_insert (e:tuile*int)(m:main) : main =
    let (t1,occ1) = e in let f = cmp_tuiles in
    match m with
    | [] -> e::[]
    | (t2,occ2)::qe -> if (f t1 t2 = 0) then e::(t2,occ2)::qe else
                     (t2,occ2)::(tuil_insert e qe) ;;

let en_ordre (ens:'a) : 'a = 
  let jokers = (filter (fun (c,d) -> c=(Joker)) ens) in
  let tuiles = (difference ens jokers) in
  (fold_left (fun accu e -> tuil_insert e accu) [] tuiles)@jokers ;;


assert (en_ordre main_test = [(T (1, Bleu), 2); (T (2, Bleu), 2); (T (3, Bleu), 2); (T (5, Bleu), 2);
 (T (7, Bleu), 2); (T (4, Rouge), 1); (T (5, Rouge), 2); (T (8, Rouge), 2);
 (T (6, Jaune), 2); (T (7, Jaune), 2); (T (9, Noir), 2); (T (10, Noir), 1);
 (T (11, Noir), 2); (Joker, 2); (Joker, 1)] )
 ;;

assert (en_ordre cst_PIOCHE_INIT =[(T (1, Bleu), 2); (T (2, Bleu), 2); (T (3, Bleu), 2); (T (4, Bleu), 2);
 (T (5, Bleu), 2); (T (6, Bleu), 2); (T (7, Bleu), 2); (T (8, Bleu), 2);
 (T (9, Bleu), 2); (T (10, Bleu), 2); (T (11, Bleu), 2); (T (12, Bleu), 2);
 (T (13, Bleu), 2); (T (1, Rouge), 2); (T (2, Rouge), 2); (T (3, Rouge), 2);
 (T (4, Rouge), 2); (T (5, Rouge), 2); (T (6, Rouge), 2); (T (7, Rouge), 2);
 (T (8, Rouge), 2); (T (9, Rouge), 2); (T (10, Rouge), 2);
 (T (11, Rouge), 2); (T (12, Rouge), 2); (T (13, Rouge), 2);
 (T (1, Jaune), 2); (T (2, Jaune), 2); (T (3, Jaune), 2); (T (4, Jaune), 2);
 (T (5, Jaune), 2); (T (6, Jaune), 2); (T (7, Jaune), 2); (T (8, Jaune), 2);
 (T (9, Jaune), 2); (T (10, Jaune), 2); (T (11, Jaune), 2);
 (T (12, Jaune), 2); (T (13, Jaune), 2); (T (1, Noir), 2); (T (2, Noir), 2);
 (T (3, Noir), 2); (T (4, Noir), 2); (T (5, Noir), 2); (T (6, Noir), 2);
 (T (7, Noir), 2); (T (8, Noir), 2); (T (9, Noir), 2); (T (10, Noir), 2);
 (T (11, Noir), 2); (T (12, Noir), 2); (T (13, Noir), 2); (Joker, 2)] )
;;


(*implémentation du type statut et état*)

type joueur = J1 | J2                                    ;;
type statut = joueur * bool * main                       ;;
type etat = (statut * statut) * table * pioche * joueur  ;;

(*Q8*)

(*specification fonction intermédiaire:
profil: extraire_prelim : nat-> pioche ->main
sémantique: (extraire_prelim n p) dorme ne main de n tuiles tirées au hasard dans p
exemples : extraire_prelim 5 cst_PIOCHE_INIT =[(T (1, Bleu), 2); (T (13, Jaune), 1); (T (10, Noir), 1); (Joker, 1)]
*)

let rec extraire_prelim (n:nat) (p:pioche) : main =
    match n with 
    |0 -> []
    |n -> let x = (un_dans p) in (x,1)::(extraire_prelim (n-1) (supprime (x,1) p)) 
;;
(*on ne peut pas faire de jeux d'essai car la fonction est aléatoire*)

let extraire (n:int) (p:pioche) : main*pioche =
let g = en_ordre in
  let y = (extraire_prelim n p) in
    if (cardinal y) > (cardinal p) then ((g y),(difference y p)) else ((g y),(difference p y)) ;;
(*on ne peut pas faire de jeux d'essai car la fonction est aléatoire*)

let distrib (():unit) : main*main*pioche =
let g = en_ordre in
    let (x,w) = (extraire 14 cst_PIOCHE_INIT) in
      let (y,z) = (extraire 14 w) in (g x,g y,g z) 
;;
(*on ne peut pas faire de jeux d'essai car la fonction est aléatoire*)

let init_partie (():unit) : etat =
  let (x,y,z) = distrib() in 
    (((J1, false, x),(J2, false, y)),[],z,J1) 
;;
(*on ne peut pas faire de jeux d'essai car la fonction est aléatoire*)

(*Q9*)
let joueur_courant (e:etat) : joueur =
  match e with
  |((a,b),c,d,j) -> j ;;

let joueur_suivant (e:etat) : joueur =
  if (joueur_courant e = J1) then J2 else J1 ;;

let la_table (e:etat) : table =
    match e with 
    |((a,b),c,d,j) -> c ;;

let la_pioche (e:etat) : pioche =
    match e with 
    |((a,b),c,d,j) -> d ;;

let le_statut (j:joueur)(e:etat):statut =
  match e with
  |(((j1,s1,m1),(j2,s2,m2)),c,d,j) -> match j with
                                      | j1 -> (j1,s1,m1)
                                      | j2 -> (j2,s2,m2) ;;

let la_main (j:joueur)(e:etat):main =
  match e with
  |(((j1,s1,m1),(j2,s2,m2)),c,d,j) -> match j with
                                      | j1 -> m1
                                      | j2 -> m2 ;;
      

(*PARTIE VII*)
(*Q10*)

let test_suite1 : combinaison = [T(1,Jaune) ; T(2,Jaune) ; T(3,Jaune); T(4,Jaune)] ;;
let test_suite2 : combinaison = [T(1,Jaune) ; T(2,Jaune) ; T(3,Jaune); T(5,Jaune)] ;;

let rec est_suite (cmb:combinaison) : bool =
match cmb with
|[]->true
|_::[] -> true
|T(n,c)::suite -> match suite with
                  |T(v,b)::sui -> ((c=b)&&(v=n+1))&&(est_suite (T(v,b)::sui)) 
                  |Joker::[] -> true 
;;
assert (est_suite test_suite1 = true) ;;
assert (est_suite test_suite2 = false) ;;

(* implementation d'une fonction qui verifie que les couleurs ne sont pas repetes dans une groupe*)
let rec no_col_rep (cmb:combinaison) : bool = 
  match cmb with
  |[] -> true
  |T(n,c)::suite -> if (length (filter (fun (T(a,b):tuile) -> b=c) cmb)>1) then false else (no_col_rep suite) ;;

(* implementation d'une fonction qui verifie que les valeurs ne sont pas repetes dans une groupe*)
let rec val_eg (cmb:combinaison) : bool =
  match cmb with
  |[] -> true
  |T(n,c)::suite -> match suite with
                  |T(i,d)::sui -> (i=n)&&(val_eg (T(i,d)::sui))
                  |Joker::[] -> true ;;

 (* implementation d'une fonction qui verifie si groupe ou non*)
let est_groupe (cmb:combinaison) : bool =
((length cmb)<=4)&&(no_col_rep cmb)&&(val_eg cmb)
;;

(*implementation d'une fonction preliminaire d'ordre superieur *)
let pre_combinaison_valide (f:combinaison->bool)(cmb:combinaison) : bool =
  ((length cmb)>=3) && (f cmb)  ;;

(*implementation d'une fonction qui verifie qu'une combinaison est valide *)
let combinaison_valide (cmb:combinaison) : bool = (pre_combinaison_valide est_suite cmb)||(pre_combinaison_valide est_groupe cmb) ;;

(*implementation d'une fonction preliminaire d'ordre superieur *)
let rec pre_combinaisons_valides (f:combinaison -> bool)(cmbl:combinaison list) : bool =
    match cmbl with
    |[]-> false
    |_::[]-> true
    |tt::suite -> (f tt)&&(pre_combinaisons_valides f suite) ;;

(*implementation d'une fonction qui verifie qu'une liste des combinaison est valide *)
let combinaisons_valides = pre_combinaisons_valides combinaison_valide;;

let combi_test : combinaison list = [[T(10,Rouge);T(11,Rouge);T(12,Rouge);(Joker)];[T(1,Rouge);T(1,Noir);T(1,Jaune); T(1,Bleu)]] ;;

  (* Q11 *)

(* implementation d'une fonction qui compte les points d'une suite en utilisant la formule d'une somme arithmetique *)
let points_suite (cmb:combinaison) : int = 
    match cmb with
    |[]-> 0
    |tt::suite -> let l = (length cmb) in match tt with 
                  | T(n,c) -> (l*(2*n+l-1))/2 ;;

(* implementaion d'une fonction qui compte les points d'une groupe *)
let points_groupe (cmb:combinaison) : int =
      match cmb with
      | []-> 0
      |tt::suite -> let l = (length cmb) in match tt with 
                    |T(n,c) -> l*n ;;

(* implementaion d'une fonction qui compte les points d'une combinaison *)                 
let points_combinaison (f:combinaison -> bool)(g:combinaison -> bool)(cmb:combinaison) : int =
  if (combinaison_valide f cmb)||(combinaison_valide g cmb) then 
  (match est_suite cmb with
  | true -> points_suite cmb
  |false -> points_groupe cmb)
  else 0
            ;;

(*implementation d'une fonction preliminaire d'ordre superieur *)
let rec pre_points_pose (f:combinaison -> bool)(g:combinaison -> bool)(cmbl:combinaison list) : int =
    match cmbl with
    |[]->0
    |cmb::suite -> (points_combinaison f g cmb) + (points_pose f g suite) ;;

(*implementation d'une fonction qui compte les points d'une pose*)
let points_pose = pre_points_pose est_suite est_groupe ;;
    (* PARTIE VIII *)
  
  (* Q12 *)

(* fonction qui convertis les tuiles en type multielement et qui forment un tuile multiensemble*)
let rec conv_combi (cmb: combinaison) : tuile multiensemble =
  match cmb with
  | []->[]
  |tt::suite -> (ajout (tt,1) [])@(conv_combi suite) ;;

(* fonction qui convertis chaque tuile d'une table en tuile multielement *)
let rec conv_combi_cumul (t:table) : tuile multiensemble =
    match t with 
    |[]-> []
    |cmb::suite -> (conv_combi cmb)@(conv_combi_cumul suite) ;;

(* fonction qui regroupe tuiles de les memes valeurs en un tuile multielement*)
let group_pair (t:tuile multiensemble): tuile multiensemble =
    match t with
    |[]->[]
    |(h,occ) :: sui -> let next = filter (fun (c,d)->c=h) t in
                        match next with
                        |tt::[] -> tt::[]
                        |tt::suit -> (ajout tt suit) ;;

(* fonction qui regroupe toutes les tuiles d'un multiensemble en appliquant la fonction precedent*)
let rec group_cumul (t:tuile multiensemble) : tuile multiensemble =
    match t with 
    |[]->[]
    |(h,occ) :: sui -> (group_pair t)@ (group_cumul (difference t [(h,occ)]));;

(*fonction preliminaire d'ordre superieur*)
let fonctions_3_ord (a:'a->'a)(b:'b->'a)(c:'b) : 'a  = 
  (a (b c))
   ;;
(* implementation d'une fonction qui convertis une table en tuile multiensemble *)
let tableVmens = fonctions_3_ord group_cumul conv_combi_cumul ;;

let list_test = [(T (1, Rouge), 1); (T (1, Noir), 1); (T (1, Jaune), 1); (T (1, Bleu), 1);
 (T (1, Rouge), 1); (T (2, Rouge), 1); (T (3, Rouge), 1); (T (4, Rouge), 1)] ;;

let table_test = [[T (1, Rouge); T (1, Noir); T (1, Jaune)];[
 T (1, Rouge); T (2, Rouge); T (3, Rouge); T (4, Rouge)]] ;; 
 let tuile_test = T(1,Rouge) ;;



(* Q13 *)

let premier_coup_ok (m1:main)(p:pose)(m2:main) : bool =
  let t = tableVmens p in
  ((points_pose p) >= 30)&&(combinaisons_valides p)&&(inclus t m1)&&((difference m1 t)=m2) ;;

let coup_ok (t1:table)(m1:main)(t2:table)(m2:main) : bool =
  let x = tableVmens t1 in
  let y = tableVmens t2 in 
  let z = difference m1 m1 in 
  (z = difference x y)&&(combinaisons_valides t1)&&(combinaisons_valides t2) ;;

(* Q14 *)

let rec ajouter_tuile (ta:table)(tu:tuile) : table = 
  match ta with
  |[]->[]
  |cmb::suite -> match cmb with
                  |t::qe -> if (combinaison_valide (tu::cmb)) then (tu::cmb)::suite 
                          else (if(combinaison_valide (cmb@[tu])) then (cmb@[tu])::suite 
                          else if (ajouter_tuile suite tu)=[] then [] else cmb::(ajouter_tuile suite tu)) 
    ;;


  (* Q15 *)
  (*
let rec plusieurs_choix (m:main)(n:int) : main =
  match n with
  | 0 -> []
  | n -> let y = (un_dans m) in (y,1)::(plusieurs_choix (supprime (y,1) m) (n-1));;

let cmpt = 0;;

let rec extraction (f:'a->bool)(m:main) : main = 
    if cmpt < 50 then
    (match f with
    |est_suite -> let n = 3 + (Random.int ((length m)-2)) in let y = en_ordre(plusieurs_choix m n) in 
                                                          if (pre_combinaison_valide est_suite y) then y else cmpt+1 , (extraction f m)
    |est_groupe -> let n = 3 + (Random.int 2) in let y = en_ordre(plusieurs_choix m n) in
                                              if (pre_combinaison_valide est_groupe y) then y 
                                              else cmpt+1 , (extraction f m)) ;;

let extraction_suite = extraction est_suite ;;
let extraction_groupe = extraction est_groupe ;; 
*)
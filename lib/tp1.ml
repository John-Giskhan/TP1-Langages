(******************************************************************************)
(*  TP1 Hiver 2024 - Langages de programmation (IFT-3000)                     *)
(*  Gestion de cours et de programmes - Dépendances de cours                  *)
(******************************************************************************)
(******************************************************************************)
(* NOM: _________________             PRÉNOM: _____________________           *)
(* MATRICULE: ___________             PROGRAMME: __________________           *)
(******************************************************************************)

(******************************************************************************)
(* Implantation                                                               *)
(******************************************************************************)

open GcpLib
open Gcp
open List

(*  ------------------------------------------------------------------------- *)
(*  Structures de données                                                     *)
(*  ------------------------------------------------------------------------- *)

type type_cours = OB | OP | Conc

(* -------------------------------------------------------------------------- *)
(* Partie réservée aux fonctions utiles ------------------------------------- *)
(* Vous pouvez ajouter les fonctions et définitions que vous voulez           *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* Début partie code (implantation) à compléter ----------------------------- *)
(* -------------------------------------------------------------------------- *)

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ---------------------------------------- *)
let est_prerequis (lc : cours list) (nc1 : num_cours) (nc2 : num_cours) : int =
  let prerequis_nc1 = lc_dans_pr (ret_descr lc nc1).pre in
  if List.exists (fun pre -> pre = nc2) prerequis_nc1 then 1
  else
    let prerequis_nc2 = lc_dans_pr (ret_descr lc nc2).pre in
    if List.exists (fun pre -> pre = nc1) prerequis_nc2 then -1 else 0

(* -- À IMPLANTER/COMPLÉTER (30 PTS) ---------------------------------------- *)
let is_cours_in_list cours lst = List.mem cours.titre lst

let simp_pre (pre : prealables) : prealables =
  let prealables = lc_dans_pr pre in
  let str = pr2str pre in
  (* print_string "before lc_dans_pr";
     print_newline ();
     print_string str;
     print_newline ();
     print_string "afteer lc_dans_pr";
     print_newline ();

     List.iter (fun x -> Printf.printf "%s " x) prealables; *)
  print_newline ();
  Aucun
(* let rec filter_pre pre lst = match pre with
   | CP cours when (is_cours_in_list cours.titre lst) -> Aucun
   | CCP cours when (is_cours_in_list cours.titre lst) -> Aucun
   | OU pres -> OU (List.filter_map (fun p -> match filter_pre p lst with Aucun -> None | x -> Some x) pres)
   | ET pres -> ET (List.filter_map (fun p -> match filter_pre p lst with Aucun -> None | x -> Some x) pres)
   | _ -> Aucun in
   filter_pre pre prealables *)

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ---------------------------------------- *)
let seuls_cours_pgm_dans_pre (lncp : num_cours list) (pre : prealables) :
    prealables =
  raise (Non_Implante "seuls_cours_pgm_dans_pre non implanté")

(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let rec cours_dans_liste_exigences (exigences : exigences list) (doublons : bool) : num_cours list =
  match exigences with
  | [] -> []
  | h::t -> if doublons then extrait_lc h @ cours_dans_liste_exigences t doublons
                        else extrait_lc h ++ cours_dans_liste_exigences t doublons

let cours_pgm_par_type (pgm : programme) (tc : type_cours) : num_cours list =
  let _, titre, _, cours_OB, cours_OP, cours_Conc = pgm in

  match tc with
  | OB ->
      let _, liste_titre_exigences = cours_OB in
      let liste_exigences =
        List.map (fun (_, exigences) -> exigences) liste_titre_exigences
      in
      cours_dans_liste_exigences liste_exigences false
  | OP ->
      let _, liste_titre_exigences = cours_OP in
      let liste_exigences =
        List.map (fun (_, exigences) -> exigences) liste_titre_exigences
      in
      cours_dans_liste_exigences liste_exigences false
  | Conc ->
      let (liste_liste_exigences : exigences list list) =
        List.map (fun (_, (_, exigences_list)) -> exigences_list) cours_Conc
      in
      let (liste_exigences : exigences list) =
        List.fold_left ( ++ ) [] liste_liste_exigences
      in
      cours_dans_liste_exigences liste_exigences true


(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let cours_pgm (pgm : programme) : num_cours list =
  let _, titre, _, cours_OB, cours_OP, cours_Conc = pgm in

  let _, liste_titre_exigences = cours_OB in
  let liste_exigences_OB =
    List.map (fun (_, exigences) -> exigences) liste_titre_exigences
  in

  let _, liste_titre_exigences = cours_OP in
  let liste_exigences_OP =
    List.map (fun (_, exigences) -> exigences) liste_titre_exigences
  in

  let (liste_liste_exigences : exigences list list) =
    List.map (fun (_, (_, exigences_list)) -> exigences_list) cours_Conc
  in

  let (liste_exigences_Conc : exigences list) =
    List.fold_left ( ++ ) [] liste_liste_exigences
  in
  cours_dans_liste_exigences (liste_exigences_OB ++ liste_exigences_OP ++ liste_exigences_Conc) false



(* -- À IMPLANTER/COMPLÉTER (15 PTS) ---------------------------------------- *)
let cours_contrib_dans_pgm (nc : num_cours) (lpgms : (string * programme) list)
    : (string * type_cours option) list =
    List.map (fun (dept, programme) -> 
      let cours_OB = cours_pgm_par_type programme OB in
      let cours_OP = cours_pgm_par_type programme OP in
      let cours_Conc = cours_pgm_par_type programme Conc in

      let type_cours_option =
        if List.exists (fun cours -> cours = nc) cours_OB then Some OB
        else if List.exists (fun cours -> cours = nc) cours_OP then Some OP
        else if List.exists (fun cours -> cours = nc) cours_Conc then Some Conc
        else None
      in
      (dept, type_cours_option)
    ) lpgms

      

(* -- À IMPLANTER/COMPLÉTER (25 PTS) ---------------------------------------- *)
let regroupe_cours_equiv (lc : cours list) (lnc : num_cours list) :
    num_cours list list =
  raise (Non_Implante "regroupe_cours_equiv non implanté")

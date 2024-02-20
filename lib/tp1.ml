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
let is_cours_in_list cours lst =
  List.mem cours.titre lst

let simp_pre (pre : prealables) : prealables =
  let prealables = lc_dans_pr pre in
  let str = pr2str pre in
  print_string("before lc_dans_pr");
  print_newline();
  print_string str;
  print_newline();
  print_string("afteer lc_dans_pr");
  print_newline();

  List.iter (fun x -> Printf.printf "%s " x) prealables;
  print_newline();

  let rec filter_pre pre lst = match pre with
  | CP cours when (is_cours_in_list cours.titre lst) -> Aucun
  | CCP cours when (is_cours_in_list cours.titre lst) -> Aucun
  | OU pres -> OU (List.filter_map (fun p -> match filter_pre p lst with Aucun -> None | x -> Some x) pres)
  | ET pres -> ET (List.filter_map (fun p -> match filter_pre p lst with Aucun -> None | x -> Some x) pres)
  | _ -> Aucun in 
  filter_pre pre prealables 

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ---------------------------------------- *)
let seuls_cours_pgm_dans_pre (lncp : num_cours list) (pre : prealables) :
    prealables =
  raise (Non_Implante "seuls_cours_pgm_dans_pre non implanté")

(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let cours_pgm_par_type (pgm : programme) (tc : type_cours) : num_cours list =
  raise (Non_Implante "cours_pgm_par_type non implanté")

(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let cours_pgm (pgm : programme) : num_cours list =
  raise (Non_Implante "cours_pgm non implanté")

(* -- À IMPLANTER/COMPLÉTER (15 PTS) ---------------------------------------- *)
let cours_contrib_dans_pgm (nc : num_cours) (lpgms : (string * programme) list)
    : (string * type_cours option) list =
  raise (Non_Implante "cours_contrib_dans_pgm non implanté")

(* -- À IMPLANTER/COMPLÉTER (25 PTS) ---------------------------------------- *)
let regroupe_cours_equiv (lc : cours list) (lnc : num_cours list) :
    num_cours list list =
  raise (Non_Implante "regroupe_cours_equiv non implanté")

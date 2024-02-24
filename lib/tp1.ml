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
let cours_est_dans_liste cours liste = List.mem cours.titre liste

(* Applatir OU et ET imbriques *)
let applatir_OU_ET liste : prealables list =
  List.fold_left
    (fun acc pre ->
      match pre with ET liste | OU liste -> acc @ liste | _ -> acc @ [ pre ])
    [] liste

let enlever_doublons liste : prealables list = [] ++ liste
let enlever_aucun liste : prealables list = List.filter (( <> ) Aucun) liste

let rec simp_pre pre : prealables =
  match pre with
  | OU liste | ET liste -> (
      let liste_simplifiee =
        liste |> List.map simp_pre |> applatir_OU_ET |> enlever_doublons
        |> enlever_aucun
      in
      match liste_simplifiee with
      | [] -> Aucun
      | [ x ] -> x
      | _ -> ( match pre with OU _ -> OU liste_simplifiee | _ -> ET liste_simplifiee))
  | _ -> pre

(* -- À IMPLANTER/COMPLÉTER (10 PTS) ---------------------------------------- *)
let rec seuls_cours_pgm_dans_pre (lncp : num_cours list) (pre : prealables) :
    prealables =
  let pre_simplifie = simp_pre pre in
  match pre_simplifie with
  | CP cours -> if List.mem cours lncp then pre_simplifie else Aucun
  | CCP cours -> if List.mem cours lncp then pre_simplifie else Aucun
  | CRE _ -> pre_simplifie
  | OU liste | ET liste -> (
      let process_liste =
        List.fold_right
          (fun elem acc ->
            let filtrer = seuls_cours_pgm_dans_pre lncp elem in
            match filtrer with Aucun -> acc | _ -> filtrer :: acc)
          liste []
      in
      match process_liste with
      | [] -> Aucun
      | [ x ] -> x
      | _ as filtrer -> (
          match pre_simplifie with
          | OU _ -> OU filtrer
          | ET _ -> ET filtrer
          | _ -> assert false))
  | Aucun -> Aucun

(* -- À IMPLANTER/COMPLÉTER (5 PTS) ----------------------------------------- *)
let rec cours_dans_liste_exigences (exigences : exigences list)
    (doublons : bool) : num_cours list =
  match exigences with
  | [] -> []
  | h :: t ->
      if doublons then extrait_lc h @ cours_dans_liste_exigences t doublons
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
  cours_dans_liste_exigences
    (liste_exigences_OB ++ liste_exigences_OP ++ liste_exigences_Conc)
    false

(* -- À IMPLANTER/COMPLÉTER (15 PTS) ---------------------------------------- *)
let cours_contrib_dans_pgm (nc : num_cours) (lpgms : (string * programme) list)
    : (string * type_cours option) list =
  List.map
    (fun (dept, programme) ->
      let cours_OB = cours_pgm_par_type programme OB in
      let cours_OP = cours_pgm_par_type programme OP in
      let cours_Conc = cours_pgm_par_type programme Conc in

      let type_cours_option =
        if List.exists (fun cours -> cours = nc) cours_OB then Some OB
        else if List.exists (fun cours -> cours = nc) cours_OP then Some OP
        else if List.exists (fun cours -> cours = nc) cours_Conc then Some Conc
        else None
      in
      (dept, type_cours_option))
    lpgms

(* -- À IMPLANTER/COMPLÉTER (25 PTS) ---------------------------------------- *)
let num_cours_dans_liste_cours (lc : cours list) (lnc : num_cours list) : bool =
  let num_cours_lc = List.map (fun (num_cours_lc, _) -> num_cours_lc) lc in
  List.for_all
    (fun elem -> respecte_motif elem "*-*" && List.mem elem num_cours_lc)
    lnc

let regroupe_cours_equiv (lc : cours list) (lnc : num_cours list) :
    num_cours list list =
  if num_cours_dans_liste_cours lc lnc then
    let equiv = nc_eq lc in
    let cours_equiv =
      List.fold_left
        (fun acc num_cours ->
          let groupes_equiv, groupes_non_equiv =
            List.partition
              (fun groupe -> List.exists (equiv num_cours) groupe)
              acc
          in
          match groupes_equiv with
          | [] -> [ num_cours ] :: acc
          | [ x ] -> (num_cours :: x) :: groupes_non_equiv
          | _ -> failwith "Erreur, plusieurs groupes sont equivalents.")
        [] lnc
    in
    List.map (List.sort Stdlib.compare) cours_equiv
  else failwith "un des cours présent dans [lnc] n'est pas défini dans [lc]"

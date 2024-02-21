(******************************************************************************)
(** TPs (depuis H-21) - Langages de programmation (IFT-3000)        
    - Gestion de cours et de programmes (IFT-3000).                           *)
(******************************************************************************)

(******************************************************************************)
(* Spécification                                                              *)
(******************************************************************************)
exception Non_Implante of string
(** Exception utilisée par le correcteur automatique. *)

(** {1 Structures de données}*)

type cours = num_cours * desc_cours
(** Un cours associe une description à un numéro de cours. *)

and desc_cours = {
  titre : titre;  (** Titre du cours. *)
  credit : credit;  (** Nombre de crédits. *)
  conco : num_cours;  (** Cours concomittant. *)
  pre : prealables;  (** Conditions préalables. *)
  dept : dept;  (** Département détenteur de cours. *)
  multi : num_cours;  (** Cours multicycle correspondant. *)
  equiv : num_cours;  (** Cours qu'on considère équivalent. *)
  (* ********** num_cours list?  *)
  offre : (session * fe) list;  (** Session et formule d'enseignement.*)
  dom : domaine list;  (** Domaine d'application du cours. *)
}
(** Type permettent de définir les différents attributs d'un cours. *)

and num_cours = string
(** Un numéro de cours comprend normalement un sigle composé de 3 
    caractères et un nombre composé de 4 chiffres, tous les deux séparés
    par un tiret («-»). *)

and titre = string
(** Type permettant de définir le titre d'un cours. *)

(** Un tel type représente soit un nombre de crédits pour un cours, soit un 
    nombre de crédits associé à un stage. *)
and credit =
  | Cr of int  (** Nombre de crédits associé à un cours. *)
  | St of int  (** Nombre de crédits associé à un stage. *)

(** Type permettant de représenter les préalables d'un cours. *)
and prealables =
  | Aucun  (** Pas de préalables. *)
  | CP of num_cours  (** Cours préalable. *)
  | CCP of num_cours  (** Cours concomittant ou préalable. *)
  | CRE of int  (** Nombre de crédits minimum. *)
  | OU of prealables list  (** Disjonction de préalables. *)
  | ET of prealables list  (** Conjonction de préalables. *)

(* ********** AJOUT CHAMP RESTRICTION?
   https://www.ulaval.ca/etudes/cours/bif-1000-profession-en-bio-informatique
   Préalable:  Doit être inscrit à Baccalauréat en bio-informatique
   -> donc, ajout champ restriction: Pgm of titre (list?) | ...
*)
and dept = string
(** Type représentant le département détenteur d'un cours. *)

(** Type permettant de représenter les sessions d'études.
    https://www.ulaval.ca/etudes/formules-denseignement *)
and session =
  | Aut  (** Session d'automne. *)
  | Hiv  (** Session d'hiver. *)
  | Ete  (** Session d'été. *)
  | Annee  (** 3 sessions de l'année. *)
(* Règlement des études - Hiver 2024 - 4 formules d'enseignement:
   - Présence (P)
   - Distance (D) (synchrone ou asynchrone)
   - Hybride (H) (présence + distance synchrone ou asynchrone)
   - Comodal (C)
*)

(** Type permettant de représenter des formules d'enseignement. *)
and fe =
  | P  (** Cours en présentiel. *)
  | H  (** Cours hybride (présentiel-distance). *)
  | D  (** Cours à distance (synchrone ou asynchrone). *)
  | C  (** Cours comodal.*)
  | A  (** Ancien code: Cours en classe; équivalent à P *)
  | Z1  (** Ancien code: Cours à distance asynchrone; équivalent à D *)
  | Z3  (** Ancien code: Cours à distance synchrone; équivalent à D *)
  | ZA  (** Ancien code: Cours comodal; équivalent à P *)
  | FEX  (** Ancien code: Toute forme d'enseignement.*)

(** Type permettant de représenter des domaines de savoir. *)
and domaine =
  | GEN  (** Général. *)
  | PROG  (** Programmation. *)
  | SYS  (** Système. *)
  | MAT  (** Mathématiques. *)
  | STAT  (** Statistiques. *)
  | LOG  (** Logique. *)
  | THEO  (** Théorique. *)
  | ANA_CON  (** Analyse et conception. *)
  | NET  (** Réseaux. *)
  | BD  (** Traitement de base de données. *)
  | IA  (** Intelligence artificielle. *)
  | SEC  (** Sécurité. *)
  | GAME  (** Jeu vidéo. *)
  | WEB  (** Technologie Web. *)
  | BIO  (** BioInfo. *)
  | ENT  (** Entreprenariat. *)
  | SIO  (** Système d'information organisationnel. *)
  | LANG  (** Langues. *)
  | CHM  (** Chimie. *)
  | DD  (** Développement durable. *)
  | ECO  (** Économie. *)

type programme =
  desc_pgm
  * titre
  * session list
  * (int * (string * exigences) list)
  * (int * (string * exigences) list)
  * concentration list
(** Un programme d'études est défini comme un tuple de valeurs :
  (décrit type, domaine savoir, #crédits, #années *
  discipline du programme *
  sessions d'admission *
  exigences au niveau des activités de formation 
    obligatoire, définies sous forme de liste de 
    règles 
    *
  exigences au niveau des activités de formation 
    à option, définies sous forme de liste de 
    règles 
    * liste de concentrations) *)

and desc_pgm = type_pro * string * int * int
(** la description d'un programme est la donnée d'un quadruplet *)

(** Le type de programme *)
and type_pro =
  | Bacc  (** Baccalauréat *)
  | Cert  (** Certificat *)
  | MP  (** Microprogramme *)
  | M  (** Maitrise avec stages *)
  | MM  (** Maitrise avec mémoire *)
  | Doct  (** Doctorat *)

(** Exigences du programme définies sous forme de type algébrique *)
and exigences =
  | CoursOB of int * num_cours list
      (** Liste de cours obligatoires totalisant un nombre de crédits *)
  | PlageCr of int * int * exigences_ext
      (** Choix au niveau exigences respectant un interval de crédits *)

and exigences_ext =
  | Cours of num_cours list  (** Liste de cours précisés au choix *)
  | CoursExclus of num_cours list
      (** Liste de cours ou motifs de cours exclus *)

and concentration = string * (int * exigences list)
(** Concentrations définies dans un programme :
    (Nom de la concentration *
    Nombre de crédits à atteindre * les exigences) *)

type choix =
  | Min
  | Max
  | Plein
  | Interv of int * int
      (** Type permettant de préciser le nombre de crédits à viser pour un 
    choix de cours *)

(*  ------------------------------------------------------------------------- *)
(** {1 Tps, H-21}                                                             *)
(*  ------------------------------------------------------------------------- *)

(** {2 Fonctions utilitaires (exportées)}                                     *)

val ( ++ ) : 'a list -> 'a list -> 'a list
(** [l1 ++ l2] ajoute aux éléments de [l1] les éléments de [l2] qui n'y 
    figurent pas déjà.
*)

val ( -- ) : 'a list -> 'a list -> 'a list
(** [l1 -- l2] retire des éléments de [l1] les éléments de [l2]. *)

val explode : string -> char list
(** [explode s] retire une liste des caractères de [s]. *)

val eql : 'a list -> 'a list -> bool
(** [eql l1 l2] retourne vrai si les 2 listes [l1] et [l2] comprennent les
    mêmes éléments, indépendemment de l'ordre de leur apparition. 
*)

val est_alpha : char -> bool
(** [est_alpha c] teste si [c] est un caractère alphabétique. *)

val est_num : char -> bool
(** [est_num c] teste si [c] est un caractère numérique. *)

val id : num_cours -> string
(** [id nc] retourne l'identifiant du cours [nc], c'est-à-dire le sigle sans 
    le tiret. *)

val idl : num_cours list -> string
(** [idl lnc] retourne l'identifiant du premier cours dans la liste [lnc]. *)

(** {2 Interface des fonctions exportées}                                     *)

val ret_descr : cours list -> num_cours -> desc_cours
(** [ret_descr bcours c] retourne, à partir du bassin de cours [bcours], 
    la description d'un cours [c]. 
    
    {b Soulève exception} {e Failure} si le cours [c] n'est pas défini dans 
    [bcours]. Par contre, la fonction ne soulève d'exceptions si le cours [c] 
    ou les cours mentionnés dans [bcours] ne respectent le format ###-$$$$.
*)

val nc_eq : cours list -> num_cours -> num_cours -> bool
(** [nc_eq bcours nc1 nc2] teste si 2 cours [nc1] et [nc2] sont égaux ou 
    équivalents.
    
    {b Soulève exception} {e Failure} si un des 2 cours [nc1] ou [nc2] n'est pas 
    défini dans [bcours] (l'exception pourrait avoir été soulevée par une autre 
    fonction appelée dans le corps de [nc_eq]).
*)

val ret_sig_num_motif : string -> string * string
(** [ret_sig_num m] retourne une paire de chaines de caractères, comprenant 
    respectivement le sigle et le numéro de [m]. 
    
    {b Soulève exception} {e Failure} si [m] ne respecte pas un des formats 
    proposés dans la documentation de la fonction [respecte_motif].
*)

val ret_sig_num : num_cours -> string * string
(** [ret_sig_num c] retourne une paire de chaines de caractères, comprenant 
    respectivement le sigle et le numéro de [c]. 
    
    {b Soulève exception} {e Failure} si [c] ne respecte pas le motif suivant 
    ###-$$$$ (# représentant un caractère alphabétique; $ représentant un 
    chiffre).
*)

val respecte_motif : num_cours -> string -> bool
(** [respecte_motif c m] vérifie si le cours [c] respecte le motif [m]. 
    
    {b Soulève exception} {e Failure} si le motif [m] ne respecte pas un des 
    formats suivants: 
    - "*-*" pour désigner tous les cours possibles;
    - "*-I" pour désigner tous les cours de 1er cycle (dont le numéro débute
       par 1, 2, 3 ou 4); 
    - "*-#*" pour désigner tous les cours ayant un numéro qui débute par #;     
    - "*-####" pour désigner tous les cours ayant le numéro ####.
    - "###-*" pour désigner tous les cours ayant un sigle ###;
    - "###-I" pour désigner tous les cours de 1er cycle ayant un sigle ###; 
    - "###-$*" pour désigner tous les cours ayant un sigle ### et un 
      numéro qui débute par $;
    - "###-$$$$" pour désigner le cours ayant un sigle ### et un numéro
      $$$$.
    
    {b Soulève exception} {e Failure} si le cours [c] ne respecte pas le motif 
    ###-$$$$ (l'exception pourrait avoir été soulevée par une autre fonction 
    appelée dans le corps de [respecte_motif]). 
*)

val pr2str : prealables -> string
(** [pr2str pr] retourne, sous forme de chaine de caractères, le préalable
    [pr] passé en argument.
*)

val existe_pr : prealables -> prealables -> bool
(** [existe_pr pr pr'] vérifie si le préalable [pr'] comprend le préalable
    [pr] (dans le sens qu'on retrouve [pr] dans [pr']). En particulier:
    - «Aucun» est considéré comme seulement inclus dans «Aucun»;
    - «CRE n» ne peut être inclus que dans un préalable identique;
    - «CP [c]» peut être inclus dans un «CP» similaire, ou un «CCP» associé
      au même cours [c];
    - «CCP [c]» ne peut être inclus que dans un préalable identique;
    - un prérequis [pr] de type «CP», «CCP», ou «CRE» est inclus dans un 
      prérequis de type «OU [lpr]», ou «ET [lpr]», si on retrouve un prérequis
      [pr]{_[2]} dans la liste [lpr] tel que [pr] est inclus dans 
      [pr]{_[2]};
    - un prérequis [pr] de type «OU» est inclus dans un prérequis de 
      type «ET [lpr]» si on retrouve un prérequis [pr]{_[2]} dans la liste 
      [lpr] tel que [pr] est inclus dans [pr]{_[2]};
    - un prérequis [pr] de type «ET» est inclus dans un prérequis de
      type «OU [lpr]» si on retrouve un prérequis [pr]{_[2]} dans la liste 
      [lpr] tel que [pr] est inclus dans [pr]{_[2]};
    - un prérequis [pr] de type «OU [lpr]{_[1]}» est inclus dans un 
      prérequis de type «OU [lpr]{_[2]}» si soit pour tous les prérequis 
      [pr]{_[1]} présents dans [lpr]{_[1]}, on retrouve un prérequis 
      [pr]{_[2]} dans la liste [lpr]{_[2]} tel que [pr]{_[1]} est inclus dans 
      [pr]{_[2]}, soit on retrouve un prérequis [pr]{_[2]} dans la liste 
      [lpr]{_[2]} tel que [pr] est inclus dans [pr]{_[2]};
    - un prérequis [pr] de type «ET [lpr]{_[1]}» est inclus dans un 
      prérequis de type «ET [lpr]{_[2]}» si soit pour tous les prérequis 
      [pr]{_[1]} présents dans [lpr]{_[1]}, on retrouve un prérequis 
      [pr]{_[2]} dans la liste [lpr]{_[2]} tel que [pr]{_[1]} est inclus dans 
      [pr]{_[2]}, soit on retrouve un prérequis [pr]{_[2]} dans la liste 
      [lpr]{_[2]} tel que [pr] est inclus dans [pr]{_[2]}.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    éventuellement dans [pr] ou [pr'] respectent le format ###-$$$$ (autrement 
    dit, la fonction [existe_pr] n'est pas censée soulever des exceptions).
*)

val lc_dans_pr : prealables -> num_cours list
(** [lc_dans_pr pr] retourne la liste des numéros de cours mentionnés dans 
    le préalable [pr]. Le résultat ne comprend qu'une seule occurence de 
    chaque cours; et l'ordre des numéros de cours n'importe pas.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    éventuellement dans [pr] respectent le format ###-$$$$ (autrement 
    dit, la fonction [lc_dans_pr] n'est pas censée soulever des exceptions).
*)

val respecte_pr : num_cours list -> int -> num_cours list -> prealables -> bool
(** [respecte_pr lco tcr lc pr] teste si, considérant une liste de cours [lco] 
    déjà suivis et réussis, et du total [tcr] des crédits obtenus, et d'une
    liste de cours [lc] envisagés, un préalable [pr] est respecté.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    dans [lco], [lc] et éventuellement dans [pr] respectent le format ###-$$$$ 
    (autrement dit, la fonction [respecte_pr] n'est pas censée soulever des 
    exceptions).
*)

val total_cr : ?avec_stage:bool -> cours list -> num_cours list -> int
(** [total_cr bcours lc] retourne, à partir du bassin de cours [bcours], 
    le total des crédits des cours mentionnés dans [lc]. À noter que si un
    cours de [lc] correspond à un stage (crédits définis à l'aide du 
    constructeur «[St]»), on comptabilise 0 crédit pour ce cours, à moins que
    le paramètre optionnel [Avec_stage] vaut true. 
    
    {b Soulève exception} {e Failure} si un des cours présents dans [lc] n'est 
    pas défini dans [bcours] (l'exception pourrait avoir été soulevée par une 
    autre fonction appelée dans le corps de [total_cr]). Par contre, il n'est 
    pas nécessaire de valider que tous les cours mentionnés dans [lc] ou 
    [bcours] respectent le format ###-$$$$.
*)

val lc_absents : cours list -> num_cours list
(** [lc_absents bcours] retourne la liste des numéros de cours mentionnés 
    dans la partie «description» du bassin de cours [bcours] mais non définis 
    dans celui-ci. Le résultat ne comprend qu'une seule occurence de chaque 
    cours; et l'ordre des numéros de cours n'importe pas.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    dans [bcours] respectent le format ###-$$$$ (autrement dit, la fonction 
    [lc_absents] n'est pas censée soulever des exceptions).
*)

val ret_bcours_lmot : string list -> cours list -> cours list
(** [ret_bcours_lmot lmot bcours] retourne, à partir du bassin de cours 
    [bcours], les cours dont le numéro respecte un des motifs précisés 
    dans [lmot]. L'ordre des éléments de la liste résultante n'importe pas.
    
    {b Soulève exception} {e Failure} si un des motifs de [lmot] ne respecte
    pas un des formats précisés pour la fonction {!respecte_motif}. De même, 
    si un cours mentionné dans [bcours] ne respecte pas le format ###-$$$$,
    une exception sera soulevée. À notez que dans les 2 cas de figure, 
    l'exception sera sûrement soulevée par une fonction appelée dans le
    corps de la fonction [ret_bcours_lmot].
*)

val ret_bcours_lses_lfe : session list -> fe list -> cours list -> cours list
(** [ret_bcours_lses_lfe lses lfe bcours] retourne, à partir du bassin de cours
    [bcours], les cours qui sont offerts dans une des sessions présentes dans
    [lses], et qui sont donnés par une des formules d'enseignement présentes 
    dans [lfe]. Si l'une des listes [lses] ou [lfe] est vide, on considéra
    qu'elles comprennent respectivement les valeurs «Annee» et «FEX».
    L'ordre des éléments de la liste résultante n'importe pas.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    dans [bcours] respectent le format ###-$$$$ (autrement dit, la fonction 
    [ret_bcours_lses_lfe] n'est pas censée soulever des exceptions).
*)

val ret_bcours_ldom : domaine list -> cours list -> cours list
(** [ret_bcours_ldom ld bcours] retourne, à partir du bassin de cours 
    [bcours], les cours qui appartiennent à l'un des domaines précisé dans 
    [ld]. L'ordre des éléments de la liste résultante n'importe pas.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    dans [bcours] respectent le format ###-$$$$ (autrement dit, la fonction 
    [ret_bcours_dom] n'est pas censée soulever des exceptions).
*)

val ret_bcours_pr : prealables -> cours list -> cours list
(** [ret_bcours_pr pr bcours] retourne, à partir du bassin de cours 
    [bcours], les cours dont le préalable inclut [pr]. L'ordre des éléments 
    de la liste résultante n'importe pas.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    dans [bcours] et éventuellement dans [pr] respectent le format ###-$$$$ 
    (autrement dit, la fonction [ret_bcours_pr] n'est pas censée soulever 
    des exceptions).
*)

val ret_bcours_admissibles : num_cours list -> int -> cours list -> cours list
(** [ret_bcours_admissibles lco tcr bcours] retourne, en considérant une liste 
    de cours [lco] déjà suivis et réussis, et du total [tcr] des crédits 
    obtenus, la liste des cours présents dans le bassin de cours [bcours],  
    non mentionnés dans [lco] (aussi, si un cours admet un cours équivalent 
    qui est présent dans la liste [lco], il ne doit non plus être retenu), et 
    dont les préalables sont respectés. Aussi, dans la liste résultante, si un 
    cours est mentionné dans l'attribut «conco» d'un des cours [c] de la liste, 
    il doit alors soit figurer dans cette liste, soit figurer dans [lco]. Par 
    ailleurs, l'ordre des éléments de la liste résultante n'importe pas.

    Remarque: il n'est pas nécessaire de valider que tous les cours mentionnés
    dans [bcours] et [lco] respectent le format ###-$$$$ (autrement dit, la 
    fonction [ret_bcours_admissibles] n'est pas censée soulever des exceptions).
*)

val construit_liste_choix :
  ('a -> 'a -> bool) -> ('a list -> 'a list -> bool) -> 'a list -> 'a list list
(** [construit_liste_choix eq1 eq2 lst] retourne une liste de combinaisons
    (listes) de k éléments sélectionnés parmi n éléments de la liste [lst].
    Le paramètre [eq1] permet d'éviter les doublons dans les sous-listes 
    résultantes; le paramètre [eq2] permet d'éviter les doublons dans 
    la liste résultante.
*)

val respecte_regle :
  cours list -> num_cours list -> exigences -> int * exigences * num_cours list
(** [respecte_regle bcours lc regle] confronte la liste [lc] avec la règle
    [regle] de type {!exigences}. Elle retourne un triplet composé du nombre 
    de crédits obtenus avec la liste [lc], considérant la règle [regle], 
    d'une nouvelle version de la règle qui tient compte des cours de [lc] 
    qui ont partiellement ou totalement respecté la règle, ainsi que la liste 
    des cours de [lc] qui ne sont concernés par la règle.
*)

val conc_obtenues : cours list -> programme -> num_cours list -> string list
(** [conc_obtenues bcours pgm lco] la liste des concentrations obtenues dans
    le programme [pgm] étant donnée la liste [lco] de cours réussis. Dans ce 
    Tp, on suppose qu'un même cours peut contribuer à l'atteinte de deux 
    concentrations ou plus. 
*)

val ou_en_suis_je :
  cours list ->
  programme ->
  num_cours list ->
  (string * int * exigences) list
  * (int * int)
  * (string * int * exigences) list
  * (int * int)
  * num_cours list
(** [ou_en_suis_je bcours pgm lco] permet de savoir où on se situe dans le
    programme [pgm] considérant la liste [lco] de cours réussis. La fonction
    retourne un triplet composé de: la liste des règles qui demeurent à
    respecter pour la partie «Activités de formation obligatoires» du programme
    et une paire de nombres précisant le nombre de crédits exigés pour cette 
    partie ainsi que le nombre de crédits obtenus; la liste des règles 
    qui demeurent à respecter pour la partie «Autres exigences» du programme
    et une paire de nombres précisant le nombre de crédits exigés pour cette 
    partie ainsi que le nombre de crédits obtenus; et la liste des
    cours de [lco] qui n'auront contribué à une des exigences du programme.
*)

val respecte_conco : cours list -> num_cours list -> num_cours list -> bool
(** [respecte_conco bcours lco lc] permet de tester si les cours présents dans
    la liste [lc] sont corrects par rapport à l'exigence des «cours 
    concomitants», considérant les cours [lco] déjà réussis. 
*)

val choix_cours_admissibles :
  cours list ->
  num_cours list ->
  session ->
  fe list ->
  choix ->
  num_cours list ->
  num_cours list list
(** [choix_cours_admissibles bcours lco session lfe ch lc] retourne la liste 
    des différentes combinaisons de cours de [lc] respectant un ensemble de 
    critères: chaque cours, de chaque combinaison de cours, est admissible
    au niveau des préalables qu'il requiert, par rapport aux cours [lco] déjà 
    réussis; s'il requiert un nombre de crédits minimum (de cours du programme) 
    accumulés dans son cheminement, il doit le respecter; aussi, il doit être
    offert à la session [session] avec une formule d'enseignement précisée dans
    la liste [lfe] (si cette liste est vide, il n'y a aucune contrainte sur la
    formule d'enseignement). De plus, chaque combinaison de cours doit 
    totaliser un nombre de crédits respectant le choix [ch] de type {!choix}.
*)

val planifier_session :
  cours list ->
  programme ->
  num_cours list ->
  session ->
  fe list ->
  choix ->
  string list ->
  num_cours list ->
  (num_cours list * string list) list
(** [planifier_session bcours pgm lco session lfe ch lm lc_hp] retourne la liste 
    des listes de cours admissibles pour l'inscription à la session [session],
    avec une formule d'enseignement précisée dans la liste [lfe] (si cette 
    liste est vide, il n'y a aucune contrainte sur la formule d'enseignement),
    considérant la liste [lco] des cours déjà réussis. Chaque liste de 
    cours admissibles est accompagnée par la liste des nouvelles concentrations 
    qu'on obtiendrait avec ce choix de cours («nouvelles», c'est-à-dire, en plus 
    des concentrations déjà acquises). Notez que dans la construction des 
    différentes listes de cours résultantes, on privilégie en priorité 
    l'inscription au maximum de cours obligatoires admissibles et par la suite, 
    on complète avec les cours à option admissibles. Chaque liste de cours 
    admissibles doit totaliser un nombre de crédits respectant le choix [ch] 
    de type {!choix}. Finalement, l'argument [lc_hp] permet de limiter le 
    nombre de cours à considérer pour les règles utilisant des motifs de cours
    (avec le constructeur «CoursExclus»); ainsi les cours considérés pour ce 
    type de règles sont issus de ce paramètre modulo le fait qu'ils sont
    admissibles (respect des préalables; offert à la session précisée avec la
    bonne formule d'enseignement); de même pour le paramètre [lm] qui permet
    de limiter le nombre de cours à considérer au niveau des autres règles
    (constructeurs «Cours» et «CoursOB»).
*)

(*  ------------------------------------------------------------------------- *)
(** {1 Tp1, H-23}                                                             *)
(*  ------------------------------------------------------------------------- *)

(** {2 Interface des fonctions exportées}                                     *)

val eq_pr : prealables -> prealables -> bool
(** [eq_pr pr pr'] teste si 2 préalables [pr] et [pr'] passés en argument sont
    égaux. Notons que [ET lst] est égal à [ET lst'] si les 2 listes comprennent
    exactement les mêmes préalables (l'ordre d'apparition de ces préalables
    dans chaque liste n'ayant aucune importance).
*)

val prerequis : cours list -> num_cours -> prealables
(** [prerequis bcours c] retourne toutes la chaine de prérequis du cours [c]
    considérant une banque de cours [bcours].
    
    {b Soulève exception} {e Failure} si le cours [c] n'est pas défini dans 
    [bcours] (l'exception pourrait avoir été soulevée par une autre fonction 
    appelée dans le corps de [prerequis]).
*)

val extrait_lc : exigences -> num_cours list
(** [extrait_lc e] extrait la liste des cours mentionnés dans une exigence
    de programme défini avec le constructeur [CoursOB] ou [PlageCr]
    (pour ce dernier cas, on se restreint aux cours définis avec le 
    constructeur [Cours]).
*)

val extrait_lc_exclus : exigences -> num_cours list
(** [extrait_lc_exclus e] extrait la liste des cours mentionnés dans
    une exigence de programme défini avec le constructeur [PlageCr]
    puis [CoursExclus].
*)

val verif_e : cours list -> exigences -> (int * int) * bool
(** [verif_e bcours e] vérifie que le nombre de crédits mentionnés dans une 
    exigence est cohérent:
    - pour une exigence de type [CoursOB(n,l)], la fonction vérifie que
      le total de crédits des cours présents dans la liste [l] vaut bien [n];
      il retourne le total de crédits calculé, suivi du même nombre, suivi
      d'une valeur booléenne résultat de la comparaison;
    - pour une exigence de type [PlageCr(n1,n2,Cours l)], la fonction vérifie 
      que que la valeur [n2] est bien supérieure ou égale à [n1] et que le 
      total de crédits des cours présents dans la liste [l] est bien plus 
      grand ou égal à [n2]; il retourne [n1] et [n2] suivi du résultat de 
      la comparaison;
    - pour une exigence de type [PlageCr(n1,n2,CoursExclus l)], la fonction 
      retourne les 2 valeurs [n1] et [n2] suivies d'un test vérifiant
      que [n2] est bien supérieure ou égale à [n1].

    Rq: le nombre de crédits pour un cours n'est pas nécessairement 3.

    {b Soulève exception} {e Failure} si un des cours de [e] n'est pas dans le 
    bassin de cours. (l'exception pourrait avoir été soulevée par une autre 
    fonction appelée dans le corps de [verif_e]). 
*)

val existe_motif : cours list -> string -> bool
(** [existe_motif bcours m] teste si il existe au moins un cours, dans le 
    bassin de cours [bcours], qui respecte le motif [m]. 

   {b Soulève exception} {e Failure} si le motif [m] ne respecte pas un des 
   formats requis par la fonction [respecte_motif] (définie dans [Gcp]). 
    (l'exception pourrait avoir été soulevée par une autre fonction appelée 
    dans le corps de [existe_motif]).
*)

val existe_cours : cours list -> num_cours list -> bool
(** [existe_cours bcours lc] teste si tous les cours présents dans [lc] sont 
    bien définis dans le bassin de cours [bcours]. Notons que les éléments 
    de [lc] peuvent soit être des numéros de cours, soit des motifs de cours.

    {b Soulève exception} {e Failure} si un des éléments de [lc] ne respecte 
    pas un des formats requis par la fonction [respecte_motif] (définie dans 
    [Gcp]). (l'exception pourrait avoir été soulevée par une autre fonction 
    appelée dans le corps de [existe_cours]). 
*)

val verif_conc : cours list -> concentration -> bool
(** [verif_conc bcours conc] vérifie que la concentration [conc] est valide.
    Plus précisément, étant donné une concentration [conc], toutes les 
    exigences qui y sont définies doivent être valides, et le total des 
    minimus et maximums de crédits des exigences doivent borner le nombre 
    de crédits de la concentration.
*)

val verif_credits : cours list -> programme -> bool
(** [verif_credits bcours pgm] vérifie que les totaux de crédits définis dans 
    un programme [pgm] sont corrects. Plus précisément, étant donné un [pgm] 
    qui est défini par [((_,_, total, _),_,_,(n1,e1),(n2,e2),concs)]:
    - les exigences de [e1] sont vérifiées;
    - les exigences de [e2] sont vérifiées;
    - le total des crédits des cours définis dans [e1] vaut [n1];
    - le minimum et le maximum de crédits des cours définis dans [e2] bornent 
      [n2];
    - les concentrations sont vérifiées;
    - la somme de [n1] et [n2] vaut bien [total].
    Le calcul du nombre de crédits se fait comme suit:
    - Pour une exigence de type [CoursOB(n,l)], il s'agit simplement de calculer
      la somme des crédits des cours présents dans [l] (qui doit être égale à
      [n]);
    - Pour une exigence de type [PlageCr(n1,n2,Cours l)], il s'agit de calculer
      la somme des crédits des cours présents dans [l] (qui doit être comprise
      entre [n1] et [n2]);
    - Pour une exigence de type [PlageCr(n1,n2,CoursExclus l)], on ne peut calculer
      de somme de crédits; on peut simplement considérer que la somme est 
      comprise entre [n1] et [n2]; on doit aussi vérifier que [n2] est bien 
      supérieure ou égale à [n1].

    {b Soulève exception} {e Failure} si un des éléments de [pgm] ne respecte 
    pas un des formats requis par la fonction [respecte_motif] (définie dans 
    [Gcp]). (l'exception pourrait avoir été soulevée par une autre fonction 
    appelée dans le corps de [verif_credits]). 
*)

val coherence_cours_conc :
  concentration list ->
  num_cours list ->
  num_cours list ->
  num_cours list ->
  bool
(** [coherence_cours_conc lconc lc_ob lc_op lc_exclus] vérifie que les cours
    dans les concentrations [lconc] ne se retrouvent pas dans des cours
    obligatoires, représentés par la liste [lc_ob], mais plutôt dans les cours
    optionnels ou hors discipline représentés resp. par les listes [lc_op] et
    [lc_exclus]. 
*)

val verif_pre : cours list -> num_cours list -> num_cours list -> bool
(** [verif_pre bcours lc1 lc2] vérifie que les préalables des cours de la liste
    [lc1] sont satisfiables par la liste de cours [lc2]. La liste [lc2] ne 
    comprenant que des numéros de cours, un préalable de type [CRE _], à
    l'instar de [Aucun], est toujours satisfait.
*)

val coherence_pgm : cours list -> programme -> bool
(** [coherence_pgm bours pgm] teste la cohérence d'un programme [pgm], étant 
    donné un bassin de cours [bcours]:
   - tous les cours mentionnés dans [pgm] existent dans [bcours]; 
   - les différents sous-totaux de crédits sont cohérents; 
   - tous les cours mentionnés dans les concentrations de [pgm] ne sont pas 
     mentionnés dans partie obligatoire du programme, mais plutôt dans la 
     partie optionnel ou hors-discipline; 
   - tous les préalables des cours obligatoires se retrouvent dans la partie
     obligatoire du programme; 
   - tous les préalables de cours optionnels se retrouvent soit dans la partie
     obligatoire du programme, soit dans la partie optionnelle.
*)

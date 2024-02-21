(* -------------------------------------------------------------------------- *)
(* ----------------------- TP1 - IFT-3000 - Hiver 2024 ---------------------- *)
(* -------------------------------------------------------------------------- *)
(** Fichier permettant de tester les fonctions implantées du TP               *)
(* -------------------------------------------------------------------------- *)

open GcpLib
open Gcp
open Cours
open Programmes
open Tp1

(******************************************************************************)
(* JEUX DE DONNÉES
   Un jeu de données est composé d'un tuple comprenant les éléments suivants:
   - le nom de la fonction à tester;
   - une fonction qui permet de préciser comment le testeur doit agir avec les
     données et le résultat attendu pour chaque test;
   - une liste de tuples comprenant chaque donnée à tester, le résultat attendu,
     une chaine de caractères précisant le code testé (fonction et arguments);
   - en option, une même liste de tuples qui concernent les cas qui devraient
     soulever une exception, précédée par le code à utiliser pour lancer le test
     et donc, en théorie, provoquer une exception.
*)
(******************************************************************************)

(* Ajoutés par rapport à version 1 du testeur
   Définition de programmes fictifs
*)
let p1 : programme =
  ( (Bacc, "XYZ", 15, 1),
    "XYZ",
    [],
    ( 9,
      [
        ("OB", CoursOB (6, [ "IFT-1003"; "STT-1000" ]));
        ("R1", PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]));
      ] ),
    ( 6,
      [
        ("R2", PlageCr (3, 3, Cours [ "GLO-2100"; "IFT-2008" ]));
        ("R3", PlageCr (3, 3, CoursExclus [ "*-IFT" ]));
      ] ),
    [] )

and p_mp : programme =
  ( (MP, "MicroPgm", 6, 0),
    "Fictif",
    [],
    (3, [ ("R1", CoursOB (3, [ "IFT-1004" ])) ]),
    (3, [ ("R2", PlageCr (3, 3, Cours [ "GIF-1003" ])) ]),
    [] )

and p_vide : programme =
  ((Bacc, "Fictif", 0, 0), "Vide", [], (0, []), (0, []), [])

(* -------------------------------------------------------------------------- *)
let jeu_est_prerequis =
  let est_prerequis' = est_prerequis bcours in
  ( "est_prerequis",
    (fun (nc1, nc2) res -> est_prerequis' nc1 nc2 = res),
    [
      ( ("IFT-2008", "IFT-3000"),
        -1,
        {| est_prerequis bcours "IFT-2008" "IFT-3000" |} );
      ( ("IFT-3000", "IFT-2008"),
        1,
        {| est_prerequis bcours "IFT-3000" "IFT-2008" |} );
      ( ("IFT-3000", "IFT-3000"),
        0,
        {| est_prerequis bcours "IFT-3000" "IFT-3000" |} );
      ( ("IFT-3000", "IFT-1004"),
        0,
        {| est_prerequis bcours "IFT-3000" "IFT-1004" |} );
      ( ("GIF-1001", "IFT-1004"),
        1,
        {| est_prerequis bcours "GIF-1001" "IFT-1004" |} );
      (* Ajoutés par rapport à version 1 du testeur *)
      ( ("IFT-1004", "IFT-1004"),
        0,
        {| est_prerequis bcours "IFT-1004" "IFT-1004" |} );
      ( ("IFT-1903", "MAT-2910"),
        -1,
        {| est_prerequis bcours "IFT-1903" "MAT-2910" |} );
      ( ("MAT-2910", "IFT-1903"),
        1,
        {| est_prerequis bcours "MAT-2910" "IFT-1903" |} );
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    Some
      ( (fun (nc1, nc2) -> est_prerequis' nc1 nc2),
        [
          (("IFT-2008", "a"), {|est_prerequis bcours "IFT-2008" "a"|});
          (* Ajoutés par rapport à version 1 du testeur *)
          (("a", "IFT-2008"), {|est_prerequis bcours "a" "IFT-2008"|});
          (("", ""), {|est_prerequis bcours "" ""|});
        ] ) )

(* -------------------------------------------------------------------------- *)
let jeu_simp_pre =
  ( "simp_pre",
    (fun pr res -> eq_pr (simp_pre pr) res),
    [
      ( OU [ CP "a"; OU [ CP "b"; CP "a" ] ],
        OU [ CP "a"; CP "b" ],
        {|simp_pre (OU [ CP "a"; OU [ CP "b"; CP "a" ] ])|} );
      (OU [ CP "a"; CP "a" ], CP "a", {|sim_pre OU [ CP "a"; CP "a" ] ]|});
      ( OU [ CP "a"; CCP "a" ],
        OU [ CP "a"; CCP "a" ],
        {|simp_pre (OU [ CP "a"; CCP "a" ])|} );
      ( ET [ CRE 12; CRE 24; CRE 12 ],
        ET [ CRE 12; CRE 24 ],
        {|simp_pre (ET [ CRE 12; CRE 24; CRE 12 ])|} );
      ( OU [ Aucun; OU [ Aucun ]; Aucun ],
        Aucun,
        {|simp_pre (OU [ Aucun; OU [ Aucun ]; Aucun ])|} );
      ( ET
          [
            CP "a";
            OU [ CP "b"; ET [ CP "a"; CP "c" ]; CP "b"; Aucun ];
            ET [ Aucun; CP "a"; CP "d" ];
          ],
        ET [ CP "a"; OU [ CP "b"; ET [ CP "a"; CP "c" ] ]; CP "d" ],
        {|simp_pre (ET
        [
          CP "a";
          OU [ CP "b"; ET [ CP "a"; CP "c" ]; CP "b"; Aucun ];
          ET [ Aucun; CP "a"; CP "d" ];
        ]) ]|}
      );
      ( ET [ CP "a"; OU [ CP "b" ] ],
        ET [ CP "a"; CP "b" ],
        {|simp_pre (ET [ CP "a"; OU [ CP "b" ] ])|} );
      (* Ajoutés par rapport à version 1 du testeur *)
      (Aucun, Aucun, {|simp_pre Aucun|});
      (CP "a", CP "a", {|simp_pre (CP "a")|});
      (CCP "a", CCP "a", {|simp_pre (CCP "a")|});
      (CRE 12, CRE 12, {|simp_pre (CRE 12)|});
      ( OU [ CP "a"; CCP "a"; CRE 12 ],
        OU [ CP "a"; CCP "a"; CRE 12 ],
        {|simp_pre (OU [CP "a"; CCP "a"; CRE 12])|} );
      ( ET [ CP "a"; CCP "a"; CRE 12 ],
        ET [ CP "a"; CCP "a"; CRE 12 ],
        {|simp_pre (ET [CP "a"; CCP "a"; CRE 12])|} );
      ( OU [ Aucun; OU [ Aucun ]; Aucun ],
        Aucun,
        {|simp_pre (OU [ Aucun; OU [ Aucun ]; Aucun ])|} );
      (OU [], Aucun, {|simp_pre (OU [])|});
      (ET [], Aucun, {|simp_pre (ET [])|});
      ( OU
          [
            CRE 12;
            OU [ CP "a"; CRE 12 ];
            CP "a";
            OU [ OU [ CRE 12 ]; OU [ CP "a" ] ];
          ],
        OU [ CRE 12; CP "a" ],
        {|simp_pre (OU[CRE 12; OU [CP "a"; CRE 12]; CP "a"; OU [OU[CRE 12]; 
          OU[CP "a"]]])|}
      );
      ( ET
          [
            CRE 12;
            ET [ CP "a"; CRE 12 ];
            CP "a";
            ET [ ET [ CRE 12 ]; ET [ CP "a" ] ];
          ],
        ET [ CRE 12; CP "a" ],
        {|simp_pre (ET[CRE 12; ET [CP "a"; CRE 12]; CP "a"; ET [ET[CRE 12]; 
          ET[CP "a"]]])|}
      );
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    None )

(* -------------------------------------------------------------------------- *)
let jeu_seuls_cours_pgm_dans_pre =
  ( "seuls_cours_pgm_dans_pre",
    (fun (lncp, pre) res -> eq_pr (seuls_cours_pgm_dans_pre lncp pre) res),
    [
      (([], Aucun), Aucun, {|seuls_cours_pgm_dans_pre [] Aucun|});
      (([], CRE 12), CRE 12, {|seuls_cours_pgm_dans_pre [] (CRE 12)|});
      (([], CP "a"), Aucun, {|seuls_cours_pgm_dans_pre [] (CP "a")|});
      ( ([ "a"; "b"; "c"; "d" ], CCP "b"),
        CCP "b",
        {|seuls_cours_pgm_dans_pre ["a"; "b"; "c"; "d"] (CCP "b")|} );
      ( ([ "a"; "b"; "c"; "d" ], CP "e"),
        Aucun,
        {|seuls_cours_pgm_dans_pre ["a"; "b"; "c"; "d"] (CP "e")|} );
      ( ([ "a"; "b" ], OU [ CP "b"; CRE 12; CP "e"; CRE 12 ]),
        OU [ CP "b"; CRE 12 ],
        {|seuls_cours_pgm_dans_pre ["a"; "b"] 
          (OU [CP "b"; CRE 12; CP "e"; CRE 12])|}
      );
      ( ( [ "a"; "b"; "c"; "d" ],
          ET [ CCP "b"; OU [ CCP "b"; CP "e" ]; OU [ CP "e"; CP "f" ] ] ),
        CCP "b",
        {|seuls_cours_pgm_dans_pre ["a"; "b"; "c"; "d"] 
        (ET [CCP "b"; OU [CCP "b"; CP "e"]; OU [CP "e"; CP "f"]])|}
      );
      (* Ajoutés par rapport à version 1 du testeur *)
      ( ( [
            "GIF-1001";
            "IFT-1004";
            "IFT-1111";
            "MAT-1200";
            "MAT-1919";
            "IFT-1000";
            "IFT-1003";
            "STT-1000";
            "IFT-2002";
            "IFT-3000";
            "IFT-3001";
            "IFT-3101";
            "IFT-2580";
            "IFT-3580";
            "GIF-1003";
            "IFT-1006";
            "GLO-2000";
            "IFT-2006";
            "GLO-2001";
            "IFT-2001";
            "GLO-2100";
            "IFT-2008";
            "GLO-2004";
            "IFT-2007";
            "IFT-2004";
            "GLO-2005";
          ],
          OU
            [
              CP "MQT-1102";
              CP "MQT-1100";
              CP "STT-2920";
              CP "MAT-1915";
              CP "STT-1900";
              CP "STT-1000";
            ] ),
        CP "STT-1000",
        {|seuls_cours_pgm_dans_pre ["GIF-1001"; "IFT-1004"; "IFT-1111"; "MAT-1200"; "MAT-1919"; "IFT-1000";
        "IFT-1003"; "STT-1000"; "IFT-2002"; "IFT-3000"; "IFT-3001"; "IFT-3101";
        "IFT-2580"; "IFT-3580"; "GIF-1003"; "IFT-1006"; "GLO-2000"; "IFT-2006";
        "GLO-2001"; "IFT-2001"; "GLO-2100"; "IFT-2008"; "GLO-2004"; "IFT-2007";
        "IFT-2004"; "GLO-2005"] (OU
        [CP "MQT-1102"; CP "MQT-1100"; CP "STT-2920"; CP "MAT-1915"; CP "STT-1900";
         CP "STT-1000"])|}
      );
      ( ( [
            "GLO-2004";
            "IFT-1003";
            "IFT-1700";
            "IFT-2001";
            "IFT-2004";
            "IFT-2006";
            "IFT-2007";
            "IFT-2103";
            "IFT-3000";
            "IFT-3002";
            "IFT-3100";
          ],
          ET
            [
              OU [ CP "GIF-1003"; CP "IFT-1006" ];
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
            ] ),
        Aucun,
        {|seuls_cours_pgm_dans_pre ["GLO-2004"; "IFT-1003"; "IFT-1700"; "IFT-2001"; "IFT-2004"; "IFT-2006";
        "IFT-2007"; "IFT-2103"; "IFT-3000"; "IFT-3002"; "IFT-3100"] (ET
                   [
                     OU [ CP "GIF-1003"; CP "IFT-1006" ];
                     OU [ CP "IFT-2008"; CP "GLO-2100" ];
                   ])|}
      );
      ( ( [
            "GIF-1001";
            "IFT-1004";
            "GIF-1003";
            "IFT-1006";
            "GLO-2100";
            "IFT-2008";
          ],
          ET
            [
              OU [ CP "GIF-1003"; CP "IFT-1006" ];
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
            ] ),
        ET
          [
            OU [ CP "GIF-1003"; CP "IFT-1006" ];
            OU [ CP "IFT-2008"; CP "GLO-2100" ];
          ],
        {|seuls_cours_pgm_dans_pre ["GIF-1001"; "IFT-1004"; "GIF-1003"; "IFT-1006"; "GLO-2100"; "IFT-2008"] (ET
        [
          OU [ CP "GIF-1003"; CP "IFT-1006" ];
          OU [ CP "IFT-2008"; CP "GLO-2100" ];
        ])|}
      );
      ( ( [
            "GIF-1002";
            "GLO-1111";
            "GLO-1901";
            "MAT-1900";
            "MAT-1919";
            "GEL-1001";
            "GIF-1001";
            "GIF-1003";
            "MAT-1910";
            "GLO-2000";
            "GLO-2004";
            "GLO-2100";
            "GLO-3101";
            "STT-2920";
            "ECN-2901";
            "GLO-2001";
            "GLO-2003";
            "GLO-2005";
            "GLO-3102";
            "GLO-4000";
            "GLO-4002";
            "IFT-3001";
            "MAT-2930";
            "GEL-4799";
            "GLO-3013";
            "GMN-2901";
            "IFT-2002";
            "GLO-3004";
            "GLO-4003";
            "PHI-2910";
            "PHI-3900";
            "GLO-3002";
            "MAT-2910";
            "GLO-2580";
            "GLO-2581";
            "GLO-3100";
            "GLO-3202";
          ],
          ET
            [
              OU [ CP "IFT-2008"; CP "GLO-2100" ];
              OU [ CP "STT-1000"; CP "STT-2920"; CP "STT-2000"; CP "MQT-1102" ];
              OU [ CP "MAT-1310"; CP "MAT-1919" ];
            ] ),
        ET [ CP "GLO-2100"; CP "STT-2920"; CP "MAT-1919" ],
        {|seuls_cours_pgm_dans_pre ["GIF-1002"; "GLO-1111"; "GLO-1901"; "MAT-1900"; "MAT-1919"; "GEL-1001";
        "GIF-1001"; "GIF-1003"; "MAT-1910"; "GLO-2000"; "GLO-2004"; "GLO-2100";
        "GLO-3101"; "STT-2920"; "ECN-2901"; "GLO-2001"; "GLO-2003"; "GLO-2005";
        "GLO-3102"; "GLO-4000"; "GLO-4002"; "IFT-3001"; "MAT-2930"; "GEL-4799";
        "GLO-3013"; "GMN-2901"; "IFT-2002"; "GLO-3004"; "GLO-4003"; "PHI-2910";
        "PHI-3900"; "GLO-3002"; "MAT-2910"; "GLO-2580"; "GLO-2581"; "GLO-3100";
        "GLO-3202"] (ET
                   [
                     OU [ CP "IFT-2008"; CP "GLO-2100" ];
                     OU [ CP "STT-1000"; CP "STT-2920"; CP "STT-2000"; 
                          CP "MQT-1102" ];
                     OU [ CP "MAT-1310"; CP "MAT-1919" ];
                   ])|}
      );
      ( ( [ "GIF-1001"; "IFT-1004"; "GIF-1003"; "IFT-1006"; "IFT-2008" ],
          ET
            [
              OU [ CP "GLO-2100"; CP "IFT-2008" ];
              OU [ CP "MAT-1200"; CP "MAT-2930"; CP "PHY-1001" ];
              OU [ CCP "IFT-4102"; CCP "GIF-4101" ];
            ] ),
        CP "IFT-2008",
        {|seuls_cours_pgm_dans_pre [] Aucun|} );
      (([ "a" ], Aucun), Aucun, {|seuls_cours_pgm_dans_pre ["a"] Aucun|});
      (([ "a" ], CRE 12), CRE 12, {|seuls_cours_pgm_dans_pre ["a"] (CRE 12)|});
      (([ "a" ], CP "a"), CP "a", {|seuls_cours_pgm_dans_pre ["a"] (CP "a")|});
      (([ "a" ], CCP "a"), CCP "a", {|seuls_cours_pgm_dans_pre ["a"] (CCP "a")|});
      ( ([ "a" ], OU [ CCP "a" ]),
        CCP "a",
        {|seuls_cours_pgm_dans_pre ["a"] (OU[CCP "a"])|} );
      ( ([ "a" ], ET [ CCP "a" ]),
        CCP "a",
        {|seuls_cours_pgm_dans_pre ["a"] (ET[CCP "a"])|} );
      (([ "b" ], CP "a"), Aucun, {|seuls_cours_pgm_dans_pre ["b"] (CP "a")|});
      (([ "b" ], CCP "a"), Aucun, {|seuls_cours_pgm_dans_pre ["b"] (CCP "a")|});
      ( ([ "b" ], OU [ CCP "a" ]),
        Aucun,
        {|seuls_cours_pgm_dans_pre ["b"] (OU[CCP "a"])|} );
      ( ([ "b" ], ET [ CCP "a" ]),
        Aucun,
        {|seuls_cours_pgm_dans_pre ["b"] (ET[CCP "a"])|} );
      ( ( [],
          ET [ ET [ Aucun; Aucun ]; Aucun; ET [ ET [ Aucun ]; ET [ Aucun ] ] ]
        ),
        Aucun,
        {|seuls_cours_pgm_dans_pre [] (ET[ET [Aucun; Aucun]; Aucun; ET 
          [ET[Aucun]; ET[Aucun]]])|}
      );
      ( ( [],
          ET
            [
              CRE 12;
              ET [ CP "a"; CRE 12 ];
              CP "a";
              ET [ ET [ CRE 12 ]; ET [ CP "a" ] ];
            ] ),
        CRE 12,
        {|seuls_cours_pgm_dans_pre [] (ET[CRE 12; ET [CP "a"; CRE 12]; CP "a"; 
          ET [ET[CRE 12]; ET[CP "a"]]])|}
      );
      ( ( [ "a" ],
          ET
            [
              CRE 12;
              ET [ CP "a"; CRE 12 ];
              CP "a";
              ET [ ET [ CRE 12 ]; ET [ CP "a" ] ];
            ] ),
        ET [ CRE 12; CP "a" ],
        {|seuls_cours_pgm_dans_pre ["a"] (ET[CRE 12; ET [CP "a"; CRE 12]; CP "a"; 
          ET [ET[CRE 12]; ET[CP "a"]]])|}
      );
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    None )

(* -------------------------------------------------------------------------- *)
let jeu_cours_pgm_par_type =
  ( "cours_pgm_par_type",
    (fun (pgm, ch) res -> eql (cours_pgm_par_type pgm ch) res),
    [
      ( (b_iig, OB),
        [
          "GIF-1001";
          "IFT-1004";
          "MNG-1000";
          "MQT-1102";
          "SIO-2103";
          "CTB-1000";
          "SIO-2100";
          "SIO-2105";
          "GSO-1000";
          "SIO-2104";
          "SIO-3100";
          "GSF-1000";
          "SIO-2107";
          "GLO-4000";
          "MRK-1000";
          "GLO-2003";
          "GLO-4002";
          "SIO-2102";
          "SIO-3110";
          "IFT-2580";
          "GIF-1003";
          "IFT-1006";
          "GLO-2001";
          "IFT-2001";
          "GLO-2000";
          "IFT-2006";
          "GLO-2004";
          "IFT-2007";
          "GLO-2100";
          "IFT-2008";
          "GLO-2005";
          "IFT-2004";
          "GIN-3060";
          "SIO-2110";
        ],
        {|cours_pgm_par_type b_iig OB|} );
      ( (b_iig, OP),
        [
          "GIF-3101";
          "GLO-3100";
          "GLO-3102";
          "GLO-3202";
          "GLO-4003";
          "GLO-4008";
          "GLO-4035";
          "IFT-2002";
          "IFT-2003";
          "IFT-2101";
          "IFT-2102";
          "IFT-3000";
          "IFT-3001";
          "IFT-3002";
          "IFT-3100";
          "IFT-3101";
          "IFT-3201";
          "IFT-4100";
          "MAT-1919";
          "ANL-2020";
        ],
        {|cours_pgm_par_type b_iig OP|} );
      ( (b_iig, Conc),
        [
          "GLO-3100";
          "GLO-3202";
          "IFT-2102";
          "IFT-3002";
          "IFT-3201";
          "IFT-4100";
          "ANL-2020";
        ],
        {|cours_pgm_par_type b_iig Conc|} );
      ( (mp_base, OB),
        [
          "GIF-1001"; "IFT-1004"; "GIF-1003"; "IFT-1006"; "GLO-2100"; "IFT-2008";
        ],
        {|cours_pgm_par_type mp_base OB|} );
      ( (mp_base, OP),
        [
          "GLO-2004";
          "IFT-1003";
          "IFT-1700";
          "IFT-2001";
          "IFT-2004";
          "IFT-2006";
          "IFT-2007";
          "IFT-2103";
          "IFT-3000";
          "IFT-3002";
          "IFT-3100";
        ],
        {|cours_pgm_par_type mp_base OP|} );
      ((mp_base, Conc), [], {|cours_pgm_par_type mp_base Conc|});
      ((mp_jv, OP), [], {|cours_pgm_par_type mp_jv OP|});
      (* Ajoutés par rapport à version 1 du testeur *)
      ( (b_ift, OB),
        [
          "GIF-1001";
          "IFT-1004";
          "IFT-1111";
          "MAT-1200";
          "MAT-1919";
          "IFT-1000";
          "IFT-1003";
          "STT-1000";
          "IFT-2002";
          "IFT-3000";
          "IFT-3001";
          "IFT-3101";
          "IFT-2580";
          "IFT-3580";
          "GIF-1003";
          "IFT-1006";
          "GLO-2000";
          "IFT-2006";
          "GLO-2001";
          "IFT-2001";
          "GLO-2100";
          "IFT-2008";
          "GLO-2004";
          "IFT-2007";
          "IFT-2004";
          "GLO-2005";
        ],
        {|cours_pgm_par_type b_ift OB|} );
      ( (b_ift, OP),
        [
          "ANL-2020";
          "ANL-3020";
          "BIF-4007";
          "GIF-3101";
          "GIF-4100";
          "GIF-4101";
          "GIF-4104";
          "GIF-4105";
          "GLO-2003";
          "GLO-3100";
          "GLO-3101";
          "GLO-3102";
          "GLO-3112";
          "GLO-3202";
          "GLO-4000";
          "GLO-4001";
          "GLO-4003";
          "GLO-4008";
          "GLO-4009";
          "GLO-4010";
          "GLO-4027";
          "GLO-4030";
          "GLO-4035";
          "IFT-2101";
          "IFT-2102";
          "IFT-2103";
          "IFT-3002";
          "IFT-3100";
          "IFT-3113";
          "IFT-4001";
          "IFT-4003";
          "IFT-4021";
          "IFT-4022";
          "IFT-4029";
          "IFT-4030";
          "IFT-4100";
          "IFT-4201";
          "SIO-2100";
          "SIO-2102";
          "SIO-2104";
          "SIO-2105";
          "SIO-2107";
          "GLO-3004";
          "GLO-4002";
          "IFT-3201";
          "IFT-2003";
          "IFT-4102";
        ],
        {|cours_pgm_par_type b_ift OP|} );
      ( (b_ift, Conc),
        [
          "IFT-4001";
          "IFT-4102";
          "BCM-1001";
          "BCM-1003";
          "BCM-1005";
          "BIF-1000";
          "BIF-1001";
          "BIF-4007";
          "GIF-4104";
          "GLO-2003";
          "GLO-3004";
          "GLO-3100";
          "GLO-3101";
          "GLO-3102";
          "GLO-3112";
          "GLO-4000";
          "GLO-4002";
          "GLO-4003";
          "GLO-4008";
          "GLO-4035";
          "IFT-3201";
          "GLO-3102";
          "GIF-3101";
          "GLO-3100";
          "GLO-3112";
          "GLO-4000";
          "GLO-4002";
          "IFT-2101";
          "IFT-3201";
          "GIF-3101";
          "GIF-4100";
          "GIF-4104";
          "GIF-4105";
          "GLO-4000";
          "IFT-2103";
          "IFT-3100";
          "IFT-3113";
          "IFT-4102";
          "PHY-1903";
          "GLO-3100";
          "GLO-3202";
          "IFT-2102";
          "IFT-3002";
          "IFT-3201";
          "SIO-2102";
          "GLO-3101";
          "GLO-4035";
          "IFT-4001";
          "SIO-2102";
          "SIO-2104";
          "GIF-4100";
          "GIF-4101";
          "GLO-4001";
          "GLO-4030";
          "IFT-2003";
          "IFT-3100";
          "IFT-4001";
          "IFT-4022";
          "IFT-4030";
          "IFT-4102";
          "GLO-4027";
          "GLO-4035";
          "GIF-4101";
          "GIF-4104";
          "GLO-4030";
          "IFT-4001";
          "IFT-4102";
          "STT-2200";
        ],
        {|cours_pgm_par_type b_ift Conc|} );
      ( (p1, OB),
        [ "IFT-1003"; "STT-1000"; "GLO-2100"; "IFT-2008" ],
        {|cours_pgm_par_type p1 OB|} );
      ((p1, OP), [ "GLO-2100"; "IFT-2008" ], {|cours_pgm_par_type p1 OP|});
      ((p1, Conc), [], {|cours_pgm_par_type p1 Conc|});
      ((p_mp, OB), [ "IFT-1004" ], {|cours_pgm_par_type p_mp OB|});
      ((p_mp, OP), [ "GIF-1003" ], {|cours_pgm_par_type p_mp OP|});
      ((p_vide, OB), [], {|cours_pgm_par_type p_vide OB|});
      ((p_vide, OP), [], {|cours_pgm_par_type p_vide OP|});
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    None )

(* -------------------------------------------------------------------------- *)
let jeu_cours_pgm =
  ( "cours_pgm",
    (fun pgm res -> eql (cours_pgm pgm) res),
    [
      ( mp_tdm,
        [
          "IFT-1004";
          "GLO-4035";
          "GLO-4027";
          "GIF-1003";
          "IFT-1006";
          "GLO-2005";
          "IFT-2004";
          "GLO-2100";
          "IFT-2008";
        ],
        {|cours_pgm mp_tdm|} );
      ( b_ift,
        [
          "GIF-1001";
          "IFT-1004";
          "IFT-1111";
          "MAT-1200";
          "MAT-1919";
          "IFT-1000";
          "IFT-1003";
          "STT-1000";
          "IFT-2002";
          "IFT-3000";
          "IFT-3001";
          "IFT-3101";
          "IFT-2580";
          "IFT-3580";
          "GIF-1003";
          "IFT-1006";
          "GLO-2000";
          "IFT-2006";
          "GLO-2001";
          "IFT-2001";
          "GLO-2100";
          "IFT-2008";
          "GLO-2004";
          "IFT-2007";
          "IFT-2004";
          "GLO-2005";
          "ANL-2020";
          "ANL-3020";
          "BIF-4007";
          "GIF-3101";
          "GIF-4100";
          "GIF-4101";
          "GIF-4104";
          "GIF-4105";
          "GLO-2003";
          "GLO-3100";
          "GLO-3101";
          "GLO-3102";
          "GLO-3112";
          "GLO-3202";
          "GLO-4000";
          "GLO-4001";
          "GLO-4003";
          "GLO-4008";
          "GLO-4009";
          "GLO-4010";
          "GLO-4027";
          "GLO-4030";
          "GLO-4035";
          "IFT-2101";
          "IFT-2102";
          "IFT-2103";
          "IFT-3002";
          "IFT-3100";
          "IFT-3113";
          "IFT-4001";
          "IFT-4003";
          "IFT-4021";
          "IFT-4022";
          "IFT-4029";
          "IFT-4030";
          "IFT-4100";
          "IFT-4201";
          "SIO-2100";
          "SIO-2102";
          "SIO-2104";
          "SIO-2105";
          "SIO-2107";
          "GLO-3004";
          "GLO-4002";
          "IFT-3201";
          "IFT-2003";
          "IFT-4102";
          "BCM-1001";
          "BCM-1003";
          "BCM-1005";
          "BIF-1000";
          "BIF-1001";
          "PHY-1903";
          "STT-2200";
        ],
        {|cours_pgm b_ift|} );
      ( c_ift,
        [
          "GIF-1001";
          "IFT-1004";
          "GIF-1003";
          "IFT-1006";
          "GLO-2100";
          "IFT-2008";
          "GIF-4104";
          "GLO-2003";
          "GLO-3101";
          "GLO-3102";
          "GLO-3202";
          "GLO-4000";
          "GLO-4002";
          "GLO-4003";
          "GLO-4008";
          "GLO-4009";
          "GLO-4027";
          "GLO-4035";
          "IFT-1003";
          "IFT-1700";
          "IFT-2101";
          "IFT-2102";
          "IFT-2103";
          "IFT-3000";
          "IFT-3002";
          "IFT-3100";
          "IFT-3201";
          "IFT-4029";
          "SIO-2102";
          "SIO-2104";
          "GLO-3112";
          "IFT-3113";
          "GLO-2004";
          "IFT-2007";
          "GLO-2000";
          "IFT-2006";
          "GLO-2001";
          "IFT-2001";
          "GLO-2005";
          "IFT-2004";
        ],
        {|cours_pgm c_ift|} );
      (* Ajoutés par rapport à version 1 du testeur *)
      ( b_glo,
        [
          "GIF-1002";
          "GLO-1111";
          "GLO-1901";
          "MAT-1900";
          "MAT-1919";
          "GEL-1001";
          "GIF-1001";
          "GIF-1003";
          "MAT-1910";
          "GLO-2000";
          "GLO-2004";
          "GLO-2100";
          "GLO-3101";
          "STT-2920";
          "ECN-2901";
          "GLO-2001";
          "GLO-2003";
          "GLO-2005";
          "GLO-3102";
          "GLO-4000";
          "GLO-4002";
          "IFT-3001";
          "MAT-2930";
          "GEL-4799";
          "GLO-3013";
          "GMN-2901";
          "IFT-2002";
          "GLO-3004";
          "GLO-4003";
          "PHI-2910";
          "PHI-3900";
          "GLO-3002";
          "MAT-2910";
          "GLO-2580";
          "GLO-2581";
          "GLO-3100";
          "GLO-3202";
          "ANL-2020";
          "ANL-3020";
          "BIF-4007";
          "GEL-1000";
          "GIF-3000";
          "GIF-3004";
          "GIF-3101";
          "GIF-4100";
          "GIF-4101";
          "GIF-4104";
          "GIF-4105";
          "GLO-3112";
          "GLO-4001";
          "GLO-4007";
          "GLO-4008";
          "GLO-4009";
          "GLO-4010";
          "GLO-4027";
          "GLO-4030";
          "GLO-4035";
          "IFT-2102";
          "IFT-2103";
          "IFT-3000";
          "IFT-3002";
          "IFT-3100";
          "IFT-3101";
          "IFT-3113";
          "IFT-3201";
          "IFT-4001";
          "IFT-4003";
          "IFT-4021";
          "IFT-4022";
          "IFT-4029";
          "IFT-4030";
          "IFT-4102";
          "IFT-4201";
          "MAT-2200";
          "STT-2200";
          "MAT-1200";
          "ANL-3905";
          "DDU-1000";
          "EDC-4000";
          "ENT-1000";
          "GSF-1020";
          "GSO-1000";
          "MRK-1000";
          "PHI-1900";
          "RLT-1000";
          "RLT-1700";
          "BCM-1001";
          "BCM-1003";
          "BIO-1910";
          "BIO-2003";
          "BIO-4900";
          "BIO-4902";
          "CHM-1000";
          "CHM-1003";
          "CHM-1905";
          "GGR-2305";
          "GMC-1003";
          "MCB-1000";
          "PHY-1000";
          "PHY-1003";
          "PHY-1006";
          "PHY-1007";
          "PHY-2100";
          "SBM-1004";
        ],
        {|cours_pgm b_glo|} );
      ( b_iig,
        [
          "GIF-1001";
          "IFT-1004";
          "MNG-1000";
          "MQT-1102";
          "SIO-2103";
          "CTB-1000";
          "SIO-2100";
          "SIO-2105";
          "GSO-1000";
          "SIO-2104";
          "SIO-3100";
          "GSF-1000";
          "SIO-2107";
          "GLO-4000";
          "MRK-1000";
          "GLO-2003";
          "GLO-4002";
          "SIO-2102";
          "SIO-3110";
          "IFT-2580";
          "GIF-1003";
          "IFT-1006";
          "GLO-2001";
          "IFT-2001";
          "GLO-2000";
          "IFT-2006";
          "GLO-2004";
          "IFT-2007";
          "GLO-2100";
          "IFT-2008";
          "GLO-2005";
          "IFT-2004";
          "GIN-3060";
          "SIO-2110";
          "GIF-3101";
          "GLO-3100";
          "GLO-3102";
          "GLO-3202";
          "GLO-4003";
          "GLO-4008";
          "GLO-4035";
          "IFT-2002";
          "IFT-2003";
          "IFT-2101";
          "IFT-2102";
          "IFT-3000";
          "IFT-3001";
          "IFT-3002";
          "IFT-3100";
          "IFT-3101";
          "IFT-3201";
          "IFT-4100";
          "MAT-1919";
          "ANL-2020";
        ],
        {|cours_pgm b_iig|} );
      (p1, [ "IFT-1003"; "STT-1000"; "GLO-2100"; "IFT-2008" ], {|cours_pgm p1|});
      (p_mp, [ "IFT-1004"; "GIF-1003" ], {|cours_pgm p_mp|});
      (p_vide, [], {|cours_pgm p_vide|});
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    None )

(* -------------------------------------------------------------------------- *)
let jeu_cours_contrib_dans_pgm =
  ( "cours_contrib_dans_pgm",
    (fun (nc, lpgms) res -> eql (cours_contrib_dans_pgm nc lpgms) res),
    [
      ( ("IFT-3000", l_pgms),
        [
          ("B-IFT", Some OB);
          ("B-IIG", Some OP);
          ("B-GLO", Some OP);
          ("C-IFT", Some OP);
          ("MP-BASE", Some OP);
          ("MP-GLO", None);
          ("MP-WEB", None);
          ("MP-JV", None);
          ("MP-TDM", None);
        ],
        {|cours_contrib_dans_pgm "IFT-3000" l_pgms|} );
      ( ("IFT-1004", l_pgms),
        [
          ("B-IFT", Some OB);
          ("B-IIG", Some OB);
          ("B-GLO", None);
          ("C-IFT", Some OB);
          ("MP-BASE", Some OB);
          ("MP-GLO", Some OB);
          ("MP-WEB", Some OB);
          ("MP-JV", Some OB);
          ("MP-TDM", Some OB);
        ],
        {|cours_contrib_dans_pgm "IFT-1004" l_pgms|} );
      ( ("GLO-4010", l_pgms),
        [
          ("B-IFT", Some OP);
          ("B-IIG", None);
          ("B-GLO", Some OP);
          ("C-IFT", None);
          ("MP-BASE", None);
          ("MP-GLO", None);
          ("MP-WEB", None);
          ("MP-JV", None);
          ("MP-TDM", None);
        ],
        {|cours_contrib_dans_pgm "GLO-4010" l_pgms|} );
      ( ("a", l_pgms),
        [
          ("B-IFT", None);
          ("B-IIG", None);
          ("B-GLO", None);
          ("C-IFT", None);
          ("MP-BASE", None);
          ("MP-GLO", None);
          ("MP-WEB", None);
          ("MP-JV", None);
          ("MP-TDM", None);
        ],
        {|cours_contrib_dans_pgm "a" l_pgms|} );
      (("IFT-1004", []), [], {|cours_contrib_dans_pgm "IFT-1004" []|});
      ( ("IFT-1004", [ ("B-GLO", b_glo) ]),
        [ ("B-GLO", None) ],
        {|cours_contrib_dans_pgm "IFT-1004" ["B-GLO",b_glo]|} );
      (* Ajoutés par rapport à version 1 du testeur *)
      (("", []), [], {|cours_contrib_dans_pgm "" []|});
      ( ("IFT-2008", [ ("p1", p1) ]),
        [ ("p1", Some OB) ],
        {|cours_contrib_dans_pgm "IFT-2008" [("p1", p1)]|} );
      ( ("IFT-1004", [ ("p_vide", p_vide) ]),
        [ ("p_vide", None) ],
        {|cours_contrib_dans_pgm "IFT-1004" [("p_vide", p_vide)]|} );
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    None )

(* -------------------------------------------------------------------------- *)
let jeu_regroupe_cours_equiv =
  let regroupe_cours_equiv' = regroupe_cours_equiv bcours in
  ( "regroupe_cours_equiv",
    (fun lnc res -> eql (regroupe_cours_equiv' lnc) res),
    [
      ([], [], {|regroupe_cours_equiv bcours []|});
      ( [ "IFT-3000" ],
        [ [ "IFT-3000" ] ],
        {|regroupe_cours_equiv bcours ["IFT-3000"]|} );
      ( [ "IFT-3000"; "IFT-1004" ],
        [ [ "IFT-3000" ]; [ "IFT-1004" ] ],
        {|regroupe_cours_equiv bcours ["IFT-3000"; "IFT-1004"]|} );
      ( [ "IFT-1004"; "IFT-3000"; "GLO-1901" ],
        [ [ "GLO-1901"; "IFT-1004" ]; [ "IFT-3000" ] ],
        {|regroupe_cours_equiv bcours ["IFT-1004"; "IFT-3000"; "GLO-1901"]|} );
      ( [ "GLO-2100"; "IFT-1004"; "IFT-2008"; "GLO-1901" ],
        [ [ "GLO-2100"; "IFT-2008" ]; [ "GLO-1901"; "IFT-1004" ] ],
        {|regroupe_cours_equiv bcours ["GLO-2100"; "IFT-1004"; "IFT-2008"; 
         "GLO-1901"]|}
      );
      ( [ "GLO-7003"; "GLO-1901"; "GLO-4010" ],
        [ [ "GLO-7003" ]; [ "GLO-1901" ]; [ "GLO-4010" ] ],
        {|regroupe_cours_equiv bcours ["GLO-7003"; "GLO-1901"; "GLO-4010"]|} );
      (* Ajoutés par rapport à version 1 du testeur *)
      ( [
          "IFT-2001";
          "GLO-2100";
          "IFT-1006";
          "GLO-2000";
          "GIF-1003";
          "IFT-1004";
          "GLO-2001";
          "IFT-2008";
          "GLO-1901";
          "IFT-2006";
        ],
        [
          [ "GLO-2001"; "IFT-2001" ];
          [ "GLO-2100"; "IFT-2008" ];
          [ "GIF-1003"; "IFT-1006" ];
          [ "GLO-2000"; "IFT-2006" ];
          [ "GLO-1901"; "IFT-1004" ];
        ],
        {|regroupe_cours_equiv bcours [
          "IFT-2001";
          "GLO-2100";
          "IFT-1006";
          "GLO-2000";
          "GIF-1003";
          "IFT-1004";
          "GLO-2001";
          "IFT-2008";
          "GLO-1901";
          "IFT-2006";
        ]|}
      );
    ],
    (* ---- Cas devant soulever une exception! ---- *)
    Some
      ( (fun lnc -> regroupe_cours_equiv' lnc),
        [
          ([ "a" ], {|regroupe_cours_equiv bcours ["a"]|});
          ([ "IFT-3000"; "b" ], {|regroupe_cours_equiv bcours ["IFT-3000";"b"]|});
        ] ) )

(*****************************************************************************)
(* ------------------------------------------------------------------------- *)
(* -- Test générique--------------------------------------------------------
   La fonction testg a comme signature (donc, extrêmement générique):
      'a * ('b -> 'c -> bool) * ('b * 'c * string) list *
      (('d -> 'e) * ('d * string) list) option ->
      'a * bool * string List.t * bool * bool
   Elle prend comme argument le nom de la fonction à tester, la fonction à
   utiliser pour comparer le résultat obtenu avec chaque jeu de test et le
   résultat attendu, un jeu de données (comme décrit au début du fichier), et
   éventuellement celui concernant les cas d'exceptions, et retourne un 5-uplet
   formé du nom de la fonction, du résultat obtenu pour le test en question,
   des commentaires éventuels issus du test, d'un booléen qui précise si la
   fonction testée est non implantée et d'un booléen qui précise si une
   exception a été soulevée durant le test de la fonction en question.
*)
(* ------------------------------------------------------------------------- *)

(* La fonction suivante permet de tester n'importe quelle autre fonction
   en prenant soin de limiter le temps d'exécution à un #sec fixé (par défaut,
   3 sec.; on peut évidemment ajuster cette valeur).
   (version fonctionnelle que sous Linux/Unix (donc, sous WSL et MacOS))

   À cause de l'utilisation du module Unix, il faut charger la librairie au
   préalable:
   - (en mode interpréteur) #require "unix";;
   - (en mode compilation) ... -package unix ...
   Avec l'outil «dune», il faut simplement mentionner le nom de cette librairie
   dans le fichier «dune» (voir celui qui est dans le présent dossier).
*)
exception Timeout

let call_with_timeout ?(time_in_seconds = 3) f =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
  try
    ignore (Unix.alarm time_in_seconds);
    let result = f () in
    ignore (Unix.alarm 0);
    result
  with exn ->
    ignore (Unix.alarm 0);
    raise exn

let testg (nom_f, f, jeu_donnees, jeu_donnees_exception_op) =
  let comment_l = ref [] in
  let ok = ref true in
  let excep = ref false in

  try
    List.iter
      (fun (p, res, cas_test) ->
        match call_with_timeout (fun () -> f p res) with
        | true -> ()
        | false ->
            ok := false;
            comment_l := !comment_l @ [ cas_test ^ " --> incorrect!" ]
        | exception Non_Implante s ->
            ok := false;
            raise (Non_Implante s)
        | exception e ->
            ok := false;
            excep := true;
            comment_l :=
              !comment_l
              @ [
                  cas_test ^ " - Exception soulevée: «" ^ Printexc.to_string e
                  ^ "»";
                ])
      jeu_donnees;
    (match jeu_donnees_exception_op with
    | None -> ()
    | Some (f', jeu_donnees_exception) ->
        List.iter
          (fun (p, cas_test) ->
            try
              ignore (call_with_timeout (fun () -> f' p));
              ok := false;
              comment_l :=
                !comment_l
                @ [ cas_test ^ " --> incorrect! Devrait soulever exception!" ]
            with
            | Failure _ -> ()
            | Timeout ->
                ok := false;
                excep := true;
                comment_l :=
                  !comment_l @ [ cas_test ^ " - Exception soulevée: «Timeout»" ]
            | e ->
                ok := false;
                excep := true;
                comment_l :=
                  !comment_l
                  @ [
                      cas_test
                      ^ " --> incorrect! Devrait soulever exception «Failure»"
                      ^ " et non «" ^ Printexc.to_string e ^ "»";
                    ])
          jeu_donnees_exception);
    (nom_f, !ok, !comment_l, false, !excep)
  with Non_Implante _ ->
    (nom_f, !ok, !comment_l @ [ "Fonction non implantée!" ], true, !excep)

(* -------------------------------------------------------------------------- *)
(* -- TESTE TOUT ------------------------------------------------------------
   La fonction test a comme signature:
     unit -> (string * bool * string list * bool * bool) list
   Elle effectue les n tests permettant de tester chacune des fonctions du Tp,
   et retourne une liste de 5-uplet:
   - nom de la fonction testée
   - un booléen qui précise le résultat du test pour cette fonction
   - les commentaires éventuels issus des tests
   - un booléen qui précise si la fonction est non implantée
   - un booléen qui précise si une exception a été soulevée
*)
(* -------------------------------------------------------------------------- *)
let tests () =
  [
    testg jeu_est_prerequis;
    testg jeu_simp_pre;
    testg jeu_seuls_cours_pgm_dans_pre;
    testg jeu_cours_pgm_par_type;
    testg jeu_cours_pgm;
    testg jeu_cours_contrib_dans_pgm;
    testg jeu_regroupe_cours_equiv;
  ]

(* -------------------------------------------------------------------------- *)
(* -- CORRIGE ---------------------------------------------------------------
   Le type de cette fonction est unit -> unit
   Elle appelle la fonction «tests», récupère les résultats des tests et
   affiche seulement le nom de chaque fonction testée, le résultat obtenu et
   les éventuels commentaires.
*)
(* -------------------------------------------------------------------------- *)
let corrige () =
  print_endline "Resultats:";
  print_endline "----------\n";
  List.iter
    (fun (nom_f, ok, comment, _, _) ->
      Printf.printf "%s : %s\n" nom_f (if ok then "OK" else "");
      List.iter (fun c -> print_endline ("\t" ^ c)) comment)
    (tests ())
;;

corrige ()

(* Avec version du corrigé:

    dune runtest

    Resultats:
    ----------

    est_prerequis : OK
    simp_pre : OK
    seuls_cours_pgm_dans_pre : OK
    cours_pgm_par_type : OK
    cours_pgm : OK
    cours_contrib_dans_pgm : OK
    regroupe_cours_equiv : OK

   Avec version remise et donc non complétée:

    dune runtest

    Resultats:
    ----------

    est_prerequis :
             Fonction non implantée!
    simp_pre :
             Fonction non implantée!
    seuls_cours_pgm_dans_pre :
             Fonction non implantée!
    cours_pgm_par_type :
             Fonction non implantée!
    cours_pgm :
             Fonction non implantée!
    cours_contrib_dans_pgm :
             Fonction non implantée!
    regroupe_cours_equiv :
             Fonction non implantée!
*)

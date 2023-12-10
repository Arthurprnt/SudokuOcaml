let grille = [|[|0;0;3; 6;0;0; 0;0;0|];
               [|0;7;0; 0;9;0; 2;0;0|];
               [|8;0;0; 0;0;0; 0;0;0|];

               [|0;5;0; 0;0;7; 0;0;0|];
               [|0;0;0; 0;4;5; 7;0;0|];
               [|0;0;0; 1;0;0; 0;3;0|];

               [|0;0;1; 0;0;0; 0;6;8|];
               [|0;0;8; 5;0;0; 0;1;0|];
               [|0;9;0; 0;0;0; 4;0;0|];|];;



let afficheGrille tab =
  for i = 0 to 8 do
    for j = 0 to 8 do
      if j mod 3 = 2 then Printf.printf "%d   " tab.(i).(j)
      else Printf.printf "%d-" tab.(i).(j);
  done;
  if i mod 3 = 2 then Printf.printf "\n\n"
  else Printf.printf "\n"
done; ();;



let valeurPossibleDansLigne v tab i = 
  let absente = ref true in
  for c = 0 to 8 do
    if tab.(i).(c) = v then absente := false
    done;
!absente;;



let valeurPossibleDansColonne v tab j =
  let absente = ref true in
  for l = 0 to 8 do
    if tab.(l).(j) = v then absente := false
    done;
!absente;;



let valeurPossibleDansCarre v tab i j =
  let icarre = 3*(i/3) in
  let jcarre = 3*(j/3) in
  let absente = ref true in
  for l = icarre to icarre+2 do
    for c = jcarre to jcarre+2 do
      if tab.(l).(c) = v then absente := false
      done;
    done;
!absente;;



let rec resoutGrille tab pos =
  if pos >= 81 then
    true
  else
    begin
      let i = pos/9 in
      let j = pos mod 9 in
      if tab.(i).(j) <> 0 then
        resoutGrille tab (pos+1)
      else
        begin
          let valeur = ref false in
          for n=1 to 9 do
            if valeurPossibleDansColonne n tab j &&
              valeurPossibleDansLigne n tab i &&
              valeurPossibleDansCarre n tab i j then
                begin
                  tab.(i).(j) <- n;
                  if not (resoutGrille tab (pos+1)) then tab.(i).(j) <- 0
                  else valeur := true
                end;
          done;
          !valeur
        end;
    end;;

afficheGrille grille;;

resoutGrille grille 0;;

Printf.printf "\n=====================\n\n\n";;
afficheGrille grille;;
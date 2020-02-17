let () =
  match [%type: bool * char] with
  | [%type: [%t? b] * [%t? c]] ->
      begin match b with
      | [%type: bool] -> ()
      | _ -> assert false
      end;
      begin match c with
      | [%type: char] -> ()
      | _ -> assert false
      end
  | _ -> assert false

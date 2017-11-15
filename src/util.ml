module StringSet_ = Set.Make(String)
module StringSet = struct
  include StringSet_
  let to_string set = Printf.sprintf "{%s}" (String.concat ", " (StringSet_.elements set))
end
module StringMap = Map.Make(String)

module List = struct
  include List
  let find x lst = 
    let rec func x lst c = match lst with
      | hd :: tl -> if hd = x then c else func x tl (c+1)
      | [] -> failwith "Not Found"
    in func x lst 0
end

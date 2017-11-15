let get_color i =
  let colors = [ 0xff0000 ; 0x0000ff ; 0x00ff00 ; 0xff00ff ; 0xffff00 ; 0x00ffff ] in
  List.nth colors (i mod (List.length colors))

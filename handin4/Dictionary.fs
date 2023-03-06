module Dictionary
    type Dictionary = D of Set<string>
    let empty () = D Set.empty<string>
    let insert (s: string) (D d) = D (d.Add s)
    let lookup (s: string) (D d) = Set.exists (fun x -> x = s) d
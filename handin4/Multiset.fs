module Multiset

    type Multiset<'a when 'a : comparison> = M of Map<'a, uint32>

    let empty = M Map.empty
    let isEmpty (M m) = Map.isEmpty m
    let size (M m) = Map.fold (fun s x y -> s+y) 0u m
    let contains x (M m) = Map.containsKey x m
    let numItems x (M m) = Map.tryFind x m |> Option.defaultValue 0u   
    let add x n (M m) =
        let num = numItems x (M m)
        M(Map.add x (n + num) m)
    let addSingle x (M m) = add x 1u (M m)
    let remove x n (M m) = 
        match Map.tryFind x m with
        | Some y -> 
            let newms = Map.remove x m
            if int(n) <= 0 || int((numItems x (M m)))-int(n) <= 0 then
                M newms
            else
                let added: Map<'a, uint32> = Map.add x (y-n) newms
                M added
        | None -> M m
    let removeSingle x (M m) = remove x 1u (M m)
 
    let fold f acc (M m) = Map.fold f acc m
    
    let foldBack f (M m) acc = Map.foldBack f m acc
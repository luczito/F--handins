module Multiset

    type Multiset<'a when 'a : comparison> = M of Map<'a, uint32>

    let empty = M Map.empty
    let isEmpty (M m) = Map.isEmpty m
    let size (M m) = uint32(Map.count m)
    let contains x (M m) = Map.containsKey x m
    let numItems x (M m) = Map.tryFind x m |> Option.defaultValue 0u   
    let add x n (M m) = M (Map.add x (numItems x (M m) + n) m)
    let addSingle x (M m) = M (Map.add x (numItems x (M m) + 1u) m)
    let remove x n (M m) = M (Map.add x (numItems x (M m) - n) m)
    let removeSingle x (M m) = M (Map.add x (numItems x (M m) - 1u) m)
    let fold (f acc) (M m) = 
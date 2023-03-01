module Multiset

    type Multiset<'a when 'a : comparison>

    val empty: Multiset<'a>
    
    val isEmpty: Multiset<'a> -> bool

    val size : Multiset<'a> -> uint32

    val contains : 'a -> Multiset<'a> -> bool

    val numItems : 'a -> Multiset<'a> -> uint32

    val add : 'a -> uint32 -> Multiset<'a> -> Multiset<'a>

    val addSingle : 'a -> Multiset<'a> -> Multiset<'a>

    val remove : 'a -> uint32 -> Multiset<'a> -> Multiset<'a>

    val removeSingle : 'a -> Multiset<'a> -> Multiset<'a>

    val fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> Multiset<'b> -> 'a

    val foldBack : ('a -> uint32 -> 'b -> 'b) -> Multiset<'a> -> 'b -> 'b 
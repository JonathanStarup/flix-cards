pub class EFunctor[m: Type -> Bool -> Type] {
    pub def map[a: Type, b: Type, ef1: Bool, ef2: Bool]
            (f: a -> b & ef1, x: m[a, ef2]): m[b, ef1 and ef2]
}

pub class EApplicative[m: Type -> Bool -> Type] with DFunctor[m] {

    pub def point(x: a): m[a, Pure]

    pub def ap[a: Type, b: Type, ef1: Bool, ef2: Bool, ef3: Bool]
            (f: m[a -> b & ef1, ef2], x: m[a, ef3]):
            m[b, ef1 and ef2 and ef3]

}

pub class EMonad[m: Type -> Bool -> Type] with DApplicative[m] {
    pub def flatMap[a: Type, b: Type, ef1: Bool, ef2: Bool, ef3: Bool]
            (f: a -> m[b, ef1] & ef2, x: m[a, ef3]):
            m[b, ef1 and ef2 and ef3]
}

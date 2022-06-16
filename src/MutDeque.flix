namespace MutDeque {
    pub def peekFront(m: MutDeque[a, r]): Option[a] & r =
        match popFront(m) {
            case None => None
            case Some(e) =>
                MutDeque.pushFront(e, m);
                Some(e)
        }
}

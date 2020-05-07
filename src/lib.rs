pub fn id<T>(x: T) -> T {
    x
}

pub fn o<'a, R, S, T>(f: &'a (dyn Fn(S) -> T), g: &'a (dyn Fn(R) -> S)) -> impl Fn(R) -> T + 'a {
    move |x| f(g(x))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn composable_identity() {
        fn dummy(x: usize) -> usize {
            x + 1
        }

        assert_eq!(dummy(1), o(&dummy, &id)(1));
        assert_eq!(dummy(1), o(&id, &dummy)(1));
        assert_eq!(o(&dummy, &id)(1), o(&id, &dummy)(1));
    }
}

pub fn id<T>(x: T) -> T {
    x
}

pub fn o<'a, R, S, T>(f: &'a (dyn Fn(S) -> T), g: &'a (dyn Fn(R) -> S)) -> impl Fn(R) -> T + 'a {
    move |x| f(g(x))
}

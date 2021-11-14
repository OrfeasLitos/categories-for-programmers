fn fmap<'a, A, B, R>(f: &'a dyn Fn(A) -> B, g: &'a dyn Fn(R) -> A) -> impl Fn(R) -> B + 'a {
   move |x: R| f(g(x))
}

#[test]
fn reader_id() {
    let f = |x: usize| x;
    let g = |x: &str| x.len();

    let f_id = fmap::<usize, usize, &str>(&f, &g);

    let s = "Hello, world!";
    for i in 0..s.len() {
        assert_eq!(f_id(&s[0..i]), g(&s[0..i]));
    }
}

#[test]
fn reader_composition() {
    let f = |x: usize| x as f64;
    let g = |x: &str| x.len();

    let f_comp = fmap::<usize, f64, &str>(&f, &g);

    let s = "Hello, world!";
    for i in 0..s.len() {
        assert_eq!(f_comp(&s[0..i]), f(g(&s[0..i])));
    }
}

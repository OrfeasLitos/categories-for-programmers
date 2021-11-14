#[derive(Clone, Debug, PartialEq)]
struct Pair<S, T> {
    a: S,
    b: T,
}

trait Bifunctor<A, B> {
    fn bimap<'a, C, D>(
        self,
        f: &'a (dyn Fn(A) -> C),
        g: &'a (dyn Fn(B) -> D)
    ) -> Pair::<C, D>;

    fn first<'a, C>(
        self,
        f: &'a (dyn Fn(A) -> C)
    ) -> Pair::<C, B>;

    fn second<'a, D>(
        self,
        g: &'a (dyn Fn(B) -> D)
    ) -> Pair::<A, D>;
}

impl<A, B> Bifunctor<A, B> for Pair::<A, B> {
    fn bimap<'a,  C, D>(
        self,
        f: &'a (dyn Fn(A) -> C),
        g: &'a (dyn Fn(B) -> D)
    ) -> Pair::<C, D> {
        Pair::<C, D> {
            a: f(self.a),
            b: g(self.b),
        }
    }

    fn first<'a, C>(
        self,
        f: &'a (dyn Fn(A) -> C)
    ) -> Pair::<C, B> {
        Pair::<C, B> {
            a: f(self.a),
            b: self.b,
        }
    }

    fn second<'a, D>(
        self,
        g: &'a (dyn Fn(B) -> D)
    ) -> Pair::<A, D> {
        Pair::<A, D> {
            a: self.a,
            b: g(self.b),
        }
    }
}

#[test]
fn bifunctoriality() {
    use crate::composition::id;

    let foo = Pair::<u32, f64> {
        a: 1,
        b: 2.,
    };
    let bar = foo.clone();
    let har = foo.clone();
    let baz = foo.clone();
    let doo = foo.clone();
    let fus = foo.clone();

    fn int_incr(x: u32) -> u32 {
        x + 1
    }

    fn float_incr(x: f64) -> f64 {
        x + 2.
    }

    assert_eq!(
        foo.bimap(&int_incr, &float_incr),
        bar.first(&int_incr).second(&float_incr)
    );

    assert_eq!(
        har.first(&int_incr),
        baz.bimap(&int_incr, &id::<f64>)
    );

    assert_eq!(
        doo.second(&float_incr),
        fus.bimap(&id::<u32>, &float_incr)
    );
}

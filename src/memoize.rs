use std::hash::Hash;
use std::collections::HashMap;

pub struct Memoized<'a, R: Eq + Hash, S> {
    f: &'a (dyn Fn(&R) -> S),
    map: HashMap<R, S>,
}

impl<'a, R: Eq + Hash, S: Clone> Memoized<'_, R, S>
    {
    pub fn new(f: &'static (dyn Fn(&R) -> S)) -> Self {
        Memoized {
            f,
            map: HashMap::new(),
        }
    }

    pub fn exe(&'a mut self, x: R) -> S {
        let map = &mut self.map;
        if let Some(y) = map.get(&x) {
            println!("memoized!");
            y.clone()
        } else {
            let y = (self.f)(&x);
            map.insert(x, y.clone());
            y
        }
    }
}

pub fn memoize<R: Eq + Hash, S: Clone>(f: &'static (dyn Fn(&R) -> S)) -> Box<(dyn FnMut(R) -> S)> {
    let mut map: HashMap<R, S> = HashMap::new();

    Box::new(
        move |x: R| {
            if let Some(y) = map.get(&x) {
                println!("memoized!");
                y.clone()
            } else {
                let y = f(&x);
                map.insert(x, y.clone());
                y
            }
        }
    )
}

// see https://users.rust-lang.org/t/memoize-function-without-cloning/42813/2 for memoize_ref()

#[test]
fn struct_memoize() {
    fn f(x: &isize) -> f64 {
        *x as f64 / 32.0
    }

    let mut mem_f = Memoized::new(&f);
    assert_eq!(mem_f.exe(42), f(&42));
    assert_eq!(mem_f.exe(42), mem_f.exe(42));
}

#[test]
fn func_memoize() {
    fn f(x: &isize) -> f64 {
        *x as f64 / 32.0
    }

    let mut mem_f = memoize(&f);
    assert_eq!(mem_f(42), f(&42));
    assert_eq!(mem_f(42), mem_f(42));
}

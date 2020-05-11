use self::{id, o};
use std::convert::TryInto;

fn main() {
    println!("{}, {}, {}, {}", id('a'), id(1), id(6.4), id(true));

    fn f(x: usize) -> f64 {
        x as f64 + 1.0
    }

    fn g(x: isize) -> usize {
        if x < 0 {
            (-x).try_into().unwrap()
        } else {
            x.try_into().unwrap()
        }
    }

    println!("{}, {}, {}", g(-1), f(g(-1)), o(&f, &g)(-1))
}

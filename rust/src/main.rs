mod composition;
use composition::id;

mod memoize;
use memoize::Memoized;

mod coproducts;
mod shapes_trait;
mod shapes_enum;
mod reader;
mod functoriality;

use std::convert::TryInto;

fn main() {
    println!("id: {}, {}, {}, {}", id('a'), id(1), id(6.4), id(true));

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

    println!(
        "composition: {}, {}, {}", g(-1), f(g(-1)), composition::o(&f, &g)(-1)
    );

    fn h(x: &usize) -> f64 {
        *x as f64 + 0.5
    }

    let mut mem_h = Memoized::new(&h);
    println!("memoize: {}, {}, {}", mem_h.exe(1), mem_h.exe(2), mem_h.exe(1));
}

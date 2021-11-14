#![allow(dead_code)]

// morphism from Either<i32, bool> to i32
#[derive(Debug, PartialEq)]
enum Either<S, T> {
    Left(S),
    Right(T),
}

fn i_prime<S, T>(x: S) -> Either<S, T> {
    Either::Left(x)
}

fn j_prime<S, T>(x: T) -> Either<S, T> {
    Either::Right(x)
}

fn m(x: Either<i32, bool>) -> i32 {
    match x {
        Either::Left(x) => x,
        Either::Right(x) => if x { 1 } else { 0 },
    }
}

fn i(n: i32) -> i32 {
    n
}

fn j(b: bool) -> i32 {
    if b { 1 } else { 0 }
}

#[test]
fn test_i() {
    assert_eq!(i(1), 1);
}

#[test]
fn test_j_false() {
    assert_eq!(j(false), 0);
}

#[test]
fn test_j_true() {
    assert_eq!(j(true), 1);
}

#[test]
fn i_is_m_dot_i_prime() {
    for n in -10..10 {
        assert_eq!(m(i_prime(n)), i(n));
    }
}

#[test]
fn j_is_m_dot_j_prime() {
    for b in [false, true].iter() {
        assert_eq!(m(j_prime(*b)), j(*b));
    }
}

// morphism from i32 to Either<i32, bool> (functions prefixed with inv_)

fn inv_i_prime(n: i32) -> i32 {
    if n < 0 { n } else { n + 2 }
}

fn inv_j_prime(b: bool) -> i32 {
    if b { 1 } else { 0 }
}

fn inv_i(n: i32) -> Either<i32, bool> {
    if n < 0 { Either::Left(n) } else { Either::Left(n + 2) }
}

fn inv_j(b: bool) -> Either<i32, bool> {
    if b { Either::Right(true) } else { Either::Right(false) }
}

fn inv_m(x: i32) -> Either<i32, bool> {
    if x < 0 || x >= 2 { Either::Left(x) }
    else if x == 0 { Either::Right(false) }
    else /*if x == 1*/ { Either::Right(true) }
}

#[test]
fn inv_i_is_inv_m_dot_inv_i_prime() {
    for n in -10..10 {
        assert_eq!(inv_m(inv_i_prime(n)), inv_i(n));
    }
}

#[test]
fn inv_j_is_inv_m_dot_inv_j_prime() {
    for b in [false, true].iter() {
        assert_eq!(inv_m(inv_j_prime(*b)), inv_j(*b));
    }
}

// answer to ex. 8: (i32, i32)
fn i_tuple(n: i32) -> (i32, i32) {
    (n, n + 2) // information duplication
}

fn j_tuple(b: bool) -> (i32, i32) {
    if b { (1, 1) } else { (0, 0) }
}

fn m_tuple_1(x: (i32, i32)) -> Either<i32, bool> {
    if x.0 == x.1 {
        if x.0 == 0 { Either::Right(false) }
        else { Either::Right(true) }
    } else {
        Either::Left(x.0) // extract info one way
    }
}

fn m_tuple_2(x: (i32, i32)) -> Either<i32, bool> {
    if x.0 == x.1 {
        if x.0 == 0 { Either::Right(false) }
        else { Either::Right(true) }
    } else {
        Either::Left(x.1 - 2) // extract info another way
    }
}

#[test]
fn m_tuple_1_and_2_both_work() {
    for n in -10..10 {
        assert_eq!(m_tuple_1(i_tuple(n)), i_prime(n));
        assert_eq!(m_tuple_2(i_tuple(n)), i_prime(n));
    }

    for b in [false, true].iter() {
        assert_eq!(m_tuple_1(j_tuple(*b)), j_prime(*b));
        assert_eq!(m_tuple_2(j_tuple(*b)), j_prime(*b));
    }
}

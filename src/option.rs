fn safe_root(x: f64) -> Option<f64> {
    if x >= 0.0 {
        Some(x.sqrt())
    } else {
        None
    }
}

fn safe_reciprocal(x: f64) -> Option<f64> {
    if x != 0.0 {
        Some(1.0/x)
    } else {
        None
    }
}

pub fn safe_root_reciprocal(x: f64) -> Option<f64> {
    let reciprocal = safe_reciprocal(x);
    if let Some(reciprocal) = reciprocal {
        safe_root(reciprocal)
    } else {
        None
    }
}

#[test]
fn root_of_4() {
    assert_eq!(Some(2.0), safe_root(4.0));
}

#[test]
fn root_of_neg() {
    assert_eq!(None, safe_root(-1.0));
}

#[test]
fn reciprocal_of_2() {
    assert_eq!(Some(0.5), safe_reciprocal(2.0));
}

#[test]
fn reciprocal_of_neg() {
    assert_eq!(Some(-2.0), safe_reciprocal(-0.5));
}

#[test]
fn reciprocal_of_zero() {
    assert_eq!(None, safe_reciprocal(0.0));
}

#[test]
fn composition_pos() {
    assert_eq!(Some(2.0), safe_root_reciprocal(0.25));
}

#[test]
fn composition_neg() {
    assert_eq!(None, safe_root_reciprocal(-0.5));
}

#[test]
fn composition_zero() {
    assert_eq!(None, safe_root_reciprocal(0.0));
}

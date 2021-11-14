use std::f64::consts::PI;

enum Shape {
    Circle { r: f64 },
    Rect { h: f64, w: f64 },
    Square { s: f64 },
}

impl Shape {
    fn area(&self) -> f64 {
        match self {
            Shape::Circle { r } => PI * r * r,
            Shape::Rect { h, w } => h * w,
            Shape::Square { s } => s * s,
        }
    }

    fn circ(&self) -> f64 {
        match self {
            Shape::Circle { r } => 2. * PI * r,
            Shape::Rect { h, w } => 2. * h + 2. * w,
            Shape::Square { s } => 4. * s,
        }
    }
}

#[test]
fn circle_area() {
    let c = Shape::Circle { r: 1. };
    assert!(c.area() <= 3.2 && c.area() >= 3.1);
}

#[test]
fn rect_area() {
    let r = Shape::Rect { h: 1., w: 2. };
    assert!(r.area() <= 2.1 && r.area() >= 1.9);
}

#[test]
fn circle_circumference() {
    let c = Shape::Circle { r: 1. };
    assert!(c.circ() <= 6.3 && c.circ() >= 6.2);
}

#[test]
fn rect_circumference() {
    let r = Shape::Rect { h: 1., w: 2. };
    assert!(r.circ() <= 6.1 && r.circ() >= 5.9);
}

#[test]
fn square_area() {
    let s = Shape::Square { s: 2. };
    assert!(s.area() <= 4.1 && s.area() >= 3.9);
}

#[test]
fn square_circumference() {
    let s = Shape::Square { s: 2. };
    assert!(s.circ() <= 8.1 && s.circ() >= 7.9);
}

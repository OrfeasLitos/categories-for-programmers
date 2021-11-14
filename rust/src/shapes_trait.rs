use std::f64::consts::PI;

trait Shape {
    fn area(&self) -> f64;
    fn circ(&self) -> f64;
}

struct Circle {
    r: f64,
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        PI * self.r * self.r
    }

    fn circ(&self) -> f64 {
        2. * PI * self.r
    }
}

struct Rect {
    h: f64,
    w: f64,
}

impl Shape for Rect {
    fn area(&self) -> f64 {
        self.h * self.w
    }

    fn circ(&self) -> f64 {
        2. * self.h + 2. * self.w
    }
}

struct Square {
    s: f64,
}

impl Shape for Square {
    fn area(&self) -> f64 {
        self.s * self.s
    }

    fn circ(&self) -> f64 {
        4. * self.s
    }
}

#[test]
fn circle_area() {
    let c = Circle { r: 1. };
    assert!(c.area() <= 3.2 && c.area() >= 3.1);
}

#[test]
fn rect_area() {
    let r = Rect { h: 1., w: 2. };
    assert!(r.area() <= 2.1 && r.area() >= 1.9);
}

#[test]
fn square_area() {
    let s = Square { s: 2. };
    assert!(s.area() <= 4.1 && s.area() >= 3.9);
}

#[test]
fn circle_circumference() {
    let c = Circle { r: 1. };
    assert!(c.circ() <= 6.3 && c.circ() >= 6.2);
}

#[test]
fn rect_circumference() {
    let r = Rect { h: 1., w: 2. };
    assert!(r.circ() <= 6.1 && r.circ() >= 5.9);
}

#[test]
fn square_circumference() {
    let s = Square { s: 2. };
    assert!(s.circ() <= 8.1 && s.circ() >= 7.9);
}

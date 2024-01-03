#![allow(dead_code)]
use crate::tag::*;

#[derive(Tag)]
pub enum WebEvent {
    OnLoad,
    Scroll(f64),
    Click { x: u64, y: u64 },
}

pub fn demo(event: WebEvent) {
    let n = match event.tag() {
        WebEventTag::OnLoad => 1,
        WebEventTag::Scroll => 2,
        WebEventTag::Click => 3,
    };
    println!("{}", n);

    //let args = ClickArgs { x: 3, y: 3 };

    //use crate::singletons::*;
    //let foo: Click;
}

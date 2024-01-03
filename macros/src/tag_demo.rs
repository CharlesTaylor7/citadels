use crate::tag::*;

#[derive(Tag, Clone)]
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
}

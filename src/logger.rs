use env_logger::{fmt::Color, Env};
use log::Level;
use std::io::Write;

pub fn init() {
    let env = Env::default().filter_or("LOG_LEVEL", "info");
    env_logger::Builder::from_env(env)
        .format(|buf, record| {
            let color = match record.level() {
                Level::Error => Color::Red,
                Level::Warn => Color::Yellow,
                Level::Info => Color::Cyan,
                Level::Debug => Color::Green,
                Level::Trace => Color::White,
            };

            let mut style = buf.style();
            style.set_color(color);

            writeln!(
                buf,
                "{: <30} {}",
                style.value(format!(
                    "[{} {}:{}]",
                    (record.level()),
                    record.file().unwrap_or("unknown"),
                    record.line().unwrap_or(0),
                )),
                record.args()
            )
        })
        .init();
}

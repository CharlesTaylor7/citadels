use env_logger::{fmt::Color, Env};
use log::*;
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
                "\n[{} {}:{}]\n{}\n",
                style.value(record.level()),
                record.file().unwrap_or("unknown"),
                record.line().unwrap_or(0),
                // chrono::Local::now().format("%Y-%m-%dT%H:%M:%S"),
                record.args()
            )
        })
        .init();
}

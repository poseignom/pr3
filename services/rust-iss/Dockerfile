# Используем официальный образ Rust
FROM rust:1.73-alpine

# Устанавливаем необходимые пакеты для сборки
RUN apk add --no-cache musl-dev openssl-dev pkgconfig

WORKDIR /app

# Копируем файлы проекта
COPY Cargo.toml .
COPY Cargo.lock .
COPY src ./src

# Собираем проект
RUN cargo build --release

# Запускаем приложение
EXPOSE 3000
CMD ["./target/release/rust_iss"]
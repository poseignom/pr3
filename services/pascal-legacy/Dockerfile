FROM alpine:3.18

# Установка Free Pascal и зависимостей
RUN apk add --no-cache \
    freepascal \
    postgresql-client \
    python3 \
    py3-pip \
    bash \
    curl \
    tzdata

# Установка Python библиотек для XLSX
RUN pip3 install pandas openpyxl xlsxwriter

# Создание рабочей директории
WORKDIR /app

# Копирование исходных файлов
COPY legacy.pas /app/
COPY run.sh /app/
COPY legacy_improved.pas /app/

# Компиляция Pascal программы
RUN fpc legacy_improved.pas -O2 -Xs -olegacy_generator

# Установка прав
RUN chmod +x /app/legacy_generator /app/run.sh

# Переменные окружения
ENV TZ=Europe/Moscow

# Точка входа
CMD ["/app/run.sh"]
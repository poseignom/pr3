FROM frolvlad/alpine-pascal:latest

# Установка зависимостей
RUN apk add --no-cache \
    python3 \
    py3-pip \
    bash \
    curl \
    tzdata

# Установка Python библиотек
RUN pip3 install pandas openpyxl xlsxwriter

# Создание структуры каталогов
RUN mkdir -p /app /data/csv /data/xlsx /data/logs

WORKDIR /app

# Копирование исходных файлов
COPY legacy_improved.pas /app/
COPY run_improved.sh /app/

# Компиляция программы
RUN fpc legacy_improved.pas -O2 -Xs -olegacy_generator

# Установка прав
RUN chmod +x /app/legacy_generator /app/run_improved.sh

# Настройка времени
ENV TZ=Europe/Moscow

# Переменные окружения
ENV GEN_PERIOD_SEC=300
ENV CSV_OUT_DIR=/data/csv
ENV LOG_DIR=/data/logs

# Точка входа
CMD ["/app/run_improved.sh"]